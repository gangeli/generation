package induction

import scala.collection.mutable.{HashMap,ArrayBuffer,HashSet}
import java.util.Random
import java.io._

import fig.basic.{IOUtils,Indexer,StatFig}
import fig.exec.Execution
import fig.exec.Execution.putLogRec
import fig.record.Record

import tea._
import tea.Utils.{map,foreach,fmt,fmt1,fmt2,fmts,returnFirst,assertValid}
import tea.Utils.{track,begin_track,end_track,logs,warnings,dbg,dbgs,fail,fails,unknownCase,impossible}

import InductionUtils._

import org.goobs.foreign.Counter

/**
 * A model of events and their text summaries (ACL 2009).
 * Model:
 *  - Select events to talk about
 *  - Generate text of those events
 * An event type has a fixed set of fields.
 * Each field has a type (numeric, categorical, symbol, string).
 * Each event has a fixed event type and values for each of the fields.
 * The model is essentially a hierachical labeled segmentation process.
 *
 * Change log:
 *  - 02/08/09: make field type explicit
 *  - 02/08/09: make the non-field type a possible field
 *  - 02/10/09: geometric distribution on numeric noise
 *  - 02/11/09: multiple trueEvents
 *  - 02/13/09: redo event model
 *  - 02/16/09: add tracks
 *  - 02/17/09: add word roles
 *  - 02/18/09: add labels, field set
 */
case class Event3Problem(opts:Options) extends WordProblem {

  ////////////////////////////////////////////////////////////
  // Generic routines

  def newIntArray(n:Int, x:Int) = Utils.set_!(new Array[Int](n), x) // Helper

  def setContains(set:Int, i:Int) = (set & (1 << i)) != 0
  def setContainsSet(set:Int, subset:Int) = (set & subset) == subset
  def setAdd(set:Int, i:Int) = set | (1 << i)
  def setSize(_set:Int) = {
    var set = _set
    var count = 0
    while (set != 0) { if ((set & 1) != 0) count += 1; set >>= 1; }
    count 
  }
  def setstr(n:Int, set:Int) = map(n, { i:Int => if (setContains(set, i)) "*" else "." }).mkString("")

  val NaN = Integer.MAX_VALUE
  // Fast conversion of string into integer (return NaN if fail)
  def str2num(s:String) : Int = {
    val n = s.length
    var i = 0
    val sign = if (i < n && s(i) == '-') { i += 1; -1 } else 1
    if (i == n) return NaN
    var x = 0
    while (i < n) {
      val d = s(i) - '0'
      if (d >= 0 && d <= 9)
        x = x * 10 + d
      else
        return NaN
      i += 1
    }
    return sign*x
  }
  def str2numOrFail(s:String) = {
    val x = str2num(s)
    if (x == NaN) throw fail("Not a number: "+s)
    x
  }

  // Rounding schemes for generating numbers
  val roundSpacing = 5
  def roundUp(x:Int) = (x + roundSpacing-1) / roundSpacing * roundSpacing
  def roundDown(x:Int) = x / roundSpacing * roundSpacing
  def roundClose(x:Int) = (x + roundSpacing/2) / roundSpacing * roundSpacing

  def processWord(word:String) = {
    if (opts.stemAll) Stemmer.stem(word)
    else word
  }
  def getWordIndex(str:String) = wordIndexer.getIndex(processWord(str))

  ////////////////////////////////////////////////////////////
  // Fields

  trait Field {
    def name : String
    def V : Int // Number of possible values
    def vstr(v:Int) : String
    def parseValue(role:Int, str:String) : Int // Inverse of vstr
    def newParams(prefix:String) : FieldParams
    def maxLength : Int
  }
  trait FieldParams extends AParams {
    // Provide potentials for salience
    def getFilter(ex:Example, e:Int, f:Int) = 1.0
    def updateFilter(ex:Example, e:Int, f:Int, prob:Double) = { }
    def emissionDist(value:Int) : ProbVec
	def generationDist(value:Int) : Counter[String] = {
		val pv:ProbVec = emissionDist(value)
		val rtn:Counter[String] = new Counter[String]
		foreach(W, (w:Int) => {
			val c = pv.getCount(w)
			if(c > 0) rtn.setCount(wstr(w), c)
		})
		rtn.normalize
		rtn
	}
  }
  class NullParams extends FieldParams {
    def foreachVec(f:(ProbVec => Any)) = { }
    def output(puts:(String => Any)) = { }
    def emissionDist(value:Int) = ProbVec.zeros(W)
  }

  // Numeric (#) - only integers allowed; words represent noisy versions of the value
  // Example: temperature in the weather domain
  case class NumField(name:String) extends Field {
    var min = Integer.MAX_VALUE
    var max = Integer.MIN_VALUE
    def V = max - min + 1 // Range (inclusive)
    def vstr(v:Int) = v.toString
    def parseValue(role:Int, str:String) = {
      val x = str2numOrFail(str)
      if (x < min) min = x
      if (x > max) max = x
      x
    }
    override def toString = fmts("#%s(%s..%s)", name, min, max)
    def newParams(prefix:String) = new NumFieldParams(prefix)
    val maxLength = 1
  }
  class NumFieldParams(prefix:String) extends FieldParams with Serializable{
	val serialVersionUID:Long = 001L;

    val methodChoices = ProbVec.zeros(M) // m -> use method m to generate numbers
    val leftNoiseChoices = ProbVec.zeros(S) // s -> generate noise s
    val rightNoiseChoices = ProbVec.zeros(S) // s -> generate noise s
    val filters = ProbVec.zeros2(H, B) // h, b -> If the field value is in histogram bin h, should we talk about this field?
    def foreachVec(f:(ProbVec => Any)) = {
      f(methodChoices)
      f(leftNoiseChoices)
      f(rightNoiseChoices)
      filters.foreach(f(_))
    }
    def output(puts:(String => Any)) = {
      foreachProb(methodChoices, puts, { m:Int => "numMethodC " + prefix + " " + mstr(m) })
      foreachProb(leftNoiseChoices, puts, { s:Int => "numLNoiseC " + prefix + " " + sstr(s) })
      foreachProb(rightNoiseChoices, puts, { s:Int => "numRNoiseC " + prefix + " " + sstr(s) })
      foreach(H, { h:Int =>
        foreachProb(filters(h), puts, { b:Int => "numFilter " + prefix + " " + hstr(h) + " " + bstr(b) })
      })
    }
    def emissionDist(v:Int) = {
		val value = v
		val rtn = ProbVec.zeros(W)
		if( value == NaN ) rtn
		else { 
			foreach(W, (word:Int) => {
				val num = str2num( wstr(word) )
				if(num == NaN) rtn.setCount_!(word, 0.0)
				else{
					//--This emission is valid
					var sumProb:Double = 0.0
					//(gen identity)
					if( num == value ) sumProb += methodChoices.getProb(M_IDENTITY) 
					//(rounding)
					if( num == roundUp(value) ) sumProb += methodChoices.getProb(M_ROUNDUP)
					if( num == roundDown(value) ) sumProb += methodChoices.getProb(M_ROUNDDOWN)
					if( num == roundClose(value) ) sumProb += methodChoices.getProb(M_ROUNDCLOSE)
					//(noise)
					val dif = if(value < num) num-value else value-num
					val a = if(value < num) rightNoiseChoices.getProb(S_CONTINUE)
						else leftNoiseChoices.getProb(S_CONTINUE)
					sumProb += (1.0-a) * Math.pow(a, dif)
					rtn.setCount_!(word, sumProb)
				}
			})
			rtn
		}
	}

	override def generationDist(v:Int):Counter[String] = {
		val value = v
		if( value == NaN ) throw new java.lang.IllegalArgumentException("Cannot generate from NaN")
		val min = value - 2*roundDown(value)
		val max = value + 2*roundUp(value)
		val rtn:Counter[String] = new Counter[String]
		foreach(min, max+1, (num:Int) => { //max inclusive
			var sumProb:Double = 0.0
			//(gen identity)
			if( num == value ) sumProb += methodChoices.getProb(M_IDENTITY) 
			//(rounding)
			if( num == roundUp(value) ) sumProb += methodChoices.getProb(M_ROUNDUP)
			if( num == roundDown(value) ) sumProb += methodChoices.getProb(M_ROUNDDOWN)
			if( num == roundClose(value) ) sumProb += methodChoices.getProb(M_ROUNDCLOSE)
			//(noise)
			val dif = if(value < num) num-value else value-num
			val a = if(value < num) rightNoiseChoices.getProb(S_CONTINUE)
				else leftNoiseChoices.getProb(S_CONTINUE)
			sumProb += (1.0-a) * Math.pow(a, dif)
			if(sumProb > 0) rtn.setCount("" + num, sumProb)
		})
		rtn.normalize
		rtn
	}
  }

  // Category (@) - we have to learn the possibly ambiguous correspondance between category values and words
  // Example: windDir in the weather domain
  // indexer = possible values (e.g., N, W, ...)
  case class CatField(name:String) extends Field {
    val indexer = new Indexer[String]
    def V = indexer.size
    def vstr(v:Int) = indexer.getObject(v)
    def parseValue(role:Int, str:String) = indexer.getIndex(str)
    override def toString = fmts("@%s(%s)", name, V)
    def newParams(prefix:String) = new CatFieldParams(prefix, this)
    val maxLength = Integer.MAX_VALUE
  }
  class CatFieldParams(prefix:String, field:CatField) extends FieldParams with Serializable{
	val serialVersionUID:Long = 001L
    val emissions = ProbVec.zeros2(field.V, W) // v, w -> express value v with word w
    val filters = ProbVec.zeros2(field.V, B) // v, b -> whether this value is notable
    def foreachVec(f:(ProbVec => Any)) = {
      emissions.foreach(f(_))
      filters.foreach(f(_))
    }
    def output(puts:(String => Any)) = {
      foreach(field.V, { v:Int =>
        foreachProb(emissions(v), puts, { w:Int => "catE " + prefix + " " + field.vstr(v) + " " + wstr(w) })
      })
      foreach(field.V, { v:Int =>
        foreachProb(filters(v), puts, { b:Int => "catFilter " + prefix + " " + field.vstr(v) + " " + bstr(b) })
      })
    }
    def emissionDist(value:Int) = emissions(value)
	override def generationDist(v:Int):Counter[String] = {
		if(v < emissions.size) { super.generationDist(v) }
		else{
			val rtn = new Counter[String]
			rtn.setCount("[[UNK]]", 1.0)
			rtn
		}
	}
  }

  // Symbol (:) - symbol values must match words verbatim (can use symbols to encode numbers where we expect no noise)
  // This is a special case of a string
  // Example: entity, relPoints in the NFL domain
  case class SymField(name:String) extends Field {
    def V = W
    def vstr(v:Int) = wordIndexer.getObject(v)
    def parseValue(role:Int, str:String) = getWordIndex(str)
    override def toString = fmts(":%s", name)
    def newParams(prefix:String) = new SymFieldParams(prefix)
    val maxLength = 1
  }
  class SymFieldParams(prefix:String) extends FieldParams with Serializable {
    val serialVersionUID:Long = 001L
    val labelChoices = ProbVec.zeros(LB) // lb -> probability of producing labels
    def foreachVec(f:(ProbVec => Any)) = {
      f(labelChoices)
    }
    def output(puts:(String => Any)) = {
      foreachProb(labelChoices, puts, { lb:Int => "labelC " + prefix + " " + lbstr(lb) })
    }
    def emissionDist(value:Int) = ProbVec.zeros(W)
  }

  // String ($) - sequence of words
  // Example: description in the NFL domain
  // indexer = possible sequences of words
  case class StrField(name:String) extends Field {
    val indexer = new Indexer[(Array[Int],Array[Int])]
    def V = indexer.size // Number of values
    def vstr(v:Int) = {
      val (words, labels) = indexer.getObject(v)
      map(Utils.same(words.length, labels.length), { i:Int =>
        wstr(words(i)) +
        (if (labels(i) == none_lb) "" else "/"+lbstr(labels(i)))
      }).mkString(" ")
    }
    def parseValue(role:Int, str:String) = {
      val tokens = str.split(" ")
      val words = new ArrayBuffer[Int]
      val labels = new ArrayBuffer[Int]
      foreach(tokens, { (i:Int,s:String) =>
        if (role == -1 || role == getWordRole(s)) {
          words += getWordIndex(s)
          labels += getLabelIndex(tokens, i)
        }
      })
      indexer.getIndex((words.toArray, labels.toArray))
    }
    override def toString = fmts("$%s(%s)", name, V)
    def newParams(prefix:String) = new StrFieldParams(prefix)
    val maxLength = Integer.MAX_VALUE
  }
  class StrFieldParams(prefix:String) extends FieldParams with Serializable {
	val serialVersionUID:Long = 001L
    val labelChoices = ProbVec.zeros2(LB, LB) // lb1, lb2 -> probability of transforming label lb1 to lb2
    def foreachVec(f:(ProbVec => Any)) = {
      labelChoices.foreach(f(_))
    }
    def output(puts:(String => Any)) = {
      foreach(LB, { lb1:Int =>
        foreachProb(labelChoices(lb1), puts, { lb2:Int => "labelC " + prefix + " " + lbstr(lb1) + " " + lbstr(lb2) })
      })
    }
    def emissionDist(value:Int) = ProbVec.zeros(W)
  }

  ////////////////////////////////////////////////////////////
  // Event and event types

  case class EventType(t:Int, name:String, fields:Array[Field]) {
    val F = fields.size
    val none_f = F
    val boundary_f = F+1
    val useFieldSets = opts.useFieldSetsOnEventTypes.contains("ALL") || opts.useFieldSetsOnEventTypes.contains(name)
    def fstr(f:Int) = {
      if (f == none_f) "(none)"
      else if (f == boundary_f) "(boundary)"
      else fields(f).name
    }
    override def toString = name
  }

  // An event fills in the values for the event type.
  // It is represented by an array:
  //   str: field f -> value at field f (an index into the field's indexer)
  //   num: numeric representation of the fields
  case class Event(t:Int, values:Array[Int]) {
    def fields = eventTypes(t).fields
    def F = fields.size
    def fstr = eventTypes(t).fstr _
    override def toString = {
      tstr(t)+":"+
      map(F, { f:Int => fstr(f)+"="+fields(f).vstr(values(f)) }).mkString(",")
    }
  }

  val none_e = -1 // Specify no event (blather)
  val unreachable_e = -2 // Sometimes the true event is not in our set, so we automatically get it wrong
  def realEvent_?(e:Int) = e != none_e && e != unreachable_e

  def none_t = T
  def T = eventTypes.size
  def tstr(t:Int) = if (t == none_t) "(none)" else eventTypes(t).name
  var eventTypes : Array[EventType] = null // Filled in later
  val eventTypeNameIndexer = new Indexer[String] // Temporary
  val eventTypesBuffer = new ArrayBuffer[EventType] // Temporary

  // Stuff for tracks
  val C = opts.eventTypeTracks.length
  val PC = 1 << C // Number of choices for the top level
  val wildcard_pc = -1
  def cstr(c:Int) = opts.eventTypeTracks(c)
  def pcstr(pc:Int) = setstr(C, pc)
  var eventTypeAllowedOnTrack : Array[HashSet[Int]] = null

  // Stuff for labels (each word has a label)
  def LB = labelIndexer.size
  val labelIndexer = new Indexer[String]
  val none_lb = labelIndexer.getIndex("-")
  val misc_lb = labelIndexer.getIndex("*")
  def lbstr(lb:Int) = labelIndexer.getObject(lb)
  def isAlpha(s:String) = Utils.forall(s.size, { i:Int => s(i).isLetter })
  def getLabelIndex(textStr:Array[String], i:Int) = {
    // For now, let the label of a word be the word after it if it's a number
    if (str2num(textStr(i)) == NaN) none_lb
    else if (i+1 == textStr.size || !isAlpha(textStr(i+1))) misc_lb
    else labelIndexer.getIndex(textStr(i+1))
  }

  // Word roles
  def useWordRoles = opts.useWordRolesOnFields.size > 0
  val wordRoleNameIndexer = new Indexer[String]
  val wordRoleMap = new HashMap[String,Int]
  var other_role = -1
  def numWordRoles = wordRoleNameIndexer.size
  def getWordRole(str:String) = wordRoleMap.getOrElse(str, other_role)
  def readWordRoles = track("readWordRoles: %s", opts.wordRolesPath) {
    other_role = wordRoleNameIndexer.getIndex("other")
    Utils.foreachLine(opts.wordRolesPath, { line:String =>
      val Array(word,role) = line.split(" ")
      wordRoleMap(word.toLowerCase) = wordRoleNameIndexer.getIndex(role)
      true
    })
    logs("%s words, %s roles", wordRoleMap.size, numWordRoles)
  }

  ////////////////////////////////////////////////////////////
  // Parameters

  // How to generate a word
  val G_FIELD_NAME = 0 // Talk about the field name (for field temperature.max, use word "high")
  val G_FIELD_VALUE = 1 // Talk about the field value (for field temperature.max, use word "24")
  val G_FIELD_GENERIC = 2 // Back off to generic words - shared globally (hopefully this becomes function words)
  val G = 3
  val gstr = Array("name", "value", "generic")
  val short_gstr = Array("n", "v", "g")

  // Methods of generate a number (for NumField)
  val M_IDENTITY = 0
  val M_NOISEUP = 1
  val M_NOISEDOWN = 2
  val M_ROUNDUP = 3
  val M_ROUNDDOWN = 4
  val M_ROUNDCLOSE = 5
  val M = 6
  val mstr = Array("identity", "noiseup", "noisedown", "roundup", "rounddown", "roundclose")
  val short_mstr = Array("1", "*", "*", ">", "<", "~")

  // Numeric noise: model as geometric distribution
  val S_CONTINUE = 0
  val S_STOP = 1
  val S = 2
  val sstr = Array("continue", "stop")

  // Histogram bins over numeric values
  val H_MIN = 0
  val H_MAX = 1
  val H_OTHER = 2
  val H = 3
  val hstr = Array("min", "max", "other")

  // Booleans
  val B_FALSE = 0
  val B_TRUE = 1
  val B = 2
  val bstr = Array("false", "true")

  case class EventTypeParams(t:Int) extends AParams with Serializable{
    import ProbVec.{zeros,zeros2,zeros3}
	val serialVersionUID:Long = 001L
    val eventType = eventTypes(t)
    val F = eventType.F
    val none_f = eventType.none_f
    val boundary_f = eventType.boundary_f
    val fields = eventType.fields
    val tstr = eventType.name
    val fstr = map(F+2, { f:Int => eventType.fstr(f) })

    // Field sets
    val FS = 1 << F // Number of sets of field types
    val fsstr = map(FS, { fs:Int => setstr(F, fs) })
    // fs is a binary mask of what fields can be present (1) or not (0)
    // efs is a binary mask of whether each field whether presence and absence is allowed (1) or not (0)
    val F2 = F+F // Maximum number of elements in the efs set
    val EFS_ABSENT = 1 // 01
    val EFS_PRESENT = 2 // 10
    val EFS_DONTCARE = 3 // 11
    val onlyabsent_efs = {
      var efs = 0
      foreach(F, { f:Int => efs = efs_addAbsent(efs, f) })
      efs
    }
    val allowed_fs = Utils.createArray({ add:(Int => Any) =>
      foreach(FS, { fs:Int =>
        val size = setSize(fs)
        if (opts.minFieldSetSize <= size && size <= opts.maxFieldSetSize) add(fs) 
      })
    })
    val dontcare_efs = (1 << (F2))-1 // All 11s
    def efsstr(efs:Int) = setstr(F2, efs)
    def efs_canBeAbesent(efs:Int, f:Int) = setContains(efs, f+f)
    def efs_canBePresent(efs:Int, f:Int) = setContains(efs, f+f+1)
    def efs_canBeEmpty(efs:Int) = setContainsSet(efs, onlyabsent_efs)
    def efs_addAbsent(efs:Int, f:Int) = setAdd(efs, f+f)
    def efs_addPresent(efs:Int, f:Int) = setAdd(efs, f+f+1)
    def fs2efs(fs:Int) = {
      var efs = 0
      foreach(F, { f:Int =>
        efs = if (setContains(fs, f)) efs_addPresent(efs, f) else efs_addAbsent(efs, f)
      })
      efs
    }

    val fieldChoices = zeros2(F+2, F+2) // f0, f -> choose field f given previous field f_0 (in event type t)
    val fieldSetChoices = zeros(FS) // Distribution over field sets
    val noneFieldEmissions = zeros(W) // w -> directly use word w (for none_f)
    val genChoices = zeros2(F, G) // f, g -> how to generate (g) a word in event f
    val fieldNameEmissions = zeros2(F, W) // f, w -> express field f with word w (G_FIELD_NAME)
    val fieldParams = map(F, { f:Int => fields(f).newParams(tstr + " " + fstr(f)) })
    val filters = zeros(B) // whether this type should be generated or not

    def foreachVec(f:(ProbVec => Any)) = {
      fieldChoices.foreach(f(_))
      f(fieldSetChoices)
      f(noneFieldEmissions)
      genChoices.foreach(f(_))
      fieldNameEmissions.foreach(f(_))
      fieldParams.foreach { a => a.foreachVec(f) }
      f(filters)
    }

    def output(puts:(String => Any)) = {
      foreach(F+2, { f0:Int => // fieldChoices
        foreachProb(fieldChoices(f0), puts, { f:Int =>
          "fieldC " + tstr + " " + fstr(f0) + " " + fstr(f)
        })
      })
      foreachProb(fieldSetChoices, puts, { fs:Int => // noneFieldEmissions
        "fieldSetC " + tstr + " " + fsstr(fs)
      })

      foreachProb(noneFieldEmissions, puts, { w:Int => // noneFieldEmissions
        "noneFieldE " + tstr + " " + wstr(w)
      })

      foreach(F, { f:Int =>
        foreachProb(genChoices(f), puts, { g:Int => // genChoices
          "genC " + tstr + " " + fstr(f) + " " + gstr(g)
        })
        foreachProb(fieldNameEmissions(f), puts, { w:Int => // fieldNameEmissions
          "fieldNameE " + tstr + " " + fstr(f) + " " + wstr(w)
        })
        fieldParams(f).output(puts)
        puts("")
      })
      foreachProb(filters, puts, { b:Int => "filter " + tstr + " " + bstr(b) })
    }
  }

  case class TrackParams(c:Int) extends AParams with Serializable{
	val serialVersionUID:Long = 001L
    val eventTypeChoices = ProbVec.zeros2(T+1, T+1) // t_0, t -> choose event of type t given we were in type t_0
    val noneEventTypeEmissions = ProbVec.zeros(W) // w -> generate word w

    def foreachVec(f:(ProbVec => Any)) = {
      eventTypeChoices.foreach(f(_))
      f(noneEventTypeEmissions)
    }

    def output(puts:(String => Any)) = {
      foreach(T+1, { t0:Int => // eventTypeChoices
        foreachProb(eventTypeChoices(t0), puts, { t:Int =>
          "eventTypeC [" + cstr(c) + "] " + tstr(t0) + " " + tstr(t)
        })
      })
      puts("")
      foreachProb(noneEventTypeEmissions, puts, { w:Int => // noneEventTypeEmissions
        "noneEventTypeE [" + cstr(c) + "] " + wstr(w)
      })
    }
  }

	
	class LengthParams extends AParams with Serializable {
		val serialVersionUID:Long = 001L
		import ProbVec.{zeros,zeros2,zeros3}
		val eventContinue = zeros(S)
		val fieldContinue = zeros2(T+1,S)
		val wordContinue = map(T, (t:Int) => zeros2(eventTypes(t).F+2, S))
		val noEventWordContinue = zeros(S)

		def foreachVec(f:(ProbVec => Any)) = {
			f(eventContinue)
			fieldContinue.foreach(f(_))
			foreach(T, (t:Int) => {
				wordContinue(t).foreach(f(_))
			})
			f(noEventWordContinue)
		}

		def output(puts:(String=>Any)) = {
			//--Geometric distribution params
			foreachProb(eventContinue, puts, (s:Int) => {
				"continue_event " + sstr(s)
			})
			puts("")
			foreach(T, (t:Int) => {
				foreachProb(fieldContinue(t), puts, (s:Int) => {
					"continue_field " + tstr(t) + " " + sstr(s)
				})
			})
			puts("")
			foreach(T, (t:Int) => {
				foreach(eventTypes(t).F, (f:Int) => {
					foreachProb(wordContinue(t)(f), puts, (s:Int) => {
						"continue_word " + tstr(t) + " " + eventTypes(t).fstr(f) + " " + sstr(s)
					})
				})
			})
			puts("")
			foreachProb(noEventWordContinue, puts, (s:Int) => {
				"continue_noevent_word " + sstr(s)
			})
		}
	}

  class Params extends AParams with Serializable {
    import ProbVec.{zeros,zeros2,zeros3}
    val serialVersionUID:Long = 001L

    val trackChoices = zeros(PC)
    val trackParams = map(C, { c:Int => new TrackParams(c) })
    val genericEmissions = zeros(W) // ideally for function words
    val genericLabelChoices = zeros(LB) // Generate labels
    val eventTypeChoicesGivenWord = zeros2(W, T+1) // w, t -> probability of generating an event type given word (not useful in practice)
    val eventTypeParams = map(T, { t:Int => new EventTypeParams(t) }) // t -> generate words for event type t
	val lengthParams = new LengthParams

    def foreachVec(f:(ProbVec => Any)) = {
      f(trackChoices)
      trackParams.foreach(_.foreachVec(f))
      f(genericEmissions)
      f(genericLabelChoices)
      eventTypeChoicesGivenWord.foreach(f(_))
      eventTypeParams.foreach(_.foreachVec(f))
	  lengthParams.foreachVec(f)
    }

    def output(puts:(String => Any)) = {
	  foreachProb(trackChoices, puts, { pc:Int =>
        "trackC " + pcstr(pc)
      })
      foreach(C, { c:Int =>
        trackParams(c).output(puts)
        puts("")
      })
      foreachProb(genericEmissions, puts, { w:Int =>
        "genericE " + wstr(w)
      })
      foreachProb(genericLabelChoices, puts, { lb:Int =>
        "genericLabelC " + lbstr(lb)
      })
      if (opts.includeEventTypeGivenWord) {
        foreach(W, { w:Int =>
          foreachProb(eventTypeChoicesGivenWord(w), puts, { t:Int =>
            "eventTypeChoice|w " + wstr(w) + " " + tstr(t)
          })
        })
      }
      puts("")
	  //(event params)
      foreach(T, { t:Int =>
        eventTypeParams(t).output(puts)
        puts("")
      })
	  //(length params)
	  lengthParams.output(puts)
    }

    override def optimize_!(smoothing:Double) = {
      // Apply targeted smoothing/discounting to individual parameters
      foreach(T+1, { t:Int =>
        foreach(C, { c:Int =>
          trackParams(c).eventTypeChoices(t).addCount_!(none_t, opts.noneEventTypeSmoothing) // Select the none event more often

          if (!opts.fixedNoneEventTypeProb.isNaN)
            trackParams(c).eventTypeChoices(t).setCountToObtainProb_!(none_t, opts.fixedNoneEventTypeProb)
        })

        if (t != none_t) {
          foreach(eventTypes(t).F+1, { f:Int =>
            eventTypeParams(t).fieldChoices(f).addCount_!(eventTypeParams(t).none_f, opts.noneFieldSmoothing) // Select no field more often

            if (f != eventTypeParams(t).none_f) {
              eventTypeParams(t).genChoices(f).addCount_!(G_FIELD_NAME, opts.fieldNameSmoothing) // Select name more than value
              if (!opts.fixedGenericProb.isNaN) // Fix the generic probability
                eventTypeParams(t).genChoices(f).setCountToObtainProb_!(G_FIELD_GENERIC, opts.fixedGenericProb)

              if (opts.discountCatEmissions) {
                eventTypeParams(t).fieldParams(f) match {
                  case fparams:CatFieldParams => fparams.emissions.foreach(_.addCount_!(-smoothing))
                  case _ =>
                }
              }
            }
          })
        }
      })

      super.optimize_!(smoothing)
    }
  }


  ////////////////////////////////////////////////////////////
  // Examples and evaluation

  // A widget is a segmentation of text into events
  case class Widget(events:Array[Array[Int]], fields:Array[Array[Int]], gens:Array[Array[Int]], numMethods:Array[Array[Int]],
      startIndices:Array[Int], e2t:(Int=>Int)) { // These are auxiliary information needed for evaluation
    var performance = "" // HACK: store on the trueWidget how we did

    // Tried to see if can use posteriors (of one track), but they're too sharply peaked to be useful
    var eventPosterior : Array[Double] = null
    def setEventPosterior(e:Int, E:Int, prob:Double) = {
      if (eventPosterior == null) eventPosterior = new Array[Double](E+1)
      eventPosterior(if (e == none_e) E else e) = prob
    }
    def eventPosteriorStr(events:Array[Event]) = {
      Utils.createArray({ add:(String => Any) =>
        Utils.foreachSorted(eventPosterior, 10, true, { (e:Int,prob:Double) =>
          if (prob > 0.01)
            add("POST " + (if (e == events.size) "(none)" else events(e)) + ":" + fmt(prob))
        })
      }).mkString("\t")
    }

    def foreachEvent(i:Int, f:(Int => Any)) = {
      foreach(events.size, { c:Int => f(events(c)(i)) })
    }

    // Warning: this function is not optimized
    // Does event query_e exist at each position in i...j
    def hasContiguousEvents(i:Int, j:Int, query_e:Int) = {
      Utils.forall(i, j, { k:Int =>
        var exists_? = false
        foreachEvent(k, { e:Int => if (e == query_e) exists_? = true })
        exists_?
      })
    }
    // Return true if there no event that could go in track c between i...j
    def hasNoReachableContiguousEvents(i:Int, j:Int, c:Int) = {
      Utils.forall(i, j, { k:Int =>
        var exists_? = false
        foreachEvent(k, { e:Int => if (realEvent_?(e) && eventTypeAllowedOnTrack(c)(e2t(e))) exists_? = true })
        !exists_?
      })
    }
  }

  // Compute number of alignments we get right
  // Only be sensitive to precision/recall of set of alignments from
  // one line of the input to the events 
  class Performance extends APerformance[Widget] {
    val result = new fig.basic.EvalResult // Precision/recall on events
    val counts = new Array[Array[Int]](T+1, T+1) // Confusion matrix on event types
    val correctCounts = new Array[Int](T) // For each event type, number of correct (some of counts(t)(t) could be wrong)

    def add(trueWidget:Widget, predWidget:Widget) : Unit = {
      if(trueWidget != null) {
        val startIndices = Utils.same(trueWidget.startIndices, predWidget.startIndices)
        val subResult = new fig.basic.EvalResult
        def addResult(tb:Boolean, pb:Boolean) = { subResult.add(tb, pb); result.add(tb, pb) }

        foreach(startIndices.size-1, { l:Int =>
          // Take care of unreachables: just get them wrong
          // ASSUMPTION: unreachables span the entire line l, so we just need to check the first position
          // Note that there might be multiple unreachables per example
          trueWidget.foreachEvent(startIndices(l), { e:Int =>
            if (e == unreachable_e)
              addResult(true, false) // Get it wrong automatically
          })

          def computeHit(widget:Widget) = {
            val hit = new HashSet[Int]
            foreach(startIndices(l), startIndices(l+1), { i:Int =>
              widget.foreachEvent(i, { e:Int => if (realEvent_?(e)) hit += e })
            })
            hit
          }

          val trueHit = computeHit(trueWidget)
          val predHit = computeHit(predWidget)

          // Get the things in common
          trueHit.foreach { e:Int =>
            val t = trueWidget.e2t(e)
            if (predHit(e)) {
              trueHit -= e
              predHit -= e
              counts(t)(t) += 1
              correctCounts(t) += 1
              addResult(true, true)
            }
          }

          // Record differences between two sets
          trueHit.foreach { e:Int => addResult(true, false) }
          predHit.foreach { e:Int => addResult(false, true) }
          if (trueHit.size == 0) {
            predHit.foreach { pe:Int => val pt = predWidget.e2t(pe)
              counts(T)(pt) += 1
            }
          }
          else if (predHit.size == 0) {
            trueHit.foreach { te:Int => val tt = trueWidget.e2t(te)
              counts(tt)(T) += 1
            }
          }
          else {
            // Heuristic: mark an error on all pairs
            trueHit.foreach { te:Int => val tt = trueWidget.e2t(te)
              predHit.foreach { pe:Int => val pt = predWidget.e2t(pe)
                counts(tt)(pt) += 1 // Note that tt = pt is possible and it is still wrong
              }
            }
          }
        })
        trueWidget.performance = subResult.toString
      }
    }

    def output(puts:(String=>Any)) = {
      // Print confusion matrix: true event types are columns, predicted event types are rows
      val ts = Utils.sortedIndices(map(T, { t:Int => tstr(t) }), false)
      val table =
        Array(Array("") ++ ts.map(tstr(_)) ++ Array("(NONE)")) ++
        ts.map { tt:Int =>
          Array(tstr(tt)) ++ ts.map { pt:Int => (if (tt == pt) correctCounts(tt)+"/" else "") + fmt(counts(tt)(pt)) } ++ Array(fmt(counts(tt)(T)))
        } ++
        Array(Array("(NONE)") ++ ts.map { pt:Int => fmt(counts(T)(pt)) } ++ Array(""))
      puts(result.toString)
      Utils.formatTable(table, { (r:Int,c:Int) => if (c == 0) -1 else if (r == 0) 0 else 1 }).foreach(puts(_))
    }
    def accuracy = result.f1
  }

  case class Example(name:String, events:Array[Event], text:Array[Int], labels:Array[Int], startIndices:Array[Int], trueWidget:Widget) extends AExample[Widget] {
    def E = events.size
    def N = text.size
    def numTokens = text.size
    val isPunctuation = map(N, { i:Int =>
      val s = wstr(text(i))
      s == "." || s == "," || s == "--" || s == "(" || s == ")" || (opts.andIsPunctuation && s == "and")
    })

    // Compute number of events of each type we have
    var eventTypeCounts : Array[Int] = null
    def computeEventTypeCounts = {
      eventTypeCounts = new Array[Int](T)
      events.foreach { event => eventTypeCounts(event.t) += 1 }
    }

    // Set up trackEvents: for each track
    var trackEvents : Array[Array[Int]] = null
    def computeTrackEvents = {
      trackEvents = map(C, { c:Int =>
        Utils.createArray({ add:(Int => Any) =>
          foreach(E, { e:Int => if (eventTypeAllowedOnTrack(c)(events(e).t)) add(e) })
        })
      })
    }

    def widgetToEvalFullString(widget:Widget) = {
      // For each original line in the input, output the widgets which were aligned
      Utils.createArray({ add:(String => Any) =>
        foreach(startIndices.size-1, { l:Int =>
          val alignedEvents = Utils.uniq(Utils.sort_!(Utils.createArray({ add:(Int => Any) =>
            foreach(startIndices(l), startIndices(l+1), { i:Int =>
              widget.foreachEvent(i, { e:Int => if (e != none_e) add(e) })
            })
          })))
          if (alignedEvents.size > 0) add(name + "\t" + l + " " + alignedEvents.mkString(" "))
        })
      }).mkString(" ")
    }

    def widgetToNiceFullString(widget:Widget) = {
      // Returns a string on one line; use tabs later to separate
      val n = Utils.same(numTokens, widget.events(0).size)
      val buf = new StringBuilder
      buf.append(name + ":")

      // This is rough (do it for entire example)
      val trueEvents = map(C, { c:Int => new HashSet[Int] }) // track -> set of events that go on that track
      if (trueWidget != null) {
        foreach(0, n, { i:Int =>
          trueWidget.foreachEvent(i, { e:Int =>
            if (realEvent_?(e))
              foreach(C, { c:Int => if (eventTypeAllowedOnTrack(c)(events(e).t)) trueEvents(c) += e })
          })
        })
      }
      // If we propose event e on track c, is that correct?
      def isOkay(c:Int, e:Int) = {
        trueWidget == null || (if (e == none_e) trueEvents(c).size == 0 else trueEvents(c)(e))
      }

      def renderWidget(widget:Widget, printUnused:Boolean) = {
        val used = new Array[Boolean](E)
        foreach(widget.events.size, { c:Int =>
          var i = 0
          while (i < n) { // Segment into entries
            val e = widget.events(c)(i)
            if (realEvent_?(e)) used(e) = true
            var j = i+1
            while (j < n && widget.events(c)(j) == e) j += 1
            buf.append("\t" + (if (widget == trueWidget || isOkay(c, e)) "" else "*") + "[TRACK"+c+"] ")
            if (e != none_e) buf.append((if (e == unreachable_e) "(unreachable)" else events(e)) + "[")

            if (widget.fields == null || !realEvent_?(e))
              buf.append(map(i, j, { k:Int => wstr(text(k)) }).mkString(" "))
            else {
              var k = i
              while (k < j) { // Segment i...j into fields
                val f = widget.fields(c)(k)
                var l = k+1
                while (l < j && widget.fields(c)(l) == f) l += 1
                if (k != i) buf.append(" ")
                if (f != -1) buf.append(events(e).fstr(f) + "[")
                buf.append(map(k, l, { m:Int =>
                  var str = wstr(text(m))
                  if (widget.gens != null && widget.gens(c)(m) != -1) str += "_" + short_gstr(widget.gens(c)(m))
                  if (widget.numMethods != null && widget.numMethods(c)(m) != -1) str += short_mstr(widget.numMethods(c)(m))
                  str
                }).mkString(" "))
                if (f != -1) buf.append("]")
                k = l
              }
            }
            if (e != none_e) buf.append("]")
            i = j
          }
        })

        // Print out unused events
        if (printUnused) {
          foreach(E, { e:Int =>
            if (!used(e))
              buf.append(fmts("\t%s[]", events(e)))
          })
        }
      }

      buf.append("\t- Pred:")
      renderWidget(widget, true) // Prediction
      if (trueWidget != null) { // Truth
        buf.append("\t- True:")
        renderWidget(trueWidget, false)
        buf.append("\t" + trueWidget.performance + " ("+E + " possible events)")
        /*if (trueWidget.eventPosterior != null)
          buf.append("\t" + trueWidget.eventPosteriorStr(events))*/
      }

      buf.toString
    }
  }

  ////////////////////////////////////////////////////////////
  // Inference

  var L = opts.maxPhraseLength
  val segPenalty = map(L+1, { l:Int => Math.exp(-Math.pow(l, opts.segPenalty)) })
  def end(i:Int, N:Int) = (i+L) min N

  case class InferState(override val ex:Example, override val params:Params, override val counts:Params, override val ispec:InferSpec) extends
      AHypergraphInferState[Widget,Example,Params](ex, params, counts, ispec) {
    def newMatrix = Utils.set_!(new Array[Array[Int]](C, ex.N), -1)
    def newWidget = new Widget(newMatrix, newMatrix, newMatrix, newMatrix, ex.startIndices, { e:Int => ex.events(e).t })

    // FUTURE: speed up these lookups
    def getv(e:Int, f:Int) = ex.events(e).values(f)
    def get_tparams(e:Int) = params.eventTypeParams(ex.events(e).t)
    def get_tcounts(e:Int) = counts.eventTypeParams(ex.events(e).t)
    def get_num_fparams(e:Int, f:Int) = get_tparams(e).fieldParams(f) match { case p:NumFieldParams => p; case _ => throw impossible }
    def get_num_fcounts(e:Int, f:Int) = get_tcounts(e).fieldParams(f) match { case p:NumFieldParams => p; case _ => throw impossible }
    def get_cat_fparams(e:Int, f:Int) = get_tparams(e).fieldParams(f) match { case p:CatFieldParams => p; case _ => throw impossible }
    def get_cat_fcounts(e:Int, f:Int) = get_tcounts(e).fieldParams(f) match { case p:CatFieldParams => p; case _ => throw impossible }
    def get_sym_fparams(e:Int, f:Int) = get_tparams(e).fieldParams(f) match { case p:SymFieldParams => p; case _ => throw impossible }
    def get_sym_fcounts(e:Int, f:Int) = get_tcounts(e).fieldParams(f) match { case p:SymFieldParams => p; case _ => throw impossible }
    def get_str_fparams(e:Int, f:Int) = get_tparams(e).fieldParams(f) match { case p:StrFieldParams => p; case _ => throw impossible }
    def get_str_fcounts(e:Int, f:Int) = get_tcounts(e).fieldParams(f) match { case p:StrFieldParams => p; case _ => throw impossible }

    def iterInRange(interval:fig.basic.Pair[Integer,Integer]) =
      ispec.iter >= interval.getFirst.intValue && (interval.getSecond.intValue == -1 || ispec.iter < interval.getSecond.intValue)
    def prevIterInRange(interval:fig.basic.Pair[Integer,Integer]) =
      ispec.iter-1 >= interval.getFirst.intValue && (interval.getSecond.intValue == -1 || ispec.iter-1 < interval.getSecond.intValue)

    // Options which change depending on iteration
    def indepEventTypes = iterInRange(opts.indepEventTypes)
    def indepFields = iterInRange(opts.indepFields)
    def newEventTypeFieldPerWord = iterInRange(opts.newEventTypeFieldPerWord)
    def newFieldPerWord = iterInRange(opts.newFieldPerWord)
    def oneEventPerExample = iterInRange(opts.oneEventPerExample)
    def oneFieldPerEvent = iterInRange(opts.oneFieldPerEvent)
    def genLabels = iterInRange(opts.genLabels)

    def useFieldSets(t:Int) = eventTypes(t).useFieldSets && iterInRange(opts.useFieldSets)

    // For smooth q-handoff
    def prevIndepEventTypes = prevIterInRange(opts.indepEventTypes)
    def prevIndepFields = prevIterInRange(opts.indepFields)
    def prevGenLabels = prevIterInRange(opts.genLabels)

    def createHypergraph(H:Hypergraph[Widget]) = {
      H.debug = opts.debug
      H.allowEmptyNodes = true // Need this because the pc sets might be inconsistent with the types

      val words = ex.text
      val labels = ex.labels
      val N = words.size
      val nums = words.map { w:Int => str2num(wstr(w)) }

      case class NumFieldValueNode(i:Int, c:Int, e:Int, f:Int)
      def genNumFieldValue(i:Int, c:Int, e:Int, f:Int) = {
        if (nums(i) == NaN) H.invalidNode // Can't generate if not a number
        else {
          val node = new NumFieldValueNode(i, c, e, f)
          if (H.addSumNode(node)) {
            val v = getv(e, f) // Consider generating nums(i) from v
            val fparams = get_num_fparams(e, f)
            val fcounts = get_num_fcounts(e, f)

            if (v == nums(i)) { // M_IDENTITY
              H.addEdge(node, new Info {
                def getWeight = get(fparams.methodChoices, M_IDENTITY)
                def setPosterior(prob:Double) = update(fcounts.methodChoices, M_IDENTITY, prob)
                def choose(widget:Widget) = { widget.numMethods(c)(i) = M_IDENTITY; widget }
              })
            }
            if (roundUp(v) == nums(i)) { // M_ROUNDUP
              H.addEdge(node, new Info {
                def getWeight = get(fparams.methodChoices, M_ROUNDUP)
                def setPosterior(prob:Double) = update(fcounts.methodChoices, M_ROUNDUP, prob)
                def choose(widget:Widget) = { widget.numMethods(c)(i) = M_ROUNDUP; widget }
              })
            }
            if (roundDown(v) == nums(i)) { // M_ROUNDDOWN
              H.addEdge(node, new Info {
                def getWeight = get(fparams.methodChoices, M_ROUNDDOWN)
                def setPosterior(prob:Double) = update(fcounts.methodChoices, M_ROUNDDOWN, prob)
                def choose(widget:Widget) = { widget.numMethods(c)(i) = M_ROUNDDOWN; widget }
              })
            }
            if (roundClose(v) == nums(i)) { // M_ROUNDCLOSE
              H.addEdge(node, new Info {
                def getWeight = get(fparams.methodChoices, M_ROUNDCLOSE)
                def setPosterior(prob:Double) = update(fcounts.methodChoices, M_ROUNDCLOSE, prob)
                def choose(widget:Widget) = { widget.numMethods(c)(i) = M_ROUNDCLOSE; widget }
              })
            }
            val noise = nums(i) - v // M_NOISEUP and M_NOISEDOWN
            if (noise > 0) {
              H.addEdge(node, new Info {
                def getWeight = {
                  get(fparams.methodChoices, M_NOISEUP) * 0.5 *   
				  Math.pow(get(fparams.rightNoiseChoices, S_CONTINUE), noise-1) *
                  get(fparams.rightNoiseChoices, S_STOP)
                }
                def setPosterior(prob:Double) = {
                  update(fcounts.methodChoices, M_NOISEUP, prob)
                  update(fcounts.rightNoiseChoices, S_CONTINUE, (noise-1)*prob)
                  update(fcounts.rightNoiseChoices, S_STOP, prob)
                }
                def choose(widget:Widget) = { widget.numMethods(c)(i) = M_NOISEUP; widget }
              })
            }
            else if (noise < 0) {
              H.addEdge(node, new Info {
                def getWeight = {
                  get(fparams.methodChoices, M_NOISEDOWN) *
				  Math.pow(get(fparams.leftNoiseChoices, S_CONTINUE), -noise-1) *
                  get(fparams.leftNoiseChoices, S_STOP)
                }
                def setPosterior(prob:Double) = {
                  update(fcounts.methodChoices, M_NOISEDOWN, prob)
                  update(fcounts.leftNoiseChoices, S_CONTINUE, (-noise-1)*prob)
                  update(fcounts.leftNoiseChoices, S_STOP, prob)
                }
                def choose(widget:Widget) = { widget.numMethods(c)(i) = M_NOISEDOWN; widget }
              })
            }
          }
          node
        }
      }

      case class CatFieldValueNode(i:Int, c:Int, e:Int, f:Int)
      def genCatFieldValue(i:Int, c:Int, e:Int, f:Int) = {
        val node = new CatFieldValueNode(i, c, e, f)
        if (H.addSumNode(node)) {
          val v = getv(e, f) // Consider generating words(i) from category v
          val w = words(i)
          val fparams = get_cat_fparams(e, f)
          val fcounts = get_cat_fcounts(e, f)

          H.addEdge(node, new Info {
            def getWeight = get(fparams.emissions(v), w)
            def setPosterior(prob:Double) = update(fcounts.emissions(v), w, prob)
            def choose(widget:Widget) = widget
          })
        }
        node
      }

      case class SymFieldValueNode(i:Int, c:Int, e:Int, f:Int)
      def genSymFieldValue(i:Int, c:Int, e:Int, f:Int) = {
        val v = getv(e, f) // words(i) must match v exactly
        if (words(i) != v) H.invalidNode
        else if (genLabels || prevGenLabels) { // Generate label
          val node = new SymFieldValueNode(i, c, e, f)
          val fparams = get_sym_fparams(e, f)
          val fcounts = get_sym_fcounts(e, f)
          if (H.addSumNode(node)) {
            H.addEdge(node, new Info {
              def getWeight = {
                get(fparams.labelChoices, labels(i)) /
                get(params.genericLabelChoices, labels(i)) // Remove default generation
              }
              def setPosterior(prob:Double) = {
                if (genLabels) {
                  update(fcounts.labelChoices, labels(i), prob)
                  updateKeepNonNegative(counts.genericLabelChoices, labels(i), -prob)
                }
              }
              def choose(widget:Widget) = widget
            })
          }
          node
        }
        else
          H.endNode
      }

      case class StrFieldValueNode(i:Int, c:Int, e:Int, f:Int)
      def genStrFieldValue(i:Int, c:Int, e:Int, f:Int) = {
        val (v_words, v_labels) = ex.events(e).fields(f) match {
          case field:StrField => field.indexer.getObject(getv(e, f)) // words(i) must match one of the words in v exactly
          case _ => throw impossible
        }
        if (!v_words.contains(words(i))) H.invalidNode
        else {
          val node = new StrFieldValueNode(i, c, e, f)
          val fparams = get_str_fparams(e, f)
          val fcounts = get_str_fcounts(e, f)
          if (H.addSumNode(node)) {
            // Note: previous versions of this code just generated the first word instead of all of them
            // That was mathematically wrong, but overfit slightly less
            foreach(Utils.same(v_words.size, v_labels.size), { v_i:Int =>
              if (v_words(v_i) == words(i)) { // Match
                H.addEdge(node, new Info {
                  def getWeight = {
                    1.0/v_words.size * // Pick uniformly at random
                    (if (genLabels || prevGenLabels)
                      get(fparams.labelChoices(v_labels(v_i)), labels(i)) / // Remove default generation
                      get(params.genericLabelChoices, labels(i))
                     else 1.0)
                  }
                  def setPosterior(prob:Double) = {
                    if (genLabels) {
                      update(fcounts.labelChoices(v_labels(v_i)), labels(i), prob)
                      updateKeepNonNegative(counts.genericLabelChoices, labels(i), -prob)
                    }
                  }
                  def choose(widget:Widget) = widget
                })
              }
            })
          }
          node
        }
      }
      
      def genFieldValue(i:Int, c:Int, e:Int, f:Int) = {
        ex.events(e).fields(f) match {
          case _:NumField => genNumFieldValue(i, c, e, f)
          case _:CatField => genCatFieldValue(i, c, e, f)
          case _:SymField => genSymFieldValue(i, c, e, f)
          case _:StrField => genStrFieldValue(i, c, e, f)
          case _ => throw impossible
        }
      }

      def getEventTypeGivenWord(t:Int, w:Int) = {
        if (opts.includeEventTypeGivenWord) get(params.eventTypeChoicesGivenWord(w), t) else 1.0
      }
      def updateEventTypeGivenWord(t:Int, w:Int, prob:Double) = {
        if (opts.includeEventTypeGivenWord) update(counts.eventTypeChoicesGivenWord(w), t, prob)
      }

      case class WordNode(i:Int, c:Int, e:Int, f:Int)
      def genWord(i:Int, c:Int, e:Int, f:Int) = { // Generate word at position i with event e and field f
        val node = new WordNode(i, c, e, f)
        val t = ex.events(e).t
        val tparams = params.eventTypeParams(t)
        val tcounts = counts.eventTypeParams(t)
        val w = words(i)
        if (H.addSumNode(node)) {
          if (f == tparams.none_f) {
            // Talk about the event type, not a particular field
            H.addEdge(node, new Info {
              def getWeight = {
                get(tparams.noneFieldEmissions, w) *
                getEventTypeGivenWord(t, w)
              }
              def setPosterior(prob:Double) = {
                update(tcounts.noneFieldEmissions, w, prob)
                updateEventTypeGivenWord(t, w, prob)
              }
              def choose(widget:Widget) = widget
            })
          }
          else {
            // G_FIELD_NAME: generate based on field name (don't need this)
            /*H.addEdge(node, new Info {
              def getWeight = {
                get(tparams.genChoices(f), G_FIELD_NAME) *
                get(tparams.fieldNameEmissions(f), w)
              }
              def setPosterior(prob:Double) = {
                update(tcounts.genChoices(f), G_FIELD_NAME, prob)
                update(tcounts.fieldNameEmissions(f), w, prob)
              }
              def choose(widget:Widget) = { widget.gens(i) = G_FIELD_NAME; widget }
            })*/

            // G_FIELD_VALUE: generate based on field value
            H.addEdge(node, genFieldValue(i, c, e, f), new Info {
              def getWeight = {
                get(tparams.genChoices(f), G_FIELD_VALUE) *
                getEventTypeGivenWord(t, w)
              }
              def setPosterior(prob:Double) = {
                update(tcounts.genChoices(f), G_FIELD_VALUE, prob)
                updateEventTypeGivenWord(t, w, prob)
              }
              def choose(widget:Widget) = { widget.gens(c)(i) = G_FIELD_VALUE; widget }
            })

            // G_FIELD_GENERIC: generate based on event type
            H.addEdge(node, new Info {
              def getWeight = {
                get(tparams.genChoices(f), G_FIELD_GENERIC) *
                get(params.genericEmissions, w) *
                getEventTypeGivenWord(t, w)
              }
              def setPosterior(prob:Double) = {
                update(tcounts.genChoices(f), G_FIELD_GENERIC, prob)
                update(counts.genericEmissions, w, prob)
                updateEventTypeGivenWord(t, w, prob)
              }
              def choose(widget:Widget) = { widget.gens(c)(i) = G_FIELD_GENERIC; widget }
            })
          }
        }
        node
      }

      // Generate field f of event e from begin to end
      case class FieldNode(begin:Int, end:Int, c:Int, e:Int, f:Int)
      def genField(begin:Int, end:Int, c:Int, e:Int, f:Int) : Any = {
        val node = new FieldNode(begin, end, c, e, f)
        if (H.addProdNode(node)) {
          foreach(begin, end, { i:Int => // Generate each word in this range independently
            H.addEdge(node, genWord(i, c, e, f), new Info {
                  def getWeight = {
                    1.0
					//get( params.lengthParams.wordContinue(ex.events(e).t)(f), S_CONTINUE )
                  }   
                  def setPosterior(prob:Double) = {
                    //update(counts.lengthParams.wordContinue(ex.events(e).t)(f), S_CONTINUE, prob)
                  }
                  def choose(widget:Widget) = widget	
		    })
          })
        }
        node
      }

      // Generate segmentation of i...end into fields; previous field is f0
      case class FieldsNode(i:Int, end:Int, c:Int, e:Int, f0:Int, efs:Int)
      def genFields(i:Int, end:Int, c:Int, e:Int, f0:Int, efs:Int) : Any = {
        val tparams = params.eventTypeParams(ex.events(e).t)
        val tcounts = counts.eventTypeParams(ex.events(e).t)
		val t = ex.events(e).t
        if (i == end) {
          if (tparams.efs_canBeEmpty(efs)) { // Make sure we've used all the fields we agreed to see
            if (indepFields)
              H.endNode
            else {
              val node = new FieldsNode(end, end, c, e, f0, efs)
              if (H.addSumNode(node)) { // Transition to boundary_f
                H.addEdge(node, new Info {
                  def getWeight = {
                    { if (prevIndepFields) 1.0
                      else get(tparams.fieldChoices(f0), tparams.boundary_f) } 
                  }
                  def setPosterior(prob:Double) = {
					update(tcounts.fieldChoices(f0), tparams.boundary_f, prob)
				  }
                  def choose(widget:Widget) = widget
                })
              }
              node
            }
          }
          else
            H.invalidNode
        }
        else {
          val node = new FieldsNode(i, end, c, e, f0, efs)
          if (H.addSumNode(node)) {
            def select_j(j:Int) = { // Choose ending position j
			  foreach(ex.events(e).F+1, { f:Int => // Choose a new field to talk about (including none field, but not boundary)
                if (f == tparams.none_f || // If not none, then...
                    ((!opts.disallowConsecutiveRepeatFields || f != f0) && // Can't repeat fields
                     tparams.efs_canBePresent(efs, f) && // Make sure f can be there
                     (!opts.limitFieldLength || j-i <= eventTypes(ex.events(e).t).fields(f).maxLength))) { // Limit field length
                  val remember_f = {
                    if (indepFields) tparams.boundary_f
                    //else if (f == tparams.none_f) f0 // Don't remember going to null none_f
                    // We now treat none_f like another field which serves a purpose - not for junk
                    else f
                  }
                  val new_efs = {
                    if (f == tparams.none_f) efs
                    else tparams.efs_addAbsent(efs, f) // Now, allow f to be absent as we've already taken care of it
                  }
                  H.addEdge(node, genField(i, j, c, e, f), genFields(j, end, c, e, remember_f, new_efs), new Info {
                    def getWeight = {
                      if (prevIndepFields){
					    get(tparams.fieldChoices(tparams.boundary_f), f) // f0 == boundary_f under indepFields, so use that
					  } else {
						get(tparams.fieldChoices(f0), f)
					  } * 
					  1.0
					  //get(params.lengthParams.fieldContinue(t), S_CONTINUE) *
					  //get(params.lengthParams.wordContinue(t)(f), S_STOP)
                    }
                    def setPosterior(prob:Double) = {
							//update(counts.lengthParams.fieldContinue(t), S_CONTINUE, prob)
							//update(counts.lengthParams.wordContinue(t)(f), S_STOP, prob)
							update(tcounts.fieldChoices(f0), f, prob)
					}
                    def choose(widget:Widget) = {
                      foreach(i, j, { k:Int => widget.fields(c)(k) = f })
                      widget
                    }
                  })
                }
              })
            }
            if (oneFieldPerEvent)
              select_j(end)
            else if (newFieldPerWord)
              select_j(i+1)
            else
              foreach(i+1, end+1, select_j _)
          }
          node
        }
      }

      // Default: don't generate any event (there should be only one of these nodes)
      // Note: we don't need any state, but include i and c so that we get distinct nodes (see note in Hypergraph)
      case class SelectNoEventsNode(i:Int, c:Int)
      def selectNoEvents(i:Int, c:Int) = {
        if (ex.E == 0)
          H.endNode
        else {
          val node = new SelectNoEventsNode(i, c)
          if (H.addProdNode(node)) {
            foreach(ex.E, { e:Int =>
              val t = ex.events(e).t
              val tparams = params.eventTypeParams(t)
              val tcounts = counts.eventTypeParams(t)
              H.addEdge(node, new Info {
                def getWeight = get(tparams.filters, B_FALSE)
                def setPosterior(prob:Double) = update(tcounts.filters, B_FALSE, prob)
                def choose(widget:Widget) = widget
              })
            })
          }
          node
        }
      }

      case class NoneEventWordsNode(i:Int, j:Int, c:Int)
      def genNoneEventWords(i:Int, j:Int, c:Int) : Any = {
        val node = new NoneEventWordsNode(i, j, c)
        if (H.addProdNode(node)) {
          foreach(i, j, { k:Int => // Generate each word in this range independently
            val w = words(k)
			H.addEdge(node, new Info {
              def getWeight = {
				get(params.trackParams(c).noneEventTypeEmissions, w) *
                getEventTypeGivenWord(none_t, w) *
                1.0
				//get( params.lengthParams.noEventWordContinue, S_CONTINUE )
              }
              def setPosterior(prob:Double) = {
                update(counts.trackParams(c).noneEventTypeEmissions, w, prob)
                updateEventTypeGivenWord(none_t, w, prob)
                //update( counts.lengthParams.noEventWordContinue, S_CONTINUE , prob)
				//dbgs("SET: none word %s at %s: %s (now %s)", wstr(w), k, prob, counts.trackParams(c).noneEventTypeEmissions.getCount(w))
              }
              def choose(widget:Widget) = widget
            })
          })
        }
        node
      }

      // Generate no event from i to j; incorporate salience if necessary
      case class NoneEventNode(i:Int, j:Int, c:Int)
      def genNoneEvent(i:Int, j:Int, c:Int) = {
        val node = new NoneEventNode(i,j,c)
		if (opts.useEventSalienceModel) {
          if (H.addProdNode(node))
            H.addEdge(node, genNoneEventWords(i, j, c), selectNoEvents(i, c), new Info{
			  def getWeight = {
			    1.0
				//get( params.lengthParams.noEventWordContinue, S_STOP )
			  }
			  def setPosterior(prob:Double) = {
			    //update( counts.lengthParams.noEventWordContinue, S_STOP , prob)
			  }
			  def choose(widget:Widget) = widget
			})
          node
        }
        else
          if(H.addSumNode(node))
		    H.addEdge(node, genNoneEventWords(i,j,c), new Info{
			  def getWeight = {
			    1.0
				//get( params.lengthParams.noEventWordContinue, S_STOP )
			  }
			  def setPosterior(prob:Double) = {
			    //update( counts.lengthParams.noEventWordContinue, S_STOP , prob)
			  }
			  def choose(widget:Widget) = widget
			})
		  node
		  //genNoneEventWords(i, j, c)
      }

      // Generate the event, but make field sets respect efs
      case class EFSEventNode(i:Int, j:Int, c:Int, e:Int, efs:Int)
      def genEFSEvent(i:Int, j:Int, c:Int, e:Int, efs:Int) = {
        val tparams = params.eventTypeParams(ex.events(e).t)
        val tcounts = counts.eventTypeParams(ex.events(e).t)
        if (opts.useEventSalienceModel) {
          val node = new EventNode(i, j, c, e)
          if (H.addProdNode(node)) {
            // We have to choose event e and not the others
            H.addEdge(node, genFields(i, j, c, e, tparams.boundary_f, efs), selectNoEvents(i, c), new Info {
              def getWeight = {
                ( get(tparams.filters, B_TRUE) /
                  get(tparams.filters, B_FALSE) )  //Compensate
//				get(params.lengthParams.fieldContinue(ex.events(e).t), S_STOP)
              }
              def setPosterior(prob:Double) = {
                update(tcounts.filters, B_TRUE, prob)
                update(tcounts.filters, B_FALSE, -prob) // Compensate
//				update(counts.lengthParams.fieldContinue(ex.events(e).t), S_STOP, prob)
              }
              def choose(widget:Widget) = widget
            })
          }
          node
        }
        else
          genFields(i, j, c, e, tparams.boundary_f, efs)
      }

      // Generate event e from i to j; incorporate salience if necessary
      case class EventNode(i:Int, j:Int, c:Int, e:Int)
      def genEvent(i:Int, j:Int, c:Int, e:Int) = {
        val tparams = params.eventTypeParams(ex.events(e).t)
        val tcounts = counts.eventTypeParams(ex.events(e).t)
        if (useFieldSets(ex.events(e).t)) {
          var node = new EventNode(i, j, c, e)
          if (H.addSumNode(node)) {
            // Choose which fields to use
            tparams.allowed_fs.foreach { fs:Int =>
              H.addEdge(node, genEFSEvent(i, j, c, e, tparams.fs2efs(fs)), new Info {
                def getWeight = get(tparams.fieldSetChoices, fs )
                def setPosterior(prob:Double) = update(tcounts.fieldSetChoices, fs, prob)
                def choose(widget:Widget) = widget
              })
            }
          }
          node
        }
        else // Can use any field set
          genEFSEvent(i, j, c, e, tparams.dontcare_efs)
      }

      // Generate track c in i...j (t0 is previous event type for track 0); allowNone and allowReal specify what event types we can use
      case class TrackNode(i:Int, j:Int, t0:Int, c:Int, allowNone:Boolean, allowReal:Boolean)
      def genTrack(i:Int, j:Int, t0:Int, c:Int, allowNone:Boolean, allowReal:Boolean) = {
        val node = new TrackNode(i, j, t0, c, allowNone, allowReal)
        val cparams = params.trackParams(c)
        val ccounts = counts.trackParams(c)
        // WARNING: allowNone/allowReal might not result in any valid nodes
        if (H.addSumNode(node)) {
          // (1) Choose the none event
		  //if(false) {
		  if (allowNone && (!trueInfer || ex.trueWidget == null || ex.trueWidget.hasNoReachableContiguousEvents(i, j, c))) {
            val remember_t = t0 // Don't remember none_t (since t == none_t, skip t)
            val recurseNode = if (c == 0) genEvents(j, remember_t) else H.endNode
			if (opts.useEventTypeDistrib) {
              H.addEdge(node, genNoneEvent(i, j, c), recurseNode, new Info {
                def getWeight = {
				  get(cparams.eventTypeChoices(t0), none_t)
				  //get(cparams.eventTypeChoices(t0), none_t) *
                  //get(params.lengthParams.fieldContinue(none_t), S_STOP) *
				  //get(params.lengthParams.eventContinue, S_CONTINUE)
			     }	
				//def setPosterior(prob:Double) = update(counts.eventTypeChoices(t0), none_t, prob)
                def setPosterior(prob:Double) = {
                  update(ccounts.eventTypeChoices(t0), none_t, prob)
				  if (ex.trueWidget != null && i == 0) // HACK
                    ex.trueWidget.setEventPosterior(none_e, ex.E, prob)
				  //update(counts.lengthParams.fieldContinue(none_t), S_STOP, prob)
                  //update(counts.lengthParams.eventContinue, S_CONTINUE, prob)
                  //dbgs("SET: none %s", prob)
                }
                def choose(widget:Widget) = { foreach(i, j, { k:Int => widget.events(c)(k) = none_e }); widget }
              })
            }
            else {
              H.addEdge(node, genNoneEvent(i, j, c), recurseNode, new Info {
                def getWeight = {
                  1.0
				  //get(params.lengthParams.fieldContinue(none_t), S_STOP) *
				  //get(params.lengthParams.eventContinue, S_CONTINUE) 
			    }
                def setPosterior(prob:Double) = { 
				  //update(counts.lengthParams.fieldContinue(none_t), S_STOP, prob)
                  //update(counts.lengthParams.eventContinue, S_CONTINUE, prob)
				}
                def choose(widget:Widget) = { foreach(i, j, { k:Int => widget.events(c)(k) = none_e }); widget }
              })
            }
          }
          // (2) Choose an event type t and event e for track c
          ex.trackEvents(c).foreach { e:Int => 
		    val t = ex.events(e).t
            if (allowReal && (!trueInfer || ex.trueWidget == null || ex.trueWidget.hasContiguousEvents(i, j, e))) {
			  val remember_t = if (indepEventTypes) none_t else t
              val recurseNode = if (c == 0) genEvents(j, remember_t) else H.endNode
              if (opts.useEventTypeDistrib) {
				H.addEdge(node, genEvent(i, j, c, e), recurseNode, new Info {
                  def getWeight = {
                    val weight =  
					  // remember_t = t under indepEventTypes
					  {
					  if (prevIndepEventTypes)  get(cparams.eventTypeChoices(none_t), t) * (1.0/ex.eventTypeCounts(t))
                      else get(cparams.eventTypeChoices(t0), t) * (1.0/ex.eventTypeCounts(t)) 
					  } *
					  1.0
					  //get(params.lengthParams.fieldContinue(t), S_STOP) *
					  //get(params.lengthParams.eventContinue, S_CONTINUE) 
					weight
                  }
                  //def setPosterior(prob:Double) = update(ccounts.eventTypeChoices(t0), t, prob)
                  def setPosterior(prob:Double) = {
                    update(ccounts.eventTypeChoices(t0), t, prob)
                    if (ex.trueWidget != null && i == 0) // HACK
                      ex.trueWidget.setEventPosterior(e, ex.E, prob)
					//update(counts.lengthParams.fieldContinue(t), S_STOP, prob)
					//update(counts.lengthParams.eventContinue, S_CONTINUE, prob)
                  }
                  def choose(widget:Widget) = { foreach(i, j, { k:Int => widget.events(c)(k) = e }); widget }
                })
              }
              else {
				H.addEdge(node, genEvent(i, j, c, e), recurseNode, new Info {
                  def getWeight = 1.0 *
				    1.0
					//get(params.lengthParams.fieldContinue(t), S_STOP)*
                    //get(params.lengthParams.eventContinue, S_CONTINUE) 
                  def setPosterior(prob:Double) = { 
					//update(counts.lengthParams.fieldContinue(t), S_STOP, prob)
					//update(counts.lengthParams.eventContinue, S_CONTINUE, prob)
				  }
                  def choose(widget:Widget) = { foreach(i, j, { k:Int => widget.events(c)(k) = e }); widget }
                })
              }
            }
          }
        }
        node
      }

      // pc says for each c whether the event type on track c can be none_t or not
      case class PCEventsNode(i:Int, j:Int, t0:Int, pc:Int)
      def genPCEvents(i:Int, j:Int, t0:Int, pc:Int) : Any = {
		def allowNone_?(c:Int) = if(opts.alignProhibitNone) false else {pc == wildcard_pc || !setContains(pc, c)}
        def allowReal_?(c:Int) = pc == wildcard_pc || setContains(pc, c)
        val node = new PCEventsNode(i, j, t0, pc)
        if (H.addProdNode(node)) {
          foreach(C, { c:Int => // For each track, do independently
            H.addEdge(node, genTrack(i, j, t0, c, allowNone_?(c), allowReal_?(c)))
          })
          // Note: there might be nothing consistent with pc (one track has no events and pc says we need events)
          if (pc == wildcard_pc) H.assertNonEmpty(node)
        }
        node
      }

      // Generate segmentation of i...N into event types; previous event type is t0
      // Incorporate eventType distributions
      case class EventsNode(i:Int, t0:Int)
      def genEvents(i:Int, t0:Int) : Any = {
        if (i == N) H.endNode
        else {
          val node = new EventsNode(i, t0)
          if (H.addSumNode(node)) {
            def selectEnd(j:Int) = {
              if (opts.jointEventTypeDecision) {
                foreach(PC, { pc:Int => // Choose track bitmask pc
                  H.addEdge(node, genPCEvents(i, j, t0, pc), new Info {
                    def getWeight = get(params.trackChoices, pc)
                    def setPosterior(prob:Double) = update(counts.trackChoices, pc, prob)
                    def choose(widget:Widget) = widget
                  })
                })
              }
              else
                H.addEdge(node, genPCEvents(i, j, t0, wildcard_pc)) // Do each track independently
            }

            if (oneEventPerExample)
              selectEnd(N)
            else if (newEventTypeFieldPerWord)
              selectEnd(i+1)
            else if (opts.onlyBreakOnPunctuation && opts.dontCrossPunctuation) // Break at first punctuation
              selectEnd(Utils.find(i+1, N, { j:Int => ex.isPunctuation(j-1) }))
            else if (opts.onlyBreakOnPunctuation) // Break at punctuation (but can cross)
              foreach(i+1, end(i, N)+1, { j:Int => if (j == N || ex.isPunctuation(j-1)) selectEnd(j) })
            else if (opts.dontCrossPunctuation) // Go up until the first punctuation
              foreach(i+1, Utils.find(i+1, N, { j:Int => ex.isPunctuation(j-1) })+1, selectEnd _)
            else // Allow everything
              foreach(i+1, end(i, N)+1, selectEnd _)

            H.assertNonEmpty(node)
          }
          node
        }
      }

      if (genLabels || prevGenLabels) {
        // Default is to generate the labels from a generic distribution unless we say otherwise
        foreach(ex.N, { i:Int =>
          H.addEdge(H.prodStartNode, new Info {
            def getWeight = get(params.genericLabelChoices, labels(i))
            def setPosterior(prob:Double) = if (genLabels) update(counts.genericLabelChoices, labels(i), prob)
            def choose(widget:Widget) = widget
          })
        })
      }
      H.addEdge(H.prodStartNode, genEvents(0, none_t), new Info {
        def getWeight = { 
		  1.0
		  //get(params.lengthParams.eventContinue, S_STOP) 
		}
        def setPosterior(prob:Double) = { 
		  //update(counts.lengthParams.eventContinue, S_STOP, prob)
		}
        def choose(widget:Widget) = widget
      })
    }

    // Override bestWidget
    if (opts.fullPredRandomBaseline) {
      if (ex.E > 0) {
        // Just match each line in the text to a single randomly chosen event
        foreach(ex.startIndices.size-1, { l:Int =>
          val e = opts.fullPredRandom.nextInt(ex.E)
          foreach(ex.startIndices(l), ex.startIndices(l+1), { i:Int =>
            bestWidget.events(0)(i) = e // Assume one track
          })
        })
      }
    }

    // Sample
    /*foreach(10, { i:Int =>
      val result = hypergraph.fetchSampleHyperpath(opts.genRandom, newWidget)
      dbgs("%s: %s", result.logWeight, ex.widgetToNiceFullString(result.widget))
    })*/
  }

  ////////////////////////////////////////////////////////////
  // Model: reading in examples

  class Model extends AModel[Widget,Params,Performance,Example,InferState] {
    
	def fieldType(f:Field):Int = {
		f match {
			case (_:NumField) => return generation.FieldType.NUMERIC
			case (_:CatField) => return generation.FieldType.CATEGORICAL
			case (_:SymField) => return generation.FieldType.SYMBOL
			case (_:StrField) => return generation.FieldType.STRING
			case (_) => throw impossible
		}
	
	}
	
	/*
	*	Interface for Numeric Generation
	*	TODO this is all a bit hack-y
	*/
	def methodProb(t:Int, f:Int, m:Int):Double = {
		params.eventTypeParams(t).fieldParams(f) match {
			case param:NumFieldParams => { param.methodChoices.getProb(m) }
			case _ => 0.0
		}
	}

	def leftNoiseProb(t:Int, f:Int, m:Int):Double = {
		params.eventTypeParams(t).fieldParams(f) match {
			case param:NumFieldParams => { param.leftNoiseChoices.getProb(m) }
			case _ => 0.0
		}
	}

	def rightNoiseProb(t:Int, f:Int, m:Int):Double = {
		params.eventTypeParams(t).fieldParams(f) match {
			case param:NumFieldParams => { param.rightNoiseChoices.getProb(m) }
			case _ => 0.0
		}
	}

	def genWord(t:Int, f:Int, v:Int, genType:Int):String = {
		def get_tparams(t:Int) = params.eventTypeParams(t)
		def get_cat_fparams(t:Int, f:Int) = get_tparams(t).fieldParams(f) match { case p:CatFieldParams => p; case _ => throw impossible }
		def get_sym_fparams(t:Int, f:Int) = get_tparams(t).fieldParams(f) match { case p:SymFieldParams => p; case _ => throw impossible }
		def get_str_fparams(t:Int, f:Int) = get_tparams(t).fieldParams(f) match { case p:StrFieldParams => p; case _ => throw impossible }	

		def genCat:String = {
			var max:Double = 0.0
			var argmax:Int = -1
			val fparams = get_cat_fparams(t,f)
			foreach(W, (w:Int) => {
				try{
					val dummyInt:Int = Integer.parseInt(wstr(w))
					val cand:Double = fparams.emissions(v).getProb(w)
					if(cand > max){
						max = cand
						argmax = w
					}
				} catch {
					case (e:Exception) => {}	//TODO this is an overly general catch clause
				}
			})
			if(argmax >= 0) wstr(argmax)
			else "FAILWORD"
		}

		def genStr:String = {
			val fparams = get_str_fparams(t,f)
			//TODO Gen from String
			"GENSTR_NOT_IMPLEMENTED"
		}

		genType match {
			case (generation.FieldType.NUMERIC) => throw impossible
			case (generation.FieldType.CATEGORICAL) => genCat
			case (generation.FieldType.SYMBOL) => throw new java.lang.IllegalArgumentException("Symbol generation not implemented")
			case (generation.FieldType.STRING) => genStr
			case (_) => throw impossible
		}
	
	}

	/*
	*	^^END
	*/


	override def logStats = {
      super.logStats
      putLogRec("numWords", W)
      putLogRec("numLabels", LB)
      putLogRec("numEventTypes", T)
      putLogRec("numFields", map(T, { t:Int => eventTypes(t).F }).mkString(" "))
      putLogRec("numFieldValues", map(T, { t:Int =>
        "["+eventTypes(t).name+"] " +
        eventTypes(t).fields.mkString(" ")
      }).mkString(" | "))
    }

    def widgetToIntSeq(widget:Widget) = widget.events(0) // WARNING: only use first track
    override def widgetToFullString(ex:Example, widget:Widget) = {
      if (opts.fullPredForEval) ex.widgetToEvalFullString(widget)
      else                      ex.widgetToNiceFullString(widget)
    }

    def exampleToString(ex:Example) = ex.name + ": " +
      ex.events(0) + " ..." + " ||| " + getObject(wordIndexer, ex.text).mkString(" ")

    def newPerformance = new Performance
    def genExample = null

    override def readExamples(path:String, maxExamples:Int, add:(Example=>Any)) = {
	  val eventsExt = opts.inputFileExt
      val eventsPath = path
      val textPath = path.replaceAll("\\."+eventsExt, ".text")
      val alignPath = path.replaceAll("\\."+eventsExt, ".align")
      val alignPathExists = new java.io.File(alignPath).exists

      if (!opts.useOnlyLabeledExamples || alignPathExists) {
        val excludedFields = new HashSet[String]
        opts.excludedFields.foreach { s => excludedFields += s }
        val excludedEventTypes = new HashSet[String]
        opts.excludedEventTypes.foreach { s => excludedEventTypes += s }

        // Read events
        case class Token(tchar:Char, fieldName:String, role:Int, value:String)
        val seenEventTypes = new HashSet[Int]
        val events = Utils.createArray({ add:(Event=>Any) => Utils.foreachLine(eventsPath, { line:String =>
          // Format: <fieldtype><field>:<value>\t...
          var t = -1 // Event type
          val tokens = Utils.createArray({ (add:Token => Any) =>
            line.split("\\t").foreach { token:String => // token @
              val i = token.indexOf(':', 1)
              if (i == -1) throw fail("Bad token: " + token)
              val tchar = token(0)
              val fieldName = token.substring(1, i)
              val value = token.substring(i+1)
              if (fieldName == "type") {
                if (!excludedEventTypes(value)) {
                  t = eventTypeNameIndexer.getIndex(value)
                  if (opts.includeEventTypeAsSymbol) add(Token(':', "type", -1, value))
                }
              }
              else if (tchar != '.' && // Ignore hidden fields
                  t != -1) { // Consider only fields that come after type
                val fullFieldName = eventTypeNameIndexer.getObject(t)+"."+fieldName 
                if (!excludedFields(fullFieldName)) { // Ignore excluded fields
                  if (tchar == '$' && opts.useWordRolesOnFields.contains(fullFieldName)) // Need to expand into the roles
                    foreach(numWordRoles, { role:Int => add(Token(tchar, fieldName+"-"+wordRoleNameIndexer.getObject(role), role, value)) })
                  else
                    add(Token(tchar, fieldName, -1, value))
                }
              }
            }
          })
          //if (t == -1) throw fails("Missing .type field: %s", line)
          // If t == -1, that means we've decided to exclude this event type

          if (t != -1 && (!opts.takeOneOfEventType || !seenEventTypes(t))) {
            seenEventTypes += t
            // Set up event type
            if (t == eventTypesBuffer.size) { // New event type
              eventTypesBuffer += new EventType(t, eventTypeNameIndexer.getObject(t),
                tokens.map { token:Token =>
                  token.tchar match {
                    case '#' => new NumField(token.fieldName)
                    case '@' => if (opts.treatCatAsSym) new SymField(token.fieldName) else new CatField(token.fieldName)
                    case ':' => new SymField(token.fieldName)
                    case '$' => new StrField(token.fieldName)
                    case _ => throw fail("Bad field name: " + token.fieldName)
                  }
                })
            }
            else { // Check that the fields are the same
              val eventType = eventTypesBuffer(t)
              foreach(Utils.same(eventType.F, tokens.size), { f:Int =>
                if (eventType.fstr(f) != tokens(f).fieldName)
                  throw fails("Field names don't match for event type %s: %s versus %s", eventType, eventType.fstr(f), tokens(f).fieldName)
              })
            }

            // Create the event with its values
            val eventType = eventTypesBuffer(t)
            add(new Event(t, map(eventType.F, { f:Int =>
              eventType.fields(f).parseValue(tokens(f).role, tokens(f).value)
            })))
          }
          true
        }) })

        // Read text
        val lineToStartText = new ArrayBuffer[Int]
        var lineIdx = 0
        var textIdx = 0
        val textStr = Utils.createArray({ add:(String => Any) => 
          Utils.foreachLine(textPath, { line:String =>
            lineToStartText += textIdx
            line.toLowerCase.split(" ").foreach { s =>
              add(s)
              textIdx += 1
            }
            lineIdx += 1
            true
          })
        })
        lineToStartText += textIdx
        val text = textStr.map(getWordIndex(_))
        val startIndices = lineToStartText.toArray
        val E = events.size
        val N = text.size
        val labels = map(N, { i:Int => getLabelIndex(textStr, i) })

        // Read alignments
        if (alignPathExists) {
          val trueEventsBuffer = map(N, { _:Int => new ArrayBuffer[Int] })
          var maxTracks = 0
          Utils.foreachLine(alignPath, { line:String =>
            // Format: <line index> <event id1> ... <event idn>
            val lineEvents = line.split(" ").map(_.toInt)
            val lineIdx = lineEvents(0)
            val alignedEvents = lineEvents.subArray(1, lineEvents.size) // List of events
            // This line goes from positions i to j
            val i = lineToStartText(lineIdx)
            val j = lineToStartText(lineIdx+1)
            maxTracks = maxTracks max alignedEvents.size

            alignedEvents.map { e:Int =>
              // -1 means that this line corresponds to an event that's not in the candidate set, so we automatically get it wrong
              if (e == -1) unreachable_e
              else e
            }.foreach { e:Int =>
              assert ((e >= 0 && e < E) || e == unreachable_e)
              foreach(i, j, { k:Int => trueEventsBuffer(k) += e })
            }
            true
          })
          val trueEvents = map(maxTracks, { c:Int =>
            map(N, { i:Int =>
              if (c < trueEventsBuffer(i).size) trueEventsBuffer(i)(c) else none_e
            })
          })

          if (opts.oneExamplePerLine) {
            foreach(startIndices.size-1, { l:Int =>
              val i = startIndices(l)
              val j = startIndices(l+1)
              val subTrueEvents = map(maxTracks, { c:Int => trueEvents(c).subArray(i, j) })
              val subStartIndices = Array(0, j-i)
              add(new Example(textPath+":"+l, events, text.subArray(i, j), labels.subArray(i, j), subStartIndices,
                new Widget(subTrueEvents, null, null, null, subStartIndices, { e:Int => events(e).t })))
            })
          }
          else {
            add(new Example(textPath, events, text, labels, startIndices,
              new Widget(trueEvents, null, null, null, startIndices, { e:Int => events(e).t })))
          }
        }
        else {
          if (opts.oneExamplePerLine) { // Originally for NFL data, but now with cooked.pruned, don't need this anymore
            foreach(startIndices.size-1, { l:Int =>
              val i = startIndices(l)
              val j = startIndices(l+1)
              val subStartIndices = Array(0, j-i)
              add(new Example(textPath+":"+l, events, text.subArray(i, j), labels.subArray(i, j), subStartIndices, null))
            })
          }
          else {
            add(new Example(textPath, events, text, labels, startIndices, null))
          }
        }
      }
    }

    override def readExamples = {
      if (useWordRoles) readWordRoles

      super.readExamples

      eventTypes = eventTypesBuffer.toArray
      eventTypeAllowedOnTrack = map(C, { c:Int =>
        def getSet(str:String) = {
          val set = new HashSet[Int]
          if (str.size > 0) {
            str.split(",").foreach { s:String =>
              if (s == "ALL") // Everything
                foreach(T, { t:Int => set += t })
              else {
                val t = eventTypeNameIndexer.indexOf(s)
                if (t == -1) warnings("Invalid event type specified in eventTypeTracks: '%s'", s)
                else set += t
              }
            }
          }
          set
        }
         
        // Format of this string: <positive type name>,...,<positive type name>/<negative type name>,...,<negative type name>
        // type name = * means all types
        val posNegStr = opts.eventTypeTracks(c).split("/")
        val posSet = getSet(posNegStr(0))
        val negSet = getSet(if (posNegStr.size > 1) posNegStr(1) else "")
        negSet.foreach { t:Int => posSet -= t }
        posSet
      })
      Utils.track_printAll("C=%s tracks", C) {
        foreach(C, { c:Int => logs("Track %s (%s event types)", cstr(c), eventTypeAllowedOnTrack(c).size) })
      }
      track("Setting up examples") {
        examples.foreach { ex:Example =>
          ex.computeEventTypeCounts
          ex.computeTrackEvents
        }
      }

    }

    override def baitInitParams = { // Hard code things
      params = newParams
      params.setUniform_!(1)

      // Not used anymore
      val t = eventTypeNameIndexer.indexOf("windDir")
      val f = 0
      val tparams = params.eventTypeParams(t)
      eventTypes(t).fields(f) match {
        case field:CatField =>
          tparams.fieldParams(f) match {
            case fparams:CatFieldParams =>
              val x = 100
              fparams.emissions(field.indexer.indexOf("N")).addCount_!(wordIndexer.indexOf("north"), x)
              fparams.emissions(field.indexer.indexOf("S")).addCount_!(wordIndexer.indexOf("south"), x)
              fparams.emissions(field.indexer.indexOf("E")).addCount_!(wordIndexer.indexOf("east"), x)
              fparams.emissions(field.indexer.indexOf("W")).addCount_!(wordIndexer.indexOf("west"), x)
            case _ => impossible
          }
        case _ => impossible
      }
      params.optimize_!(opts.initSmoothing)
    }

    def newInferState(ex:Example, params:Params, counts:Params, ispec:InferSpec) = new InferState(ex, params, counts, ispec)
    def newParams = new Params


	
	def genWordFromExample(ex:Example, c:Int, e:Int, f:Int) = {
		//--Utility Functions
		def getv(e:Int, f:Int) = ex.events(e).values(f)
		def get_tparams(e:Int) = params.eventTypeParams(ex.events(e).t)
		def get_num_fparams(e:Int, f:Int) = get_tparams(e).fieldParams(f) match { case p:NumFieldParams => p; case _ => throw impossible }
		def get_cat_fparams(e:Int, f:Int) = get_tparams(e).fieldParams(f) match { case p:CatFieldParams => p; case _ => throw impossible }
		def get_sym_fparams(e:Int, f:Int) = get_tparams(e).fieldParams(f) match { case p:SymFieldParams => p; case _ => throw impossible }
		def get_str_fparams(e:Int, f:Int) = get_tparams(e).fieldParams(f) match { case p:StrFieldParams => p; case _ => throw impossible }	
		
		//--Field Generation
		def genNum(probs:ProbVec, c:Int, e:Int, f:Int) : ProbVec = {
			val rtn:ProbVec = ProbVec.zeros(probs.size)
			val v = getv(e, f)
			val fparams = get_num_fparams(e, f)

			foreach(W, (w:Int) => {
				val num = str2num(wstr(w))
				if(num != NaN) {
					//--Try to generate this number
					//(identity)
					if(v == num) {
						rtn.addCount_!(w, fparams.methodChoices.getProb(M_IDENTITY)	)
					}
					//(round up)
					if(roundUp(v) == num) {
						rtn.addCount_!(w, fparams.methodChoices.getProb(M_ROUNDUP) )
					}
					//(round down)
					if(roundDown(v) == num) {
						rtn.addCount_!(w, fparams.methodChoices.getProb(M_ROUNDDOWN) )
					}
					//(round close)
					if(roundClose(v) == num) {
						rtn.addCount_!(w, fparams.methodChoices.getProb(M_ROUNDCLOSE) )
					}
					//(noise)
					val noise:Double = (num - v).doubleValue;
					if(noise > 0) {
						//(noise up)
						val count = fparams.methodChoices.getProb(M_NOISEUP) * 
							0.5 *
							Math.pow( fparams.rightNoiseChoices.getProb(S_CONTINUE), (noise-1.0) ) *
							fparams.rightNoiseChoices.getProb(S_STOP) 
						rtn.addCount_!(w, count)
						if(count.toString.equals("NaN") || rtn.getCount(w) < 0 || count.toString.equals("Infinity")) 
							throw fails("genNum: Product of these is not a number: " +
							fparams.methodChoices.getProb(M_NOISEUP) + " " +
							Math.pow(fparams.rightNoiseChoices.getProb(S_CONTINUE), (noise-1.0)) + " " +
							fparams.rightNoiseChoices.getProb(S_STOP))
					}else if(noise < 0){
						//(noise down)
						val count = fparams.methodChoices.getProb(M_NOISEDOWN) *
							0.5 *
							Math.pow( fparams.leftNoiseChoices.getProb(S_CONTINUE), -noise-1.0 ) *
							fparams.leftNoiseChoices.getProb(S_STOP) 
						rtn.addCount_!(w, count)
						if(count.toString.equals("NaN") || rtn.getCount(w) < 0 || count.toString.equals("Infinity")) 
							throw fails("genNum: Product of these is not valid: " +
							fparams.methodChoices.getProb(M_NOISEUP) + " " +
							Math.pow(fparams.leftNoiseChoices.getProb(S_CONTINUE), (-noise-1.0)) + " " +
							fparams.leftNoiseChoices.getProb(S_STOP) + "  " +noise)
					}
				}
				if(rtn.getCount(w).toString.equals("NaN")) throw fails("Number prob is NaN")
			})
			rtn
		}


		def genCat(probs:ProbVec, c:Int, e:Int, f:Int):ProbVec = {
			val rtn:ProbVec = ProbVec.zeros(probs.size)
			val v = getv(e, f)
			val fparams = get_cat_fparams(e, f)
			
			foreach(W, (w:Int) => {
				rtn.addCount_!(w, fparams.emissions(v).getProb(w))
			})
			rtn
		}
	
/*
		def genSym(probs:ProbVec, c:Int, e:Int, f:Int):ProbVec = {
			val rtn:ProbVec = probs.copy
			val v = getv(e, f)
			val fparams = get_sym_fparams(e, f)
			
			foreach(W, (w:Int) => {
				if(w == v && (genLabels || prevGenLabels)){
					rtn.addCount_!(w, fparams.labelChoices.getProb(labels(i)) /
						fparams.genericLabelChoices.getProb(labels(i))  )
				}
			})
			rtn
		}
*/		

		def genStr(probs:ProbVec, c:Int, e:Int, f:Int):ProbVec = {
			val rtn:ProbVec = ProbVec.zeros(probs.size)
			val (v_words, v_labels) = ex.events(e).fields(f) match {
					case field:StrField => field.indexer.getObject( getv(e, f) )
					case _ => throw impossible
			}
			val fparams = get_str_fparams(e, f)
			
			foreach(W, (w:Int) => {
				if( v_words.contains(w) ){
					foreach(Utils.same(v_words.size, v_labels.size), (v_i:Int) => {
						if(v_words(v_i) == w){
							rtn.addCount_!(w, 1.0/v_words.size) //TODO Ignores label params
						}
					})
					rtn.addCount_!(w, 1.0) 
				}
			})
			rtn
		}
		
		//--Generate Field
		def genField(probs:ProbVec, c:Int, e:Int, f:Int):ProbVec = {
			val tParams = get_tparams(e)
			if(f == tParams.boundary_f) {
				throw fail("Cannot generate word from boundary_f")
			} else if(f == tParams.none_f) {
				throw fail("Should not reach here")
			} else {
				val fields = ex.events(e).fields
				if(f >= fields.size){
					println(">>> " + f + " " + fields.size + " " + tParams.none_f + " " + tParams.boundary_f)
				}
				fields(f) match {
					case _:NumField => genNum(probs,c,e,f)
					case _:CatField => genCat(probs,c,e,f)
					case _:SymField => throw fail("Sym fields not implemented")
					case _:StrField => genStr(probs,c,e,f)
					case _ => throw impossible
				}
			}
		}
		
		//--Generate Word
		def genEventTypeGivenWord(t:Int, w:Int) = {
			val rtn = if(opts.includeEventTypeGivenWord) params.eventTypeChoicesGivenWord(w).getProb(t) else 1.0
			if(rtn.toString.equals("NaN")) throw fails("Event Type Given Word is NaN")
			rtn
		}
		
		
		import ProbVec.{zeros}
		val rtn:ProbVec = zeros(W)
		val t = ex.events(e).t
		val tparams = get_tparams(e)
		val fieldProbs:ProbVec = 
			if(f != tparams.none_f) genField(rtn, c, e, f) //warning: relies on functional programming
			else zeros(W)
		
		foreach(W, (w:Int) => {
			if(f==tparams.none_f){
				//Generate From Event
				rtn.addCount_!(w, tparams.noneFieldEmissions.getProb(w) *
					genEventTypeGivenWord(t, w)  )
			}else{
				//Generate From Field Name
				rtn.addCount_!(w, tparams.genChoices(f).getProb(G_FIELD_NAME) *
					tparams.fieldNameEmissions(f).getProb(w) )
				if(rtn.getCount(w).toString.equals("NaN")) throw fails("Word probability is NaN in genWord")
				//Generate From Field Value
				rtn.addCount_!(w, tparams.genChoices(f).getProb(G_FIELD_VALUE) *
					genEventTypeGivenWord(t,w) *
					fieldProbs.getProb(w)  )
				if(fieldProbs.getCount(w).toString.equals("NaN")) throw fails("Word probability is NaN in genWord")
				//Generate From Field Generic
				rtn.addCount_!(w, tparams.genChoices(f).getProb(G_FIELD_GENERIC) *
					params.genericEmissions.getProb(w) *
					genEventTypeGivenWord(t,w)  )
				if(rtn.getCount(w).toString.equals("NaN")) throw fails("Word probability is NaN in genWord")
			}
		})
		if(rtn.totalCount == 0 || (rtn.totalCount.toString.equals("NaN")))
			throw fail("Word has no probability contributions")
		rtn
	}
	
	def genFromExample(lm:generation.LanguageModel, ex:Example):List[Int] = {
		//(values)
		val P:Int = opts.numParticles 
		val E = ex.events.size
		val none_e = E
		val stop_e = E+1
		def none_f(e:Int) = if(e==none_e) -1 else eventTypes(ex.events(e).t).none_f
		def stop_f(e:Int) = if(e==none_e) -1 else (eventTypes(ex.events(e).t).F+2)
		val stop_w = W
		//(utility functions)
		def eTot(e:Int) = if(e==none_e) none_t else ex.events(e).t
		def get_tparams(e:Int) = params.eventTypeParams(ex.events(e).t)
	
		//(particle definition)
		def isTerminated(part:Particle):Boolean = {part.event == stop_e}
		case class Particle(event:Int, field:Int, words:List[Int], weight:Double) {
			var events = List[Int]()
			var fields = List[Int]()
		}

		//(caching)
		val wordChoiceCache = new java.util.HashMap[(Int,Int,Int), ProbVec]
		val fieldChoiceCache = new java.util.HashMap[(Int,Int,Int), ProbVec]
		val eventChoiceCache = new java.util.HashMap[(Int,Int), ProbVec]
		
		def genChild(part:Particle):Particle = {
			//--Variables
			//(particle specific)
			val lastE:Int = part.event
			val lastF:Int = part.field
			val history:List[Int] = part.words
			val weight:Double = part.weight
			//val lmProb:ProbVec = lm.nextWordDist(history, wordIndexer) 
			//(params)
			val lparams = params.lengthParams
			//(special values)
			val c = 0
			//(error checking)
			if(lastE == stop_e) throw fails("The last event is the STOP special symbol")
			
			//--Generation Methods
			//chose a new word from this field and event
			def choseWord(c:Int, e:Int, f:Int, allowStop:Boolean):(Int,Double) = {
				if(e == stop_e) {
					throw fails("Generating word from STOP event")
				}else if(f == stop_f(e)) {
					(stop_w, -1.0)
				} else {
					val key = (c, e, f)
					val wordChoice:ProbVec = {
						if( wordChoiceCache.containsKey(key) ){ wordChoiceCache.get( key ) }
						else{
						val wordProb:ProbVec = 
							if(e == none_e) params.trackParams(c).noneEventTypeEmissions
							else genWordFromExample(ex, c, e, f)
						val wc:ProbVec = ProbVec.zeros(W+1)
						foreach(W, (w:Int) => {
							wc.setCount_!(w, wordProb.getProb(w))
							//wc.setCount_!(w,
							//	Math.pow( wordProb.getProb(w), opts.semWeight ) *
							//	Math.pow( lmProb.getProb(w), opts.linWeight ) )
						})
						wordChoiceCache.put( key, wc )
						wc
						}	
					}
					//(stop prob)
					val stopProb = 
						if(e == none_e) lparams.noEventWordContinue.getProb(S_STOP)
						else lparams.wordContinue(eTot(e))(f).getProb(S_STOP)
					if(allowStop){
						wordChoice.setCountToObtainProb_!(stop_w, stopProb)
					}else{
						wordChoice.setCountToObtainProb_!(stop_w, 0.0)
					}
					//(sample from the word choice vector)
					val (word, wProb) = wordChoice.samplePair(lm.rand)
					//println("\t" + {if(word==stop_w) "(stop)" else wstr(word)} + "   " + wProb)
					(word, wProb)
				}
			}
			//chose a new field, and then a new word from that field
			def choseField(c:Int, e:Int, lastF:Int, allowStop:Boolean):(Int,Int,Double) = {
				if(e == none_e) {
					//Cannot change fields in none_event
					if(!allowStop) throw fails("We cannot change fields in the none event")
					else (stop_w, stop_f(e), -1.0)
				} else if(e == stop_e) {
					throw fails("Cannot chose a field in stop_e")
				} else {
					//(variables)
					val lparams = params.lengthParams
					val tparams = get_tparams(e)
					val t = eTot(e)
					//(set up next field distribution)
					val fieldChoice = {
						if( fieldChoiceCache.containsKey((c,e,lastF)) ) fieldChoiceCache.get( (c,e,lastF) )
						else{
						val fieldProb:ProbVec = tparams.fieldChoices(lastF)
						val fc:ProbVec = ProbVec.zeros(ex.events(e).F+3)
						foreach(fieldProb.size, (f:Int) => {
							fc.setCount_!(f, fieldProb.getProb(f))
						})
						fc.setCount_!(tparams.boundary_f, 0.0) //We never want to chose the boundary field
						//fc.setCount_!(tparams.none_f, 0.0) //Comment to allow none_f
						fieldChoiceCache.put( (c,e,lastF), fc )
						fc
						}
					}
					//(stop prob)
					if(allowStop){
						fieldChoice.setCountToObtainProb_!(stop_f(e), lparams.fieldContinue(t).getProb(S_STOP))
					}else{
						fieldChoice.setCountToObtainProb_!(stop_f(e), 0.0)
					}
					//(sample next field)
					val (field, fProb) = fieldChoice.samplePair(lm.rand)
					if(field == stop_f(e)) (stop_w, stop_f(e), -1.0)		//CASE: don't generate more fields from event
					else {
						//we've chosen a new field
						val (word, wProb) = choseWord(c, e, field, false)
						if(word == stop_w) choseField(c, e, field, true)	//CASE: don't generate any words from new field
								//^impossible
						else (word, field, wProb*fProb)						//CASE: generate word from new field
					}
				}
			}
			//chose a new event, and then chose a new field and word from that event
			def choseEvent(c:Int, lastE:Int, allowStop:Boolean):(Int,Int,Int,Double) = {
				//println("      GENERATING EVENT")
				//(variables)
				val lparams = params.lengthParams
				val cparams = params.trackParams(c)
				//(set up the next event distribution)
				val eventChoice = {
					if(eventChoiceCache.containsKey((c,lastE)) ) {eventChoiceCache.get((c,lastE))}
					else{
					val eventTypeProb:ProbVec = cparams.eventTypeChoices(eTot(lastE))
					val ec:ProbVec = ProbVec.zeros(E+2)
					foreach(E, (e:Int) => {
						val t = eTot(e)
						ec.setCount_!(e, eventTypeProb.getProb(t))
					})
					//TODO None Event
					ec.setCount_!(none_e, 0.0)	//Comment to allow none_event
					if(allowStop) ec.setCountToObtainProb_!(stop_e, lparams.eventContinue.getProb(S_STOP))
					eventChoiceCache.put( (c,lastE), ec )
					ec
					}
				}
				//(stop event)
				if(allowStop){
					eventChoice.setCountToObtainProb_!(stop_e, lparams.eventContinue.getProb(S_STOP))
				}else{
					eventChoice.setCountToObtainProb_!(stop_e, 0.0)
				}
				//(sample next word)
				val (e, eProb) = eventChoice.samplePair(lm.rand)
				if(e == stop_e) (stop_w, -1, stop_e, -1.0)			//CASE: don't generate more events
				else{
					val tparams = params.eventTypeParams(eTot(e))
					val(word, field, fProb) = choseField(c, e, tparams.boundary_f, false)
					if(field == stop_f(e)) choseEvent(c, e, true)	//CASE: don't generate any fields from new event
							//^impossible
					else (word, field, e, fProb*eProb)				//CASE: generate word from new event
				}
			}
			
			//--Generate Word
			//(generate from semantic model)
			var (word, wProb) = (stop_w, -1.0)
			var field = lastF 
			var event = lastE
			val (w, wP) = choseWord(c, lastE, lastF, true)							//CASE: keep event/field
			word = w; wProb = wP;
			if(word == stop_w){		//no more words to generate from field
				val (w, f, wP) = choseField(c, lastE, lastF, true)					//CASE: keep event, change field
				word=w; field=f; wProb=wP;
				if(field == stop_f(lastE)){		//no more fields to generate from event
					val (w, f, e, wP):(Int,Int,Int,Double) 
						= choseEvent(c, lastE, !history.isEmpty)					//CASE: change event, field
					word=w; field=f; event=e; wProb=wP;		//event might be stop_e
				}
			}
			//(error check)
			if(event!=stop_e){
				if(field==stop_f(event)) throw fails("Returning a STOP Field")
				if(word==stop_w) throw fails("Returning a STOP Word")
			}
			//(check with linguistic model)
			val newProb:Double = 
				if(event==stop_e) 1.0
				else Math.pow(wProb, opts.semWeight) * Math.pow(lm.nextWordProb(word,history,wordIndexer), opts.linWeight)			
			val rtn = new Particle(event, 
				field, 
				{if(event==stop_e) history else history ::: List(word)}, 
				newProb)
			rtn.fields = {if(event==stop_e) part.fields else part.fields ::: List(field)}
			rtn.events = {if(event==stop_e) part.events else part.events ::: List(event)}
			rtn
		}

		def sortByTerminated(parts:Array[Particle]):(Int,Array[Particle],ProbVec) = {
			var start:Int = 0
			var i:Int = 0
			val rtnDist:ProbVec = ProbVec.zeros(parts.size)
			val rtnParts:Array[Particle] = new Array[Particle](parts.size)
			//--Filter out the terminated
			foreach(parts.size, (p:Int) => {
				if( isTerminated(parts(p)) ){
					rtnParts(start) = parts(p)
					rtnDist.setCount_!(start, 0.0)
					start += 1
				}
			})
			//--Filter the nonTerminated
			i = start
			foreach(parts.size, (p:Int) => {
				if( !isTerminated(parts(p)) ){
					rtnParts(i) = parts(p)
					if(parts(p).weight < 0) throw fails("Particle has negative weight")
					rtnDist.addCount_!(i, parts(p).weight)
					i += 1
				}
			})
			if(i < parts.size) throw fails("i is less than parts.size")
			return (start, rtnParts, rtnDist)
		}
	
		//TODO don't need initDist
		def growParticles(initDist:ProbVec, initParts:Array[Particle], depth:Int):(ProbVec,Array[Particle]) = {
			//--Base Case
			val nonTerminated:Int = initParts.filter( !isTerminated(_) ).size
			logs(depth + ": \t" +  nonTerminated + " particles left in pool")
			if(nonTerminated == 0) (initDist, initParts)
			else {
				//--Generate Children
				
				val (start, genParts, genDist) =	// genDist has 0 prob for terminated elements
					sortByTerminated( initParts.map( (part:Particle) => {
							if(isTerminated(part)){
								val rtn = new Particle(part.event, part.field, part.words, -1.0)
								rtn.events = part.events
								rtn.fields = part.fields
								rtn
							}else{
								genChild(part)
							}
						}) 
					)
				/*
				val genParts =
					initParts.map( (part:Particle) => {
						if(isTerminated(part)){
							val rtn = new Particle(part.event, part.field, part.words, 1.0)
							rtn.events = part.events
							rtn.fields = part.fields
							rtn
						}else{
							val p = genChild(part)
							p
						}
					})
				val genDist = ProbVec.zeros(P)
				foreach(P, (p:Int) => {genDist.addCount_!(p, genParts(p).weight)})
				*/
				//--Resample
				
				val children:Array[Int] = genDist.sampleN(lm.rand, P-start)	//sample over indexes in genDist
				val resampParts:Array[Particle] = new Array[Particle](P)
				foreach(P, (p:Int) => { 
					if(p < start) resampParts(p) = genParts(p) 
					else resampParts(p) = genParts(children(p-start)) 
				})
				val resampDist:ProbVec = ProbVec.zeros(P)
				foreach(P, (p:Int) => {resampDist.setCount_!(p, resampParts(p).weight) } )
				/*
				val children:Array[Int] = genDist.sampleN(lm.rand, P)
				val resampParts:Array[Particle] = new Array[Particle](P)
				foreach(P, (p:Int) => {
					resampParts(p) = genParts(children(p))
				})
				val resampDist:ProbVec = ProbVec.zeros(P)
				foreach(P, (p:Int) => {resampDist.setCount_!(p, resampParts(p).weight) } )
				*/
				//--Recurse
				growParticles(resampDist, resampParts, depth+1)
			}
		}
		
		//--Start the generation
		//(initial variables)
		val initDist:ProbVec = ProbVec.zeros(P)
		val initParts:Array[Particle] = new Array[Particle](P)
		foreach(P, (p:Int) => { 
				initParts(p) = new Particle(none_e, stop_f(none_e), List[Int](), initDist.getProb(p)) 
		})
		initDist.addCount_!(1.0)
		//(generate)
		val (probs:ProbVec, parts:Array[Particle]) = growParticles(initDist, initParts, 0)
		val part = probs.sampleUniform(lm.rand)
		
		//(debug)
		println("Manual Sample")
		foreach(1, (i:Int) => {
				val part:Particle = parts(probs.sampleUniform(lm.rand))
				if(part.events.size != part.words.size) throw fails ("Did not keep track of events: " + part.events.size + " " + part.words.size)
				foreach(part.words.size, (i:Int) => {
					if(part.events(i) == stop_e) throw fails("Found STOP_E Event")
					var t = eTot(part.events(i))
					print(wstr(part.words(i)) + "\t")
					var f = part.fields(i)
					if(f == stop_f(part.events(i))) throw fails("FOUND STOP_F Field")
					print( {if(f>=0) {if(t>=0) get_tparams(part.events(i)).fstr(part.fields(i)) else "(none)"} else "(none)"}+ "\t")
					println( {if(t>=0) tstr(t) else "(none)"})
				})
		})
		println("\nOther Generations")
		foreach(10, (i:Int) => {println(parts(probs.sampleUniform(lm.rand)).words.map(wstr(_)))})
		println("")

		parts(part).words
	}
	

	override def generate() : Array[List[String]] = {
		val rtn:Array[List[String]] = new Array[List[String]](opts.lmNumGenerated)
		val deterministic = opts.deterministic
		val wi = new Indexer[String]
		//foreach(W, (w:Int) => {
		//	val word:String = wordIndexer.getObject(w)
		//	if( wi.getIndex(word) != w ) throw fails("Internal Error")
		//})
		val lm:generation.LanguageModel = opts.lm match {
			case Options.LanguageModel.simple => new generation.LanguageModel(opts, opts.numGrams, wi)
			case Options.LanguageModel.kneserney => new generation.KneserNeyLM(opts, opts.numGrams, wi)
			case Options.LanguageModel.event3 => new generation.Event3LM(opts, opts.numGrams, wi)
			case _ => fail("Unknown language model type: " + opts.lm); null
		}
		logs("Reading LM Data")
		lm.readData
		logs("Training LM")
		lm.train
		lm.interactive

		foreach(rtn.size, (i:Int) => rtn(i) = if(i < examples.size) {
				val gen = track("Example " + i){
					genFromExample(lm, examples(i)).map( (w:Int) => wstr(w)) 
				} 
				end_track
				gen
			} else {
				List() 
			})
		rtn
	}

	override def interactive:Unit = {
		val invalid_e = -1
		val invalid_t = -2
		val invalid_f = -3
		def readLine(prompt:String):String = {
			print(prompt)
			val rtn = (new java.io.BufferedReader(new java.io.InputStreamReader(System.in))).readLine()
			rtn
		}
		def genFromEx(exNum:Int) = {
			val ex = examples(exNum)
			val none_e = ex.events.size
			def none_f(e:Int) = eventTypes(ex.events(e).t).none_f
			def strToEvent(str:String) = {
				def getT(i:Int):Int = {
					if( i >= eventTypes.size) invalid_t
					else if( eventTypes(i).name equalsIgnoreCase str ) i
					else getT(i+1)
				}
				def getE(t:Int, i:Int):Int = {
					if(t == invalid_t)	invalid_e
					else if(i >= ex.events.size ) invalid_e
					else if(ex.events(i).t == t) i
					else getE(t, i+1)
				}
				getE(getT(0), 0)
			}
			def strToField(str:String, e:Int) = {
				val t = ex.events(e).t
				val fields = eventTypes(t).fields
				def getF(i:Int):Int = {
					if( i >= fields.size ) invalid_f
					else if( fields(i).name equalsIgnoreCase str ) i
					else getF(i+1)
				}
				getF(0)
			}
			println("EXAMPLE " + exNum)
			var input = readLine("sm> ")
			while(!input.equalsIgnoreCase("exit")){
				//(read input)
				val tokens = new java.util.StringTokenizer(input)	
				val event = if(tokens.hasMoreTokens) strToEvent(tokens.nextToken) else none_e
				val field =
					if(event==none_e || event==invalid_e) invalid_f
					else if(tokens.hasMoreTokens) strToField(tokens.nextToken, event)
					else none_f(event)
				if(event == invalid_e) println("   Invalid event string")
				else if(event!=none_e && field == invalid_e) println("   Invalid field string")
				else{
					println("   Generating From: " + tstr(ex.events(event).t) + "\t" +
						eventTypes(ex.events(event).t).fstr(field) + "\t" +
						{if(field==none_f(event)) "---" 
						 else ex.events(event).fields(field).vstr(ex.events(event).values(field)) 
						})
					//(print most likely words)
					val wordDist:ProbVec = genWordFromExample(ex, 0, event, field)
					val df = new java.text.DecimalFormat("0.00000")
					var countRead:Double = 0.0
					println("   P(w | e, f, v)")
					foreach(5, (i:Int) => {
						val (w,prob) = wordDist.maxPair
						println("      " + 
							df.format(wordDist.getCount(w)/(wordDist.totalCount+countRead)) + "\t" + 
							wstr(w))
						countRead += wordDist.getCount(w)
						wordDist.setCount_!(w, 0.0)
					})
				}
				input = readLine("sm> ")
			}
		}

		println( "Interactive Semantics Model" )
		println("enter an example index or 'exit'")
		var input:String = readLine("ex> ")
		while(!input.equalsIgnoreCase("exit")){
			try{
				val exNum = Integer.parseInt(input)
				genFromEx(exNum)
			} catch {
				case (nfe:NumberFormatException) => println("   invalid example number")
				case (e:Exception) => e.printStackTrace
			}
			println("")
			println("enter an example index or 'exit'")
			input = readLine("ex> ")
		}
	}


    override def saveParams(filename:String):Unit = {
	  logs("Saving Parameters")
	  val objStream:ObjectOutputStream = new ObjectOutputStream(new FileOutputStream(filename))
	  objStream.writeObject(params)
	  objStream.close
    }
  
    override def loadParams(filename:String):Unit = {
      logs("Loading Parameters");
	  val objStream:ObjectInputStream = new ObjectInputStream(new FileInputStream(filename))
      this.params = objStream.readObject().asInstanceOf[Params]
      objStream.close()
    }

	def output(filename:String):Unit = {
		println("hello")
	}

    
     // case class Example(name:String, events:Array[Event], text:Array[Int], labels:Array[Int], startIndices:Array[Int], trueWidget:Widget) extends AExample[Widget] {
    
  }

  def newModel = new Model
}
