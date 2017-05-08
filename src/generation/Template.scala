package generation

import AlignedDataset.AlignedSentence
import induction.Event3Problem
import tea.Utils.{foreach}
import fig.basic.LogInfo.{logs,dbg,fail,error,track,end_track}
import org.goobs.foreign.Counter

import scala.collection.mutable.HashSet
import scala.collection.mutable.PriorityQueue
import scala.util.matching.Regex
import java.util.Iterator
import java.util.HashMap


/**
 *	t			the event type this template belongs to
 *	fields		the fields to be filled
 *	wordDists	the word distributions for each field
 */
class Template(
		t:Int, 
		fields:Array[Int],
		wordDists:Array[Counter[Array[String]]]
		){

	var parents:HashSet[Template] = new HashSet[Template]()
	var fieldSet:FieldSet = null
	
	def getT = t
	def getFields = fields
	def getWordDists = wordDists


	var templateWordsCache:Array[String] = null
	def templateWords:Array[String] = {
		if(templateWordsCache == null){
			var lst:List[String] = List()
			for( i <- 0 until fields.length){
				val f:Int = fields(i)
				if(f == GenUtils.none_f(t)){
					wordDists(i).argMax().foreach( (s:String) => {
						lst = s :: lst
					})
				}
			}
			templateWordsCache = lst.reverse.toArray
		}
		return templateWordsCache
	}
	
	/**
	*	Returns the first word of this template
	*	If the first word is a field, it returns the first word of what
	*	that field would fill to.
	*	@param e The event index the template is from
	*	@param ws The world state we would be generating from
	*/
	def firstWord(e:Int,ws:WorldState):String = {
		if(fields(0) != GenUtils.none_f(t)){
			GenUtils.emissions(t,fields(0),ws.events(e).fields(fields(0)).value).argMax
		} else { 
			wordDists(0).argMax()(0)
		}
	}

	def fill(e:Int, ws:WorldState):(Array[Int],Array[String]) = {
		fill(e,ws, (a:Array[String]) => 1.0)
	}

	def fill(e:Int, ws:WorldState, lm:Array[String]=>Double):(Array[Int],Array[String]) = {
		if(e != GenUtils.none_e(ws) && GenUtils.e2t(e,ws) != t){
			throw new IllegalArgumentException("e and t do not match: e=" 
				+ e + " t_e=" + GenUtils.tstr( GenUtils.e2t(e,ws) ) + " t=" + GenUtils.tstr(t) )
		}
		if(t == GenUtils.none_t){
			if(fields.length > 1) throw new IllegalStateException("none_t has multiple fields!")
			val wRtn:Array[String] = inferWords(wordDists(0))
			(Array.make(wRtn.length,GenUtils.none_f(t)), wRtn )
		}else {
			var wRtn:Array[String] = new Array[String](0)
			var fRtn:Array[Int] = new Array[Int](0)
			foreach(fields.length, (i:Int) => {
				val f = fields(i)
				//(fill special terms function)
				val fill:Array[String]=>Array[String] = (out:Array[String]) => {
					out.map( (str:String) => {
						if(str == Template.NUM_STR){
							if(t == GenUtils.none_t || f == GenUtils.none_f(t) || !ws.events(e).fields(f).isNumeric)
								throw new IllegalStateException("NUM term in non-numeric field: "
									+ GenUtils.tstr(t) + "." + GenUtils.fstr(t,f))
							val str = GenUtils.emissions(t,f,ws.events(e).fields(f).value).argMax
							str
						} else if(str == Template.ENTITY_STR){
							if(t == GenUtils.none_t || f == GenUtils.none_f(t))
								throw new IllegalStateException("ENTITY term for none field: "
									+ GenUtils.tstr(t) + "." + GenUtils.fstr(t,f))
							GenUtils.emissions(t,f,ws.events(e).fields(f).value).argMax
						}else{
							str
						}
					})
				}
				//(get candidate strings)
				var out:Array[String] = 
					if(f == GenUtils.none_f(t)) {
						inferWords(wordDists(i))
					} else {
						val prior:Counter[String] = GenUtils.emissions(t,f,ws.events(e).fields(f).value)
						//inferWords(wordDists(i), prior, lm, fill) //infer from emission, empirical, and language model
						//inferWords(prior) //infer entirely from the emission distribution
						inferWords(wordDists(i), prior, (s:Array[String]) => {1.0}, fill) //infer from emission and empirical dist
					}
				//(check for UNK)
				/*
				if(out == null) {
					println(this)
					println(out)
					println(GenUtils.emissions(t,f,ws.events(e).fields(f).value))
					println(GenUtils.fstr(t,f))
					println(wordDists(i))
					println(GenUtils.arrayToString(wordDists(i).argMax))
					throw new IllegalStateException("INTERNAL: inferred word is null? what?")
				}
				if(out(0) == null) {
					println(this)
					println(GenUtils.arrayToString(out))
					throw new IllegalStateException("INTERNAL: inferred word has null in it? what?")
				}
				*/
				if(out == null || (out.length == 1 && (out(0) == null || out(0).equals("[[UNK]]")))){
					if(ws.events(e).fields(f).isNumeric ||  Template.opts.allowVstrFallback){
						out = GenUtils.vstr(t,f,ws.events(e).fields(f).value).toLowerCase.trim.split(" +")
					}else{
						out = Array[String]("42")
					}
				}
				//(concatenate)
				fRtn = Array.concat(fRtn, Array.make(out.length, f))
				wRtn = Array.concat(wRtn, out)
			})
			(fRtn, wRtn)
		}
	}

	def inferWords[E](wordDist:Counter[E]):Array[String] = {
		val obj:E = wordDist.argMax
		if(obj.isInstanceOf[String]) return obj.asInstanceOf[String].trim.split(" +")
		else return obj.asInstanceOf[Array[String]]
	}
	
	def inferWords(	wordDist:Counter[Array[String]], 
					prior:Counter[String],
					lm:Array[String]=>Double,
					filter:Array[String]=>Array[String]
						):Array[String] = {
		val dist:Counter[Array[String]] = new Counter[Array[String]]()
		val iter:Iterator[Array[String]] = wordDist.keySet.iterator
		//println(this)
		while(iter.hasNext){
			val raw:Array[String] = iter.next			//raw fragment (values empty)
			if(raw.length > 1) throw new IllegalStateException("multiword template completion: " + GenUtils.arrayToString(raw))
			val frag:Array[String] = filter( raw );		//filtered fragment (values filled)
			if(frag.length > 1) throw new IllegalStateException("multiword template completion: " + GenUtils.arrayToString(frag))
			val count:Double = wordDist.getCount(raw)
			//val count:Double = 1.0
			var numer:Double = 0
			var denom:Double = 0
			//(emissions weight)
			raw.foreach( (s:String) => {
				if(s == Template.NUM_STR || s == Template.ENTITY_STR) {
					numer += 1.0;	//assumed 1.0 for entity emission; taken care of in filter
				} else {
					numer += prior.getCount(s)
				}
				denom += 1.0
			})
			//(lm weight)
			val lmScore:Double = Math.pow(Math.exp(lm(frag)), Template.opts.lmFactor)
			//(set weight)
			//println("\tLM Score=" + GenUtils.df(lmScore) + "   Emission Score=" + GenUtils.df(numer/denom) +
			//	"     " + GenUtils.arrayToString(frag))
			dist.setCount(frag, lmScore*count*(numer/denom))
		}
		dist.argMax
	}

	def check = {
		if(size == 0) 
			throw new IllegalArgumentException("0 size template: " + this)
		if(fields.length != wordDists.length) 
			throw new IllegalArgumentException("field and word lengths don't match: " + this)
		foreach(wordDists, (counts:Counter[Array[String]]) => {
			if(counts.isEmpty) throw new IllegalArgumentException("no completions for template: " + this)
		})
		if(GenUtils.toMarkov(fields).length != fields.length){
			throw new IllegalArgumentException("Duplicate fields next to each other: " + this + "\n" 
				+ GenUtils.arrayToString( fields.map(GenUtils.fstr(t,_))));
		}
	}

	def size = fields.length

	def dump:String = {
		var rtn:String = toString + "\n"
		val df:java.text.DecimalFormat = new java.text.DecimalFormat("0.000")
		rtn += "\tField Set: " + this.fieldSet
		rtn += "\tParents:\n"
		parents.foreach((m:Template) => {
			rtn += "\t\t" + m + "\n"
		})
		foreach(fields.length, (i:Int) => {
			rtn += "\tf=" + GenUtils.fstr(t,fields(i)) + "\n"
			val counts:Counter[Array[String]] = wordDists(i)
			val iter:Iterator[Array[String]] = counts.keySet.iterator
			while(iter.hasNext){
				val key:Array[String] = iter.next
				val count = counts.getCount(key)
				rtn += "\t\t" + df.format(count) + "\t" + GenUtils.arrayToString(key) + "\n"
			}
		})
		rtn
	}

	def isMaximallySpecific:Boolean = {
		foreach(fields.size, (i:Int) => {
			val f = fields(i)
			if(f == GenUtils.none_f(t)){
				if(wordDists(i).keySet.size > 1) throw new IllegalStateException("Incidental check")
			}else{
				val iter:Iterator[Array[String]] = wordDists(i).keySet.iterator
				while(iter.hasNext){
					val words:Array[String] = iter.next
					if(words.length > 1) return false
				}
			}
		})
		return true
	}
	
	override def toString:String = {
		if(this == GenUtils.STOP_TEMPLATE) return "[[STOP]]"
		var str = "[Template]: t=" + GenUtils.tstr(t) + ":"
		foreach(size, (i:Int) => {
			val f:Int = fields(i)
			if(f == GenUtils.none_f(t)){
				str += " " + GenUtils.arrayToString(inferWords(wordDists(i)))
			} else {
				str += " --" + GenUtils.fstr(t,f) + "--"
			}
		})
		str + " (size=" + size + ")"
	}

	override def equals(o:Any):Boolean = {
		if(this eq o.asInstanceOf[AnyRef]) return true
		if(o.isInstanceOf[Template]){
			val other:Template = o.asInstanceOf[Template]
			if(other.getWordDists == null) return this.wordDists == null	//something's null
			if(other.getT != t) return false;	//different t
			val otherFields = other.getFields
			if(otherFields.length != fields.length) return false //different num fields
			foreach(fields.length, (i:Int) => {
				if(fields(i) != otherFields(i)) return false	//field doesn't match
			})
			foreach(fields.length, (i:Int) => {
				if(fields(i) == GenUtils.none_f(t)){
					if(wordDists(i).keySet.size > 1 || other.getWordDists(i).keySet.size > 1) throw new IllegalStateException("Invalid template found")
					val thisWords = wordDists(i).argMax
					val otherWords = other.getWordDists(i).argMax
					if(! (thisWords deepEquals otherWords) ) return false	//none_f emission doesn't match
				}
			})
			val tSpec:Boolean = this.isMaximallySpecific
			val oSpec:Boolean = other.isMaximallySpecific
			if( (tSpec && !oSpec) || (!tSpec && oSpec)) return false		//bit of a hack
			return true	//well, no choice but to accept it...
		}else{
			return false	//not a template
		}
	}
	var hashCodeCache:Int = 0
	override def hashCode:Int = {
		if(this == GenUtils.STOP_TEMPLATE) return -1337
		if(hashCodeCache == 0){
			var hashCode:Int = t
			//(hash code for each field)
			foreach(fields.length, (i:Int) => {
				val f:Int = fields(i)
				val term:Int = if(f == GenUtils.none_f(t)){
					//(hash code for string)
					var strCode:Int = 0
					val words:Array[String] = inferWords(wordDists(i))
					words.foreach( (a:String) => {
						strCode = strCode ^ a.hashCode
					})
					strCode
				}else{
					f
				}
				hashCode = hashCode ^ (term << (4*i))
			})
			hashCodeCache = hashCode
		}
		hashCodeCache
	}
}


case class FieldSet(t:Int,fields:HashSet[Int]) {
	def isEmpty = fields.isEmpty
	def hasField(f:Int):Boolean = {
		fields contains f
	}
	def getField(str:String):Int = {
		fields.foreach( (f:Int) => {
			if(GenUtils.fstr(f).equals(str)) return f
		})
		return -1
	}
	def size = fields.size
	def length = fields.size
	override def toString:String = {
		if(t < 0) return "[none_fs]"
		val b:StringBuilder = new StringBuilder
		b.append(GenUtils.tstr(t)).append(":")
		fields.foreach( (f:Int) => {
			b.append(" ").append(GenUtils.fstr(t,f))
		})
		b.toString
	}
	override def equals(o:Any):Boolean = {
		if(o.isInstanceOf[FieldSet]){
			val otherFields = o.asInstanceOf[FieldSet].fields
			val otherT = o.asInstanceOf[FieldSet].t
			if(otherT != t) return false //different ts
			return fields equals otherFields
		}
		return false //object is not a fieldSet
	}
	override def hashCode:Int = {
		var code = t
		val set = new HashSet[Int]
		//create hashcode
		fields.foreach( (f:Int) => {
			code ^= f
		})
		code
	}
}

object FieldSet{
	val fieldSetMap:HashMap[FieldSet,FieldSet] = new HashMap[FieldSet,FieldSet]
	def intern(fs:FieldSet):FieldSet = {
		val rtn:FieldSet = fieldSetMap.get(fs)
		if(rtn == null) {
			fieldSetMap.put(fs,fs)
			return fs
		}
		return rtn;
	}
}


object PhraseInterner {
	case class InternTerm(array:Array[String]) {
		override def equals(o:Any):Boolean = {
			if(o.isInstanceOf[InternTerm]){
				val other:Array[String] = o.asInstanceOf[InternTerm].array
				if(other.length != array.length) return false
				return (array deepEquals other)
			}
			return false
		}
		override def hashCode:Int = {
			var code:Int = 0;
			array.foreach( (s:String) => {
				code = code ^ s.hashCode
			})
			return code
		}
	}

	val internMap:HashMap[InternTerm, Array[String]] = new HashMap[InternTerm, Array[String]]

	def intern(a:Array[String]):Array[String] = {
		val term = new InternTerm(a)
		val rtn = internMap.get(term)
		if(rtn == null){
			internMap.put(term, a)
			return a
		}else{
			return rtn
		}
	}
}

object Template {
	val NUM_STR = "***NUM***"
	val ENTITY_STR = "***ENTITY***"
	var opts:induction.Options = null

	var ENTITY_REGEX = ""
	
	var templates:Array[List[Template]] = null
	val templateCounts:Counter[Template] = new Counter[Template]
	val templateMap:HashMap[Template,Template] = new HashMap[Template,Template]()
	val templateParentMap:HashMap[Template,Template] = new HashMap[Template,Template]()
		
	def isSpecial(w:String) = { w == ENTITY_STR || w == NUM_STR }

	def registerAndGet(t:Int, fieldList:Array[Int], wordList:Array[Array[String]]) : Template = {
		//--Specific Template
		//(variables)
		var newFieldList = List[Int]()
		var newWordList = List[Array[String]]()
		var lastF:Int = -1
		var wordTmp:List[String] = List[String]()
		val next:Unit=>(Int,Int,String) = GenUtils.deepIter(wordList)
		var info:(Int,Int,String) = next()
		//(loop)
		while(info != null){
			val w:String = info._3
			val f:Int = if( !isSpecial(w) ) GenUtils.none_f(t) else fieldList(info._1) //everything but special terms is none_f
			if( isSpecial(w) && f == GenUtils.none_f(t)) throw new IllegalArgumentException("Special term in none field")

			//(add a template part)
			if(lastF >= 0 && f != lastF){
				if(wordTmp isEmpty) throw new java.lang.IllegalArgumentException("Something went wrong...")
				newFieldList = lastF :: newFieldList
				if(wordTmp.length > 1 && lastF != GenUtils.none_f(t)){
					throw new IllegalStateException("multiword field: " + wordTmp)
				}
				newWordList = wordTmp.reverse.toArray :: newWordList
				wordTmp = List[String]()
			}
			//(add this word
			wordTmp = w :: wordTmp
			lastF = f
			info = next()
		}
		//(last term)
		if(wordTmp isEmpty) throw new java.lang.IllegalArgumentException("Something went wrong...")
		newFieldList = lastF :: newFieldList
		newWordList = wordTmp.reverse.toArray :: newWordList
		wordTmp = List[String]()
		//(create arrays)
		val specFieldList = newFieldList.reverse.toArray
		val specWordList = newWordList.reverse.toArray
		//--Test Time
		if(templates == null){
			val counters:Array[Counter[Array[String]]] = new Array[Counter[Array[String]]](specWordList.length)
			foreach(counters.length, (i:Int) => {
				counters(i) = new Counter[Array[String]]()
				if(specFieldList(i) != GenUtils.none_f(t) &&
					specWordList(i).length > 1) 
						throw new IllegalStateException("Multiword field: " + GenUtils.arrayToString(specWordList(i)))
				counters(i).incrementCount(specWordList(i), 1.0)
			})
			val key:Template = new Template(t, specFieldList, counters)
			val m:Template = templateMap.get(key)
			if(m == null) throw new IllegalArgumentException("Extracted a template that doesn't exist: " + key)
			if(m.fieldSet == null) throw new IllegalArgumentException("Extracted template without fieldset: " + key)
			if(!m.isMaximallySpecific) throw new IllegalStateException("Retreived template which is not maximally specific: " + m )
			return m
		}
		//--Register a new template
		//(create specific template)
		val counters:Array[Counter[Array[String]]] = new Array[Counter[Array[String]]](specWordList.length)
		foreach(counters.length, (i:Int) => {
			counters(i) = new Counter[Array[String]]()
			counters(i).incrementCount(specWordList(i), 1.0)
		})
		var m:Template = new Template(t, specFieldList, counters)
		//intern it
		if(templateMap.get(m) != null) m = templateMap.get(m)
		templateMap.put(m,m)
		//set its field set
		val fieldSet:FieldSet = FieldSet.intern(new FieldSet(t,
			{
				//note: add every numeric field in specific template
				//plus every categorical field in the original field list
				val set:HashSet[Int] = new HashSet[Int]
				var fieldSetList = fieldList.filter( (f:Int) => !WorldState.isNumeric(t,f) ).toList
				fieldSetList = fieldSetList ::: specFieldList.filter( (f:Int) => WorldState.isNumeric(t,f)).toList
				fieldSetList.foreach( (f:Int) => if(f != GenUtils.none_f(t)) set += f )
				set
			}))
		m.fieldSet = fieldSet
		//(create parent template)
		val pCounters:Array[Counter[Array[String]]] = new Array[Counter[Array[String]]](wordList.length)
		foreach(pCounters.length, (i:Int) => {
			pCounters(i) = new Counter[Array[String]]()
			pCounters(i).incrementCount(wordList(i), 1.0)
		})
		var parent:Template = new Template(t, fieldList, pCounters)
		if(templateParentMap.get(parent) != null) parent = templateParentMap.get(parent)
		templateParentMap.put(parent,parent)
		
		/*
		if(m.parents.size >= 1){
			if(m.parents.size > 1) println("-------------!!!!!!!!!!!!!!-------------")
			for( i <- 0 until fieldList.length){
				print("(" + GenUtils.fstr(0,fieldList(i)) + ") ")
				print(GenUtils.arrayToString(wordList(i)))
			}
			println()
			println("templ: " + m)
			println("parents:")
			m.parents.foreach(println(_))
			println("new parent:")
			println(parent)
		}
		*/

		m.parents += parent
		//(return it)
		if(templates(t) == null) templates(t) = List[Template]()
		templates(t) = m :: templates(t)
		return m
	}

	def maximalFieldSet(t:Int) = {
		FieldSet.intern(new FieldSet(t,
			{
				val set:HashSet[Int] = new HashSet[Int]
				for( f <- 0 until GenUtils.F(t) ){
					set += f
				}
				set
			}))
	}

	def alignedSentence2Templates(as:AlignedSentence, ws:WorldState):List[(Template,Int,Array[String])] = {
		//--Overhead
		val strings:Array[String] = as.getWords
		//(state)

		//--FN: Extract Given t
		def extractTemplate(e:Int, start:Int, stop:Int):Template = {
			//(variables)
			val t = GenUtils.e2t(e,ws)
			var fieldList = List[Int]()
			var wordList = List[Array[String]]()
			var fieldWordList = List[String]()
			var lastF = GenUtils.dead_f(t)
			foreach(start, stop, (i:Int) => {
				val f = if(t == GenUtils.none_t) GenUtils.none_f(t) else as.fields(i)
				if(strings(i).trim().equals("")){ 
					throw new IllegalArgumentException("null word in template?")
				}
				//(field boundary)
				if(lastF != GenUtils.dead_f(t) && f != lastF){
					fieldList = lastF :: fieldList
					wordList = PhraseInterner.intern(fieldWordList.reverse.toArray) :: wordList
					fieldWordList = List[String]()
				}
				//(add this word)
				val str:String 
					//(actual field)
					= if(t != GenUtils.none_t && f != GenUtils.none_f(t)){
						if(	ws.events(e).fields(f).isNumeric &&	GenUtils.isInt(strings(i))) 
							NUM_STR 	// a number from a numeric field
						else if( strings(i).matches(ENTITY_REGEX) )
							ENTITY_STR	// this is a special entity string
						else{
							strings(i)
						}
					//(none_t or none_f)
					} else { 
						strings(i)	// anything else
					}
				fieldWordList = str :: fieldWordList
				lastF = f
			})
			if(fieldWordList.length > 0){
				//(add last field)
				fieldList = lastF :: fieldList
				wordList = PhraseInterner.intern(fieldWordList.reverse.toArray) :: wordList
			}
			//(return a template)
			registerAndGet(t, fieldList.reverse.toArray, wordList.reverse.toArray)
		}
		
		//--FN: Extract Words
		def extractWords(e:Int, start:Int, stop:Int):Array[String] = {
			as.getWords.subArray(start, stop)
		}

		//--Extract Templates
		var start = 0
		var templateList = List[(Template,Int,Array[String])]()
		foreach(GenUtils.toMarkov(as.observedEvents), (e:Int) => {
			//(get range of event type)
			var end = start+1
			while(end < as.observedEvents.length && as.observedEvents(end) == e) 
				end += 1
			if(start == end || end > as.observedEvents.length) throw new IllegalStateException("this shouldn't happen")
			val m:Template = extractTemplate(e, start, end)
			val words:Array[String] = extractWords(e, start, end)
			templateList = (m,e,words) :: templateList
			start = end
		})
		//(Option: use maximal field set)
		if(opts.forceMaximalFS){
			templateList = templateList.map( (tuple:(Template,Int,Array[String])) => {
				val m:Template = tuple._1
				m.fieldSet = maximalFieldSet(m.getT)
				(m,tuple._2,tuple._3)
			})
		}
		//(return)
		templateList.reverse
	}

	val times:Array[String] = Array[String](
		"early afternoon",
		"mid afternoon",
		"late afternoon",
		"afternoon",
		"end of period",
		"early evening",
		"late evening",
		"mid evening",
		"evening",
		"early morning",
		"mid morning",
		"late morning",
		"morning",
		"midday",
		"midnight",
		"later"
	)

	//NOTE: THIS IS ONLY FOR SUMTIME!!
	def heuristicAlign(wds:Array[Int], sent:String, ws:WorldState):AlignedSentence = {
		var words:Array[Int] = wds
		var fields = sent
		def replace(src:String, dest:String):Unit = {
			fields = fields.replaceAll(""" """ + src + """ """, " "+dest+" ")
			fields = fields.replaceAll("""^""" + src + """$""", ""+dest+"")
			fields = fields.replaceAll("""^""" + src + """ """, ""+dest+" ")
			fields = fields.replaceAll(""" """ + src + """$""", " "+dest+"")
		}
		
		def extractTimes:Unit = {
			replace("""[0-9][0-9][0-9][0-9]""", "**TIME**")
			times.foreach( (s:String) => {
				val len:Int = s.trim.split(" +").length
				if(len == 1)
					replace(s, "**TIME**")
				else if(len == 2)
					replace(s, "**TIME** **TIME**")
				else if(len == 3)
					replace(s, "**TIME** **TIME** **TIME**")
				else
					throw new IllegalArgumentException("Unsupported Case: length=" + len + "   str=" + s)
			})
		}
		//--Get Fields
		replace(" +", " ")
		replace("""gusts [0-9]+ *- *[0-9]+""", " gusts **GUST_MIN** - **GUST_MAX**")
		replace("""[0-9]+ *- *[0-9]+""", "**MIN** - **MAX**")
		replace("""[0-9]+ or less""", "**MIN** or less ")
		replace("""less than [0-9]+""", " less than **MAX**")
		replace("""(n|s|e|w)(n|s|e|w)?(n|s|e|w)?(-(n|s|e|w)(n|s|e|w)?(n|s|e|w)?)?""", "**DIR**")
		replace("""gusts [0-9]+""", " gusts **GUST_MIN**")
		replace("""gust [0-9]+""", " gust **GUST_MIN**")
		extractTimes
		replace("""gusts around [0-9]+""", " gusts around **GUST_MIN**")
		replace("""gust around [0-9]+""", " gust around **GUST_MIN**")
		replace("""gusts to [0-9]+""", " gusts to **GUST_MIN**")
		replace("""cyclonic variable""", "**DIR** **DIR**")
		replace("""variable""", "**DIR**")
		replace("""[0-9]+""", "**MIN**")
		
		//--Words and Fields
		var fs:Array[Int] = fields.split(" +").map( (s:String) => {
				if(s equals "**MIN**") GenUtils.fval(0,"wind_min")		//HACK always event type 0 (for sumtime use only)
				else if(s equals "**MAX**") GenUtils.fval(0,"wind_max")
				else if(s equals "**DIR**") GenUtils.fval(0,"wind_dir")
				else if(s equals "**TIME**") GenUtils.fval(0,"time")
				else if(s equals "**GUST_MIN**") GenUtils.fval(0,"gust_min")
				else if(s equals "**GUST_MAX**") GenUtils.fval(0,"gust_max")
				else GenUtils.none_f(0)
			})
		//--Filter Times
		if(opts.filterTimes){ //prolly not a good idea to use
			var fList = List[Int]()
			var wList = List[Int]()
			var lastF:Int = -1
			var lastW:Int = -1
			for(i <- 0 until fs.length){
				val f:Int = fs(i)
				val w:Int = words(i)
				if(f == GenUtils.fval(0,"time")){
					if(lastF == f){
						//subsequent time fields: do nothing
					}else{
						//first time field: add
						fList = f :: fList
						wList = GenUtils.dead_w :: wList //different
					}
				}else{
					//standard case
					fList = f :: fList
					wList = w :: wList
				}
				lastF = f
			}
			fs = fList.reverse.toArray
			words = wList.reverse.toArray
		}

		if(words.length != fs.length) {
			println(GenUtils.arrayToString(words))
			println(GenUtils.arrayToString(fs))
			println(GenUtils.arrayToString(fs.map(GenUtils.fstr(0,_))))
			println(sent)
			println(fields)
			throw new IllegalStateException("Extracted fields wrong: " + words.length + ", " + fs.length)
		}
		
		//--Events
		var lastFBreak:Int = 0
		var lastEBreak:Int = 0
		var lastF:Int = GenUtils.dead_f(0)
		var lastSplit = 0
		var e = 0
		var seen:HashSet[Int] = new HashSet[Int]
		val events:Array[Int] = new Array[Int](words.length)
		foreach(words.length, (i:Int) => {
			val w = words(i)
			val f = fs(i)
			//break?
			def break = {
				//println(">>split on " + GenUtils.fstr(0,f) + " (" + i + ")" )
				foreach(lastEBreak, lastFBreak+1, (k:Int) => events(k) = e ) //fill last event
				e += 1
				foreach(lastFBreak+1, i, (k:Int) => events(k) = e ) //fill this event (redundant)
				lastEBreak = lastFBreak+1
				seen = new HashSet[Int]
				lastSplit = i
			}
			//break conditions
			if( ((f != lastF) && (lastSplit != i) && (f != GenUtils.fval(0,"time")) && (seen contains f)) )
				break
			if( ( (f != lastF) &&(lastSplit != i) && (!seen.contains(GenUtils.fval(0,"wind_dir"))) && (f == GenUtils.fval(0,"wind_min")) )  )
				break
			if( (f != lastF) && (lastSplit != i) && (!seen.isEmpty) && (f == GenUtils.fval(0,"wind_dir")) )
				break
			if( (f != lastF) && f == GenUtils.fval(0,"wind_min") && (lastSplit != i) && (i > 0) && ((GenUtils.wstr(words(i-1)).equals("increasing")) || (GenUtils.wstr(words(i-1)).equals("easing"))))
				break
			//if not none_f
			if(f != GenUtils.none_f(0)){
				lastFBreak = i
				seen += f
			}
			//overhead
			lastF = f
		})
		foreach(lastEBreak, events.length, (k:Int) => events(k) = e)

		
		val as:AlignedSentence = new AlignedSentence(new Array(events.length), new Array(events.length), fs, words, events) 
		if(e != ws.events.length-1) {
			return null
			//throw new IllegalStateException("Extracted too many/few events: e=" + e + " ws.size=" + ws.size)
		}
		return as
	}

	def createTemplates(dataset:AlignedDataset, start:Int, stop:Int, useVector:Array[Boolean]):Array[Template] = {
		val sentences:Array[AlignedSentence] = dataset.aligns.subArray(start, stop)
		val worldStates:Array[WorldState] = dataset.worldStates.subArray(start, stop)
		templates = new Array[List[Template]](GenUtils.T+1)

		//--Start 'er off
		if(sentences.length != worldStates.length) throw new java.lang.IllegalArgumentException("lengths don't match")
		val templateHash:HashSet[Template] = new HashSet[Template]
		//(add each template)
		foreach(sentences.length, (i:Int) => {
			if(useVector(start+i)) {
				val templs = alignedSentence2Templates(sentences(i), worldStates(i))
				templs.foreach( (tuple:(Template,Int,Array[String])) => {
						val m:Template = tuple._1
						templateCounts.incrementCount(m, 1.0)
						templateHash += m
					})
			}
		})
		//(convert to array)
		var rtn:List[Template] = List[Template]()
		var i=0; templateHash.foreach( (m:Template) => { 
			if(templateCounts.getCount(m) > opts.minTemplCount) rtn = m :: rtn
		})
		templates = null	//null the templates structure (use templateMap instead)
		return rtn.toArray;
	}

	import org.goobs.choices.ChoiceGenerator.Generation
	import AlignedDataset.AlignedSentence
	def gen2Sent(gen:Generation[(Int,Array[Int],Array[String])], ws:WorldState):(AlignedSentence,Double) = {
		//--Variables
		//(from generation)
		val templates:Array[(Int,Array[Int],Array[String])] = gen.getGenerated.map( x=>x )
		//(for aligned sentence)
		var events:Array[Int] = new Array[Int](0)
		var fields:Array[Int] = new Array[Int](0)
		var strings:Array[String] = new Array[String](0)
		//--Convert
		//(fill variables)
		foreach(templates.length, (mI:Int) => {
			val e = templates(mI)._1;
			val flds = templates(mI)._2;
			val wrds = templates(mI)._3;
			events = Array.concat(events, Array.make(flds.length,e))
			fields = Array.concat(fields, flds)
			strings = Array.concat(strings, wrds)
		})
		//(create sentence)
		val as:AlignedSentence 
			= new AlignedSentence(
				new Array[Int](events.length), 
				events.map( GenUtils.e2t(_,ws) ), 
				fields, 
				Array.make(events.length, GenUtils.dead_w), 
				events)
		as.strings = strings
		return (as,gen.getScore())
	}

	def defaultTemplate(t:Int):Template = {
		val counters:Array[Counter[Array[String]]] = new Array[Counter[Array[String]]](1)
		counters(0) = new Counter[Array[String]]()
		counters(0).incrementCount(Array[String]("**DEFAULT**"), 1.0)
		val m:Template = new Template(t, Array.make(1,GenUtils.none_f(t)), counters)
		//(intern this template)
		if(!templateMap.containsKey(m)) templateMap.put(m,m)
		templateMap.get(m)
	}

}
