package generation

import java.io.File

import tea.Utils.{map,foreach,fails,fail,logs}
import java.util.HashMap
import javax.script._
import org.goobs.utils.StatCalc
import org.goobs.classify._
import org.goobs.foreign.Counter
import org.goobs.distributions.Particle
import org.goobs.utils.Pair
import org.goobs.choices._
import AlignedDataset.AlignedSentence

import scala.collection.mutable.PriorityQueue

/*
*	A Trait for a feature key
*/
trait Feature { 
}

/*
*	An Event3 Choice History
*/
trait ChoiceHistory {
	def tooLong:Boolean
}

/*
*	NOTE: the fields of this class should not be modified by
*	anything other than this class, for risk of breaking things
*	in nasty and unexpected ways
*
*	TODO migrate specific histories to this class
*/
case class GeneralHistory(matrix:Array[Array[Any]], numGenerated:Array[Int], depth:Int) extends ChoiceHistory{
	override def tooLong:Boolean = matrix != null && matrix(0) != null && length > 100

	var genSoFar:Array[String] = null //really just a cache
	
	def length:Int = matrix(0).length

	def copy:GeneralHistory = copy(false, false, false, depth)
	//TODO try cheating
	private def cheatingCopy(goDown:Boolean, goRight:Boolean, newDepth:Int):GeneralHistory 
		= copy(goDown, goRight, false, newDepth)
	private def copy(goDown:Boolean, goRight:Boolean, cheat:Boolean, newDepth:Int):GeneralHistory = {
		//--Quit early if we can cheat
		if(cheat && !goDown && !goRight){
			if(newDepth == depth) return this
			else return new GeneralHistory(matrix, numGenerated, depth)
		}
		//--Copy Stuff
		val newMat:Array[Array[Any]] = new Array[Array[Any]]( {if(goDown) matrix.length+1 else matrix.length} )
		for( i <- 0 until newMat.length ){
			newMat(i)
				= if(cheat && !(goRight && i==depth)) matrix(i) //we're not going to modify this level
				  else{
					val rtn = new Array[Any]( {if(i==depth && goRight) matrix(i).length+1 else matrix(i).length} )
					Array.copy(matrix(i), 0, rtn, 0, matrix(i).length)
					rtn
				  }
		}
		val newNumGenerated = new Array[Int]( {if(goDown) numGenerated.length+1 else numGenerated.length} )
		Array.copy(numGenerated, 0, newNumGenerated, 0, numGenerated.length)
		new GeneralHistory(newMat,newNumGenerated,newDepth)

	}

	def parent[E](stepsUp:Int, stepsLeft:Int):E = {
		val vertIndex = depth-stepsUp
		if(vertIndex < 0) throw new IllegalArgumentException("Cannot go " + stepsUp + " steps up")
		val horizIndex = matrix(vertIndex).length-stepsLeft
		if(horizIndex < 0) throw new IllegalArgumentException("Cannot go " + stepsLeft + " steps left")
		return matrix(vertIndex)(horizIndex).asInstanceOf[E]
	}

	def goDown:GeneralHistory = {
		val rtn:GeneralHistory = cheatingCopy(depth==matrix.length, false, depth+1)
		rtn.numGenerated(rtn.depth) = 0 //note: must create new instance
		rtn
	}

	def goUp:GeneralHistory = {
		val rtn:GeneralHistory = cheatingCopy(false, false, depth-1)
		rtn
	}

	def goRight(term:Any):GeneralHistory = {
		val rtn:GeneralHistory = cheatingCopy(false, true, depth)
		rtn.matrix(rtn.depth)(rtn.matrix(rtn.depth).length-1) = term
		rtn.numGenerated(rtn.depth) += 1
		rtn
	}


}

/*
*	An Event3 Choice
*/
abstract class Event3Choice[History,Output](factory:ProbabilisticClassifierFactory[History,Feature,Output]) 
	extends ClassifierChoice[(History,WorldState),Feature,Output](
		factory.asInstanceOf[ProbabilisticClassifierFactory[(History,WorldState),Object,Output]]
		){
	val trainingData:java.util.ArrayList[LabeledInstance[(History,WorldState),Output]]
		= new java.util.ArrayList[LabeledInstance[(History,WorldState),Output]]()
	
	/*
	*	To Override
	*/
	def possibleOutputs(in:History, ws:WorldState):Array[Output]
	def inputFeatures(in:History, out:Output, ws:WorldState):Counter[Feature]
	def outputFeatures(in:History, out:Output, ws:WorldState):Counter[Feature]
	def jointFeatures(in:History, out:Output, ws:WorldState):Counter[Feature]
	def noHist:History
	def back(in:History,out:Output,ws:WorldState):History
	def extractInputs(sent:AlignedSentence, ws:WorldState, add:(History,Int)=>Any) : Unit
	def outputToString(in:History, out:Output, ws:WorldState):String

	//(redirects)
	override def possibleOutputs(in:(History,WorldState)):Array[Output] = possibleOutputs(in._1, in._2).map( x => x )
	override def inputFeatures(in:(History,WorldState), out:Output):Counter[Feature] = inputFeatures(in._1, out, in._2)
	override def outputFeatures(in:(History,WorldState), out:Output):Counter[Feature] = outputFeatures(in._1, out, in._2)
	
	override def jointFeatures(in:(History,WorldState), out:Output):Counter[Feature] = jointFeatures(in._1, out, in._2)
	override def noHistory:(History,WorldState) = (noHist,null)
	override def feedback(in:(History,WorldState),out:Output):(History,WorldState) = (back(in._1,out,in._2), in._2)
	override def outputToString(in:(History,WorldState), out:Output):String = outputToString(in._1, out, in._2)

	/*
	*	Implemented
	*/
	override def concat(left:Feature, right:Feature) = (left, right)

	def registerSentence(as:AlignedSentence, ws:WorldState):Unit = {
		//do nothing
	}

	def addExample(in:(History,WorldState), out:Output):Unit = {
		val posOutputs:Array[Output] = possibleOutputs(in)
		val instance:LabeledInstance[(History,WorldState),Output] 
			= new LabeledInstance[(History,WorldState), Output](
				in,
				posOutputs,
				posOutputs.indexOf(out)
				)
		//(add data point)
		trainingData.add(instance)
	}
	
	def addSentence(rawInp:(AlignedSentence,WorldState)){
		extractInputs(rawInp._1, rawInp._2, (in:History, out:Int) => {
			//(get possible outputs)
			val posOutputs:Array[Output] = possibleOutputs(in,rawInp._2).map( x => x )
			//(create data point)
			val instance:LabeledInstance[(History,WorldState),Output] 
				= new LabeledInstance[(History,WorldState), Output](
					(in,rawInp._2),
					posOutputs,
					out
					)
			//(add data point)
			trainingData.add(instance)
			//(error checking)
			if(out < 0 || out >= posOutputs.size){
				fails("Output index is out of range: " + out)
			}
		})
	}
	
	def train():Any = {
		train(trainingData)
	}

	def dumpParams(name:String) = {
		if(classifier == null) throw new java.lang.IllegalStateException("Train before dumping weights")
		val genParamOutputDir:String = fig.exec.Execution.getFile("features");
		if(genParamOutputDir != null){
			//(get the directory)
			val dir:File = new File(genParamOutputDir)
			if(!dir.exists) {
				dir.mkdirs
			}
			val filename:String = genParamOutputDir + "/" + name
			//(get the file)
			val f:File = new File(filename)
			if(f.exists){
				f.delete
			}
			//(dump params)
			classifier.dumpWeights(filename)
		}
	}
}


/*
*					-----------------------
*						RANGE CHECKER
*					-----------------------
*
*/


object RangeChecker {
	val statCalcs = new HashMap[Int,HashMap[Int,StatCalc]]
	var resolution:Int = 10

	def dump(puts:(Int,Int,StatCalc)=>Any):Any = {
		val iter = statCalcs.keySet().iterator()
		while(iter.hasNext){
			val t:Int = iter.next
			val calcs = statCalcs.get(t)
			val iter2 = calcs.keySet().iterator()
			while(iter2.hasNext){
				val f:Int = iter2.next
				val calc:StatCalc = calcs.get(f)
				puts(t,f,calc)
			}
		}
	}

	def getCalc(t:Int, f:Int) : StatCalc = {
		val map:HashMap[Int,StatCalc] = statCalcs.get(t)
		if(map == null) throw new java.lang.IllegalStateException("RangeChecker has no info for event type " + t)
		val calc  = map.get(f)
		if(calc == null) new StatCalc
		else calc
	}
	
	def digestWorldState(ws:WorldState):Any = {
		ws.events.foreach( (ev:Event) => {
			val t = ev.t
			if(statCalcs.get(t) == null) statCalcs.put(t, new HashMap[Int,StatCalc])
			val fieldCalcs = statCalcs.get(t)
			ev.fields.foreach( (field:Field) => {
				val f = field.f
				if(f >= 0){
					if(fieldCalcs.get(f) == null) fieldCalcs.put(f, new StatCalc)
					val calc = fieldCalcs.get(f)
					calc.enter(field.value)
				}
			})
		})
	}

	def addFeaturesFlat(t:Int, f:Int, v:Int, feats:Counter[Object], uniqueID:String) : Unit = {
		feats.incrementCount(new ValueFeature(uniqueID,t,f," value=", v, true), 1.0)
	}
	
	def addFeaturesBucket(t:Int,f:Int,v:Int,feats:Counter[Object], uniqueID:String) : Unit = {
		val minVal:Int = getCalc(t,f).getMin.asInstanceOf[Int]
		val maxVal:Int = getCalc(t,f).getMax.asInstanceOf[Int]
		val inc:Int = Math.max(1, (maxVal-minVal) / resolution)
		var current:Int = minVal + inc
		while(current < (maxVal + inc)) {
			if(v > current)		feats.incrementCount(new ValueFeature(uniqueID,t,f," is ", current, false), 1.0)
			current += inc
		}
	}

	
	def addFeaturesFuzzy(t:Int,f:Int,v:Int,feats:Counter[Object],uniqueID:String) : Unit = {
		//(coordinates)
		val calc = getCalc(t,f)
		//TODO MISNOMERS: absMin = min, minVal = mean-stdDev, etc...
		val absMin:Double = calc.getMin 
		val minVal:Double = Math.max(calc.getMean - calc.getStandardDeviation, calc.getMin)
		val maxVal:Double = Math.min(calc.getMean + calc.getStandardDeviation, calc.getMax)
		val middle:Double = calc.getMean
		val absMax:Double = calc.getMax
		
		//(fuzzy values)
		def veryLow(v:Double) : Double = {
			val cand:Double = (minVal - v) / (minVal - absMin)
			if(absMin > minVal) 0.0 //case: stdev falls below min value
			else if(v < absMin) 1.0
			else if(cand < 0.0) 0.0
			else cand
		}
		def low(v:Double) : Double = {
			val cand:Double = (middle-v) / (middle-minVal)
			if(v < minVal) 1.0
			else if(cand < 0.0) 0.0
			else cand
		}
		def medium(v:Double) : Double = {
			val cand:Double = if(v>=middle) { (maxVal-v)/(maxVal-middle)  } else { (v-minVal)/(middle-minVal) }
			if(cand < 0.0) 0.0
			else cand
		}
		def high(v:Double) : Double = {
			val cand:Double = (v-middle) / (maxVal-middle)
			if(v > maxVal) 1.0
			else if(cand < 0.0) 0.0
			else cand
		}
		def veryHigh(v:Double) : Double = {
			val cand:Double = (v-maxVal) / (absMax - maxVal)
			if(absMax < maxVal) 0.0 //case: stdev rises above max value
			else if(v > absMax) 1.0
			else if(cand < 0.0) 0.0
			else cand
		}
		//(increment values)
		val vl = veryLow(v); val l = low(v); val m = medium(v); val h = high(v); val vh = veryHigh(v);
		if(l>0.0) feats.incrementCount(new ValueFeature(uniqueID, t, f, " is low", -1, false), l)
		if(m>0.0) feats.incrementCount(new ValueFeature(uniqueID, t, f, " is medium", -1, false), m)
		if(h>0.0) feats.incrementCount(new ValueFeature(uniqueID, t, f, " is high", -1, false), h)
		if(vl>0.0) feats.incrementCount(new ValueFeature(uniqueID,t, f, " is very low", -1, false), vl)
		if(vh>0.0) feats.incrementCount(new ValueFeature(uniqueID,t, f, " is very high", -1, false), vh)
		if(l>1.0) throw new IllegalStateException("Value is greater than 1: " + l)
		if(m>1.0) throw new IllegalStateException("Value is greater than 1: " + m)
		if(h>1.0) throw new IllegalStateException("Value is greater than 1: " + h)
		if(vl>1.0) throw new IllegalStateException("Value is greater than 1: " + vl)
		if(vh>1.0) throw new IllegalStateException("Value is greater than 1: " + vh)
	}

	def addFeaturesDynamic(e:Int, f:Int, v:Int, feats:Counter[Object], uniqueID:String, valType:induction.Options.ValueType, ws:WorldState):Unit = {
		if(valType == induction.Options.ValueType.none){
			return;
		}
		val t = ws.events(e).t
		if(ws.events(e).fields(f).isNumeric){
			valType match {
				case induction.Options.ValueType.none => {  }
				case induction.Options.ValueType.simple => { addFeaturesFlat(t,f,v,feats,uniqueID) }
				case induction.Options.ValueType.bucket => { addFeaturesBucket(t,f,v,feats,uniqueID) }
				case induction.Options.ValueType.fuzzy  => { addFeaturesFuzzy(t,f,v,feats,uniqueID) }
				case _ => throw new java.lang.IllegalArgumentException("Unknown value feature type")
			}
		} else {
			valType match {
				case induction.Options.ValueType.none => { }
				case _ => addFeaturesFlat(t,f,v,feats,uniqueID)
			}
		}
	}
}



/*
*					------------------
*						FEATURES
*					------------------
*
*/

//(general features)
case class ValueFeature(prefix:String, t:Int, f:Int, value:String, v:Int, appendV:Boolean) extends Feature{
	override def toString:String = prefix + ",t=" + GenUtils.tstr(t) + ",f=" + GenUtils.fstr(t,f) + value + {if(appendV) GenUtils.vstr(t,f,v) else ""}
}
case class StrFeature(str:String) extends Feature{
	override def toString:String = str
}
//(event features)
case class EventMarkovTail(tail:List[Int]) extends Feature{
	override def toString:String = {
		val sb:StringBuilder = new StringBuilder
		sb.append("t:")
		var t = tail
		while(t.size > 0){
			sb.append(GenUtils.tstr(t.head)).append("-")
			t = t.tail
		}
		sb.toString
	}
}
case class EventSeen(t:Int) extends Feature{
	override def toString:String = "t=" + GenUtils.tstr(t) + "-seen"
}
case class EventMarker(t:Int) extends Feature{
	override def toString:String = "t=" + GenUtils.tstr(t)
}
case class RepeatedEventType() extends Feature{
	override def toString:String = "repeated-event-type"
}
case class RepeatedEvent() extends Feature{
	override def toString:String = "repeated-event"
}
case class EventPos(t:Int, pos:Int) extends Feature{
	override def toString = "t=" + GenUtils.tstr(t) + "-is-" + {if(pos==0) "first" else if(pos==1) "middle" else "end"}
}
case class DistinctEventTypes(num:Int) extends Feature{
	override def toString = "distinct-t-in-ws=" + num
}
case class EventNewType(t:Int, seen:Boolean) extends Feature{
	override def toString = "t=" + GenUtils.tstr(t) + "-seen-in-last-ws=" + seen
}
case class EventPresent(t:Int)  extends Feature{
	override def toString = "t=" + GenUtils.tstr(t) + "-present-in-ws"
}
case class EventSeenSet(seen:List[Int]) extends Feature{
	override def toString = "seen=" + seen.map(GenUtils.tstr(_))
}
//(field features)
case class FieldMarkovTail(tail:List[Int], t:Int) extends Feature{
	override def toString:String = {
		val sb:StringBuilder = new StringBuilder
		sb.append("f:")
		var ta = tail
		while(ta.size > 0){
			sb.append(GenUtils.fstr(t,ta.head)).append("-")
			ta = ta.tail
		}
		sb.toString
	}
}
case class FieldMarker(t:Int,f:Int) extends Feature{
	override def toString:String = "f=" + GenUtils.fstr(t,f) + ",t=" + GenUtils.tstr(t)
}
case class RepeatedField(t:Int) extends Feature{
	override def toString:String = "repeated-field-t=" + GenUtils.tstr(t)
}
//(word choices)
case class WordParentEvent(t:Int) extends Feature{
	override def toString:String = "parent-t=" + GenUtils.tstr(t)
}
case class WordParentField(t:Int, f:Int) extends Feature{
	override def toString:String = "parent-f=" + GenUtils.fstr(t,f) + ",t=" + GenUtils.tstr(t)
}
case class WordMarker(w:Int) extends Feature{
	override def toString:String = "w=" + GenUtils.wstr(w)
}
case class WordLength(t:Int,f:Int,gen:Int,isStop:Boolean) extends Feature{
	override def toString:String = "" + {if(isStop) "stop" else "continue"} + "-after-len=" + gen + ",t=" + GenUtils.tstr(t) + ",f=" + GenUtils.fstr(t,f)
}
case class WordMarkov(t:Int,f:Int,lastW:Int) extends Feature{
	override def toString:String = "" + GenUtils.wstr(lastW) +",t=" + GenUtils.tstr(t) + ",f=" + GenUtils.fstr(t,f) + " ->"
}
case class LMScore() extends Feature{
	override def toString:String = "lm-score"
}
case class WSFeature(ws:WorldState) extends Feature{
	override def toString:String = "ws: " + GenUtils.uniqueIndex(ws)
}
//(field set choices)
case class FSMarker(m:FieldSet) extends Feature{
	override def toString:String = "fs=" + m
}
//(template choices)
case class TemplateMarker(m:Template) extends Feature{
	override def toString:String = "m=" + m
}
case class LengthMarker(l:Int) extends Feature{
	override def toString:String = "len=" + l
}
case class ParentTemplate(m:Template) extends Feature{
	override def toString:String = "parent=" + m
}
case class ValueDiff(t:Int,f:Int,dir:Int) extends Feature{
	override def toString:String = "" + GenUtils.fstr(t,f) + " " + {if(dir > 0) "increased" else if(dir == 0) "stayed the same" else "decreased"}
}




/*
*					--------------------
*						EVENT CHOICE
*					--------------------
*
*/
/*
class EventHistory(matrix:Array[Array[Any]], genNum:Array[Int], depth:Int) 
	extends GeneralHistory(matrix, genNum, depth) {
	
	def eventHist = matrix(0)

	override def toString = {
		"[" + Array.deepToString eventHist + "]: " + Array.deepToString genSoFar
	}

}
*/
case class EventHistory(eventHist:Array[Int], genSoFar:Array[String]) extends ChoiceHistory{
	override def tooLong = {
		eventHist.size > 100
	}
	override def toString = {
		"[" + GenUtils.arrayToString(eventHist) + "]: " + GenUtils.arrayToString(genSoFar)
	}
}



case class EventChoice(
		lm:LanguageModel, 
		factory:ProbabilisticClassifierFactory[EventHistory,Feature,Int], 
		outputOK:Int=>Boolean, 
		opts:induction.Options)
	extends Event3Choice[EventHistory,Int](factory) {	
	
	def outputOK(t:Int):Boolean = true

	def eventIndexToPossibleOutputIndex(e:Int, ws:WorldState):Int = if(e==stopTerm) GenUtils.dead_e(ws) else e

	override def stopTerm = GenUtils.STOP_INT
	override def noHist
		= new EventHistory(new Array[Int](0), Array[String]())

	//--Hack for single event generation
	override def getProbabilities(in:(EventHistory,WorldState)) : Counter[Int] = {
		val cand:Counter[Int] = super.getProbabilities(in)
		if(opts.singleEventGen && in._1.eventHist.size > 0){
			val rtn = new Counter[Int]()
			rtn.setCount(stopTerm, 1.0)
			rtn
		}else{
			//remove invalid keys
			val iter:java.util.Iterator[Int] = cand.keySet.iterator
			var toRemove = List[Int]()
			while(iter.hasNext){
				val e = iter.next
				if(e != stopTerm && !outputOK(GenUtils.e2t(e, in._2))){
					toRemove = e :: toRemove
				}
			}
			toRemove.foreach( (e:Int) => cand.removeKey(e))
			//ensure at least one event is present
			if(cand.keySet.isEmpty){
				cand.setCount(GenUtils.none_e(in._2), 1.0)
			}
			//return
			cand.normalize
			//println(GenUtils.tstr(GenUtils.e2t(cand.argMax(),in._2)) + ": " + cand.getCount(cand.argMax()) + "   stop=" + cand.getCount(stopTerm))
	//		p//rintln("\tsuper stop="  + super.getProbabilities(in).getCount(stopTerm))
			//println("\thistory=" + in._1)
			cand
		}
	}

	override def extractInputs(sent:AlignedSentence, ws:WorldState, add:(EventHistory,Int)=>Any) : Unit = {
		//add(EventHistory, Event Index in PossibleOutputs) => Any
		val array:Array[Int] = GenUtils.toMarkov(sent.observedEvents)
		foreach(array.size, (index:Int) => {
			val input:Array[Int] = new Array[Int](index)
			foreach(input.size, (i:Int) => {input(i) = array(i)})
			val output:Int = eventIndexToPossibleOutputIndex(array(index), ws)
			//print(GenUtils.estr(array(index),ws) + "(" + array(index) + ") | "); 
			//GenUtils.printArray(input.map(GenUtils.estr(_,ws)));
			add(new EventHistory(input,Array[String]()), output) //TODO DON'T CONDITION ON GEN_SO_FAR
		})
		//(add stop event)
		var fullHist:Array[Int] = new Array[Int](array.size)
		foreach(fullHist.size, (i:Int) => {fullHist(i) = array(i)})
		//print("STOP" + " | "); GenUtils.printArray(fullHist.map(GenUtils.estr(_,ws)));
		add(new EventHistory(fullHist, sent.getWords), eventIndexToPossibleOutputIndex(stopTerm, ws))
	}


	override def inputFeatures(in:EventHistory, e:Int, ws:WorldState):Counter[Feature] = {		
		def markovTail(eventHist:Array[Int], f:Counter[Feature]) = {
			var tail:Int = opts.featEventTail
			var eventI = eventHist.size - 1
			var tailList = List[Int]()
			while(tail > 0){
				if(eventI < 0) tailList = GenUtils.start_t :: tailList
				else tailList = GenUtils.e2t(eventHist(eventI),ws) :: tailList
				eventI -= 1
				tail -= 1
				if(opts.featEventAllTails) f.incrementCount(new EventMarkovTail(tailList), 1.0)
			}
			if(!opts.featEventAllTails) f.incrementCount( new EventMarkovTail(tailList), 1.0 )
		}
		val eventHist = in.eventHist
		val f:Counter[Feature] = new Counter[Feature]
		// Last event type
		if(opts.featEventMarkov){
			markovTail(eventHist, f)
		}
		//all previous event types seen
		if(opts.featEventHist){
			eventHist.foreach( (e:Int) => {
				f.setCount( new EventSeen(GenUtils.e2t(e,ws)), 1.0 )
			})
		}
		//all previous events types seen (in a set)
		if(opts.featEventHistSet){
			val cpy:Array[Int] = eventHist.map(GenUtils.e2t(_,ws))
			scala.util.Sorting.quickSort(cpy)
			var lst:List[Int] = List[Int]()
			foreach(1,cpy.size, (i:Int) => {
				if(cpy(i) != cpy(i-1)) lst = cpy(i) :: lst
			})
			f.incrementCount( new EventSeenSet(lst), 1.0)
		}
		//number of distinct event types in world state
		if(opts.featEventDistinct){
			val seen = new scala.collection.mutable.HashSet[Int]
			ws.events.foreach( (ev:Event) => {
				seen += ev.t
			})
			f.incrementCount( new DistinctEventTypes(seen.size), 1.0)
		}
		//Other events present
		if(opts.featEventOtherPresent){
			ws.events.foreach( (ev:Event) => {
				val t = ev.t
				f.incrementCount( new EventPresent(t), 1.0 )
			})
		}
		//Field Values
		if(e != stopTerm && e != GenUtils.none_e(ws)){
			val event:Event = ws.events(e)
			event.fields.foreach( (field:Field) => {
				val fI = field.f
				RangeChecker.addFeaturesDynamic(	
									e, 
									fI, 
									field.value, 
									f.asInstanceOf[Counter[Object]], 
									"[t-value]:", 
									opts.featEventVal, 
									ws)
			})
		}

		//Overkill
		if(opts.featEventOverkill){
			f.setCount( new WSFeature(ws), 1.0 )
		}

		return f
	}

	override def outputFeatures(in:EventHistory, e:Int, ws:WorldState):Counter[Feature] = {
		val f:Counter[Feature] = new Counter[Feature]
		if(e == stopTerm) {
			// Stop Term
			f.incrementCount(new EventMarker(stopTerm), 1.0)
		} else if(e == GenUtils.none_e(ws)) {
			// None Event
			f.incrementCount(new EventMarker(GenUtils.none_t), 1.0)
		} else {
			//Event Type
			f.incrementCount(new EventMarker(GenUtils.e2t(e,ws)), 1.0)
		}
		return f
	}
	
	override def jointFeatures(in:EventHistory, e:Int, ws:WorldState):Counter[Feature] = {
		val eventHist = in.eventHist
		val f:Counter[Feature] = new Counter[Feature]
		
		// Number of events of that type
		if(opts.featEventIndex && e >= 0 && e != GenUtils.none_e(ws)){
			//(determine feature index)
			val t = ws.events(e).t
			var first = true; var isFirst = false; var lastI = -1
			foreach(ws.events.size, (i:Int) => {
				if(ws.events(i).t == t){
					if(first && i==e) isFirst = true;
					first = false;
					lastI = i
				}
			})
			val isLast = (lastI == e)
			//(add features)
			if(isFirst && !isLast) f.incrementCount(new EventPos(t, 0), 1.0)
			if(!isFirst && !isLast) f.incrementCount(new EventPos(t, 1), 1.0)
			if(isLast && !isFirst) f.incrementCount(new EventPos(t, 2), 1.0)
		}

		if(opts.featEventTemporal && e >= 0 && e != GenUtils.none_e(ws)){
			if(e <= ws.events.size / 2){
				f.incrementCount(new StrFeature("event-in-first-half"), 1.0)
			}else{
				f.incrementCount(new StrFeature("event-in-second-half"), 1.0)
			}
		}

		if(e == stopTerm && opts.featEventLMStop){
			try{
				//TODO why does this exception on weather?
				val prob:Double = lm.stopProb(in.genSoFar.toList)
				if(prob > 0){
					f.incrementCount( new StrFeature("eventLMStop"), Math.log(prob) )
				}else{
					throw new IllegalStateException("LM returned 0 probability");
				}
			}catch{
				case (e:Exception) => {
					e.printStackTrace
					System.err.println("EXCEPTION: " + in.genSoFar.toList)
				}
			}
		}

		/*
		// New Event Type
		if(opts.featEventNewType && e != stopTerm && in.lastWS != null){
			var seen = false;
			in.lastWS.events.foreach( (ev:Event) => {
				if(ev.t == GenUtils.e2t(e,ws)) seen = true
			})
			f.incrementCount(new EventNewType(GenUtils.e2t(e,ws), seen), 1.0)
		}
		*/
		
		// Event Type seen before
		if( e != stopTerm && e != GenUtils.none_e(ws) && opts.featEventTypeRep ){
			val seenCount:Int = eventHist.filter( (x:Int) => {GenUtils.e2t(x,ws)==GenUtils.e2t(e,ws)} ).size
			f.incrementCount(new RepeatedEventType(), seenCount)
		}
		
		// Actual Event seen before
		if( e != stopTerm && e != GenUtils.none_e(ws) && opts.featEventRep){
			val seenCount:Int = eventHist.filter( (x:Int) => {x==e} ).size
			f.incrementCount(new RepeatedEvent(), seenCount)
		}
			

		return f
	}

	// defined as possibleOutputs(outputIndex) = eventIndex
	override def possibleOutputs(in:EventHistory, ws:WorldState):Array[Int] = {
		val rtn = (0 to GenUtils.dead_e(ws)).toArray
		rtn(GenUtils.dead_e(ws)) = GenUtils.STOP_INT
		rtn
	}
	
	override def back(in:EventHistory, out:Int, ws:WorldState) : EventHistory = {
		val append = new Array[Int](1)
		append(0) = out
		new EventHistory(Array.concat(in.eventHist, append), in.genSoFar)
	}

	override def outputToString(in:EventHistory, out:Int, ws:WorldState) : String = {
		if(out == GenUtils.none_e(ws)) GenUtils.tstr(GenUtils.none_t) 
		else if(out == GenUtils.fail_e) "fail_e"
		else "(" + out + ")" + GenUtils.tstr(GenUtils.e2t(out,ws))
	}

	override def toString:String = "EventChoice"
}






/*
*					-------------------------
*						STRICT EVENT CHOICE
*					-------------------------
*
*/
case class StrictEventChoice(eventFn:WorldState=>Array[Int], outputOK:Int=>Boolean, opts:induction.Options)
		extends AChoice[(EventHistory,WorldState),Int] {
	var cacheCond:WorldState = null
	var cache:Array[Int] = null
	override def stopTerm:Int = GenUtils.STOP_INT
	override def output(in:(EventHistory,WorldState)):Int = {
		val index:Int = in._1.eventHist.length	//hijack the length of the array as the index
		val ws:WorldState = in._2
		val events:Array[Int] = if(ws eq cacheCond){ cache } else eventFn(ws)
		cacheCond = ws; cache = events
		var e = if(index >= events.length) stopTerm else events(index)
		if(e != stopTerm && !outputOK(GenUtils.e2t(e,in._2))){
			val ws:WorldState = in._2
			//we're returning an event we can't handle!
			//default to first valid event
			foreach(ws.size+1, (candE:Int) => {
				if(outputOK(GenUtils.e2t(candE,ws))){
					e = candE
				}
			})
			if(!outputOK(GenUtils.e2t(e,ws))){
				throw new IllegalStateException("No events in this example have been seen before!")
			}
		}
		e
	}
	override def noHistory:(EventHistory,WorldState) 
		= ( new EventHistory(new Array[Int](0),Array[String]()), null )
	override def feedback(in:(EventHistory,WorldState),out:Int):(EventHistory,WorldState) = {
		val newHist:Array[Int] = Array.concat( in._1.eventHist, Array(out) )
		( new EventHistory(newHist, in._1.genSoFar), in._2 )
	}
	override def outputToString(in:(EventHistory,WorldState), out:Int):String 
		= if(out == stopTerm) "[[STOP]]" else GenUtils.tstr( in._2.events(out).t )
	override def toString:String = "StrictEventChoice"
}






/*
*					--------------------
*						FIELD CHOICE
*					--------------------
*
*/
case class FieldHistory(fieldHist:Array[Int], parentEvent:Int, genSoFar:Array[Int]) extends ChoiceHistory{
	def this(fieldHist:Array[Int],parentEvent:Int) = this(fieldHist,parentEvent,new Array[Int](0))
	override def tooLong() = {
		fieldHist.size > 100
	}
	// parentEvent is an event index
	override def toString() = {
		"(e=" + parentEvent + ")[" + GenUtils.arrayToString(fieldHist) + "]"
		/*
		val b = new java.lang.StringBuilder
		b.append("FH:e=").append(parentEvent).append(":")
		b.append("[ ")
		fieldHist.foreach((f:Int) => b.append("f=").append(f).append(" "))
		b.append("]")
		b.toString
		*/
	}
}
case class FieldChoice(t:Int, factory:ProbabilisticClassifierFactory[FieldHistory,Feature,Int], opts:induction.Options) 
	extends Event3Choice[FieldHistory,Int](factory) {	
	
	def fieldIndexToPossibleOutputIndex(f:Int):Int = if(f==stopTerm) GenUtils.dead_f(t) else f

	override def stopTerm = GenUtils.STOP_INT
	override def noHist = new FieldHistory(new Array[Int](0), GenUtils.fail_e, new Array[Int](0))

	override def extractInputs(sent:AlignedSentence, ws:WorldState, add:(FieldHistory,Int)=>Any) : Unit = {		
		//add(FieldHistory, Field Index in PossibleOutputs) => Any
		//--Add all sub-histories
		def addAll(fields:Array[Int], e:Int) = {
			if(fields.size == 0) throw new java.lang.IllegalArgumentException("No stop term in sequence")
			foreach(fields.size, (end:Int) => {
				val fieldHist = new Array[Int](end)
				foreach(fieldHist.size, (i:Int) => {fieldHist(i) = fields(i)})
				val input:FieldHistory = new FieldHistory(fieldHist, e)
				val output = fieldIndexToPossibleOutputIndex(fields(end))
				add(input, output)
			})
		}
		
		//--Recursive Digest Function
		def digest(index:Int, lastE:Int):List[Int] = {
			if(index >= sent.observedEvents.size){
				//(base case)
				List[Int](fieldIndexToPossibleOutputIndex(GenUtils.STOP_INT))
			}else{
				val etype:Int = sent.eventTypes(index)
				val e:Int = sent.observedEvents(index)
				val f:Int = sent.fields(index)
				val recList:List[Int] = digest(index + 1, e)
				if(e != lastE){
					//(event boundary)
					if(etype == t){
						// add the sequence
						addAll( (if(f==recList.head) recList else f::recList).toArray, e)
					}
					List[Int](fieldIndexToPossibleOutputIndex(GenUtils.STOP_INT))
				}else{
					//(not an event boundary)
					val nextF:Int = recList.head
					if(nextF==f) 	recList
					else			f :: recList
				}
			}
		}
		//--Start Things Off
		if( digest(0, sent.observedEvents(0)-1).size > 1 ) //guarantee a miss on the first go
			throw new java.lang.IllegalStateException("Fields did not extract properly")
	}
	

	override def inputFeatures(in:FieldHistory, out:Int, ws:WorldState):Counter[Feature] = {
		val f:Counter[Feature] = new Counter[Feature]
		val fieldHist = in.fieldHist
		val parent = in.parentEvent
		def markovTail(fieldHist:Array[Int], f:Counter[Feature]) = {
			var tail:Int = opts.featFieldTail
			var fieldI = fieldHist.size - 1
			var tailList = List[Int]()
			while(tail > 0){
				if(fieldI < 0) tailList = GenUtils.start_f :: tailList
				else tailList = fieldHist(fieldI) :: tailList
				fieldI -= 1
				tail -= 1
				if(opts.featFieldAllTails) f.incrementCount(new FieldMarkovTail(tailList, t), 1.0)
			}
			if(!opts.featEventAllTails) f.incrementCount( new FieldMarkovTail(tailList, t), 1.0 )
		}
		
		// Last field type
		if(opts.featFieldMarkov){
			markovTail(fieldHist,f)
			//f.incrementCount( "" + {if(fieldHist.size==0) "no" else fstr(fieldHist(fieldHist.size-1))} + "-last", 1.0)
		}

		// Last field value 
		if(opts.featFieldLastVal && fieldHist.size > 0){
			val lastF:Int = fieldHist(fieldHist.size-1)
			if(lastF != GenUtils.none_f(t)){
				val lastV:Int = ws.events(parent).fields(lastF).value
				RangeChecker.addFeaturesDynamic(
					parent,
					lastF,
					lastV,
					f.asInstanceOf[Counter[Object]],
					"last-value-in-f=" + GenUtils.fstr(t,lastF),
					opts.featFieldVal,
					ws)
			}else{
				f.incrementCount(new StrFeature("last-value-in-f=none"), 1.0)
			}
		} else if(opts.featFieldLastVal){
			f.incrementCount(new StrFeature("last-value-no"), 1.0)
		}
		return f
	}

	override def outputFeatures(in:FieldHistory, f:Int, ws:WorldState):Counter[Feature] = {
		val feats:Counter[Feature] = new Counter[Feature]
		if(opts.featFieldMarkov){
			if(f == GenUtils.STOP_INT) {
				// Stop Term
				feats.incrementCount(new FieldMarker(t,GenUtils.STOP_INT), 1.0)
			} else if(f == GenUtils.none_f(t)) {
				// None Event
				feats.incrementCount(new FieldMarker(t,GenUtils.none_f(GenUtils.e2t(in.parentEvent,ws))), 1.0)
			} else {
				//Field Type
				feats.incrementCount(new FieldMarker(t,f), 1.0)
			}
		}
		return feats
	}

	override def jointFeatures(in:FieldHistory, f:Int, ws:WorldState):Counter[Feature] = {
		val fieldHist = in.fieldHist
		val parent = in.parentEvent
		val feats:Counter[Feature] = new Counter[Feature]

		// Field seen before
		if( f != GenUtils.STOP_INT && opts.featFieldRep ){
			val seenCount:Int = fieldHist.filter( (x:Int) => {x==f} ).size
			feats.incrementCount(new RepeatedField(t), seenCount)
		}
		//Field value
		if(f != GenUtils.STOP_INT && f != GenUtils.none_f(t)) {
				val event = ws.events(parent)
				val field = ws.events(parent).fields(f)
				RangeChecker.addFeaturesDynamic(	
									parent, 
									f, 
									field.value, 
									feats.asInstanceOf[Counter[Object]], 
									"[field-value]:" + GenUtils.fstr(t,f), 
									opts.featFieldVal, 
									ws)
		}
		return feats
	}
	
	// defined as possibleOutputs(outputIndex) = eventIndex
	override def possibleOutputs(in:FieldHistory, ws:WorldState):Array[Int] = {
		val rtn = (0 to (GenUtils.dead_f(t))).toArray
		rtn(GenUtils.dead_f(t)) = GenUtils.STOP_INT
		rtn
	}
	
	override def back(in:FieldHistory, out:Int, ws:WorldState) : FieldHistory = {
		val append = new Array[Int](1)
		append(0) = out
		new FieldHistory(Array.concat(in.fieldHist, append), in.parentEvent)
	}

	override def outputToString(in:FieldHistory, out:Int, ws:WorldState) : String = {
		//(get value)
		val e = in.parentEvent
		val v = if(out==GenUtils.STOP_INT || out==GenUtils.none_f(t)) -1 else ws.events(e).fields(out).value
		
		if(out==GenUtils.STOP_INT) "STOP" 
		else if(out==GenUtils.none_f(t)) "(none_f)" 
		else {	if(GenUtils.fstr(t,out).size>10) {GenUtils.fstr(t,out).substring(0,7) + "..."} 
				else GenUtils.fstr(t,out)} + " = " +  v
	}
	
	override def toString:String = "FieldChoice(" + t + ")" 
}



/*
*					------------------------
*						NONE_FIELD CHOICE
*					------------------------
*
*/
case class NoneFieldChoice(opts:induction.Options) extends AChoice[(FieldHistory,WorldState),Int] {
	override def stopTerm:Int = GenUtils.STOP_INT
	override def output(in:(FieldHistory,WorldState)):Int 
		= if(in._1.fieldHist.size > 0) stopTerm else GenUtils.none_f(GenUtils.none_t)
	override def noHistory:(FieldHistory,WorldState) 
		= (new FieldHistory(new Array[Int](0), -523), null) //only important part is the length 0 array
	override def feedback(in:(FieldHistory,WorldState),out:Int):(FieldHistory,WorldState) 
	= (new FieldHistory(new Array[Int](1), -523), in._2)	//only important part is the length 1 array
	override def outputToString(in:(FieldHistory,WorldState), out:Int):String = "---"
	override def toString:String = "NoneFieldChoice"
}




/*
*					--------------------
*						WORD CHOICE
*					--------------------
*
*/
case class WordHistory(wordHist:Array[Int], parentField:Int, parentEvent:Int, wordsGenerated:Int) extends ChoiceHistory{
	override def tooLong() = {
		wordHist.size > 100
	}
	// parentEvent is an event index
	override def toString() = {
		"(e=" + parentEvent + ",f=" + parentField + ")[" + GenUtils.arrayToString(wordHist) + "]"
		/*
		val b = new java.lang.StringBuilder
		b.append("WH:e=").append(parentEvent).append(":f=").append(parentField).append(":")
		b.append("[ ")
		wordHist.foreach((e:Int) => b.append("w=").append(e).append(" "))
		b.append("]")
		b.toString
		*/
	}
}
case class WordChoice(
		lm:((Array[Int],Int)=>Double),
		factory:ProbabilisticClassifierFactory[WordHistory,Feature,Int],
		opts:induction.Options
		)
	extends Event3Choice[WordHistory,Int](factory) {	
	
	def wordIndexToPossibleOutputIndex(w:Int,ws:WorldState,e:Int,f:Int):Int 
		= if(w==stopTerm) GenUtils.W else w

	override def stopTerm = GenUtils.STOP_INT
	override def noHist = new WordHistory(new Array[Int](0), GenUtils.start_f, GenUtils.start_t, 0)

	override def extractInputs(sent:AlignedSentence, ws:WorldState, add:(WordHistory,Int)=>Any) : Unit = {
		//add(FieldHistory, Field Index in PossibleOutputs) => Any
		//--Add a sequence of words (last word is the output)
		def addAll(e:Int, f:Int, start:Int, stop:Int) = {
			foreach(start, stop, (end:Int) => {
				val s:Int = Math.max(0, end-opts.numGrams)
				val wordHist:Array[Int] = new Array[Int](end-s)
				foreach(s, end, (i:Int) => {wordHist(i-s) = sent.words(i)})
				val out:Int = wordIndexToPossibleOutputIndex(sent.words(end), ws, e, f)
				val input:WordHistory = new WordHistory(wordHist, f, e, end-start)
				add(input, out)
			})
			val wordHist = new Array[Int](stop)
			foreach(stop, (i:Int) => {wordHist(i) = sent.words(i)})
			val stopHist = new WordHistory(wordHist, f, e, stop-start)
			add(stopHist, wordIndexToPossibleOutputIndex(GenUtils.STOP_INT, ws, e, f))
		}
		//--Scroll through the sentence
		def digest(index:Int):Any = {
			val e:Int = sent.observedEvents(index)
			val f:Int = sent.fields(index)
			val w:Int = sent.words(index)
			def findEnd(i:Int):Int = {
				if(i < sent.observedEvents.size){
					val nE:Int = sent.observedEvents(i)
					val nF:Int = sent.fields(i)
					val nW:Int = sent.words(i)
					if(nE != e || nF != f) i
					else findEnd(i+1)
				}else{
					i
				}
			}
			val end:Int = findEnd(index+1)
			addAll(e, f, index, end)
			if(end < sent.observedEvents.size) digest(end)
		}
		//--Start things off
		digest(0)
	}
	
	

	override def inputFeatures(in:WordHistory, out:Int, ws:WorldState):Counter[Feature] = {
		val parentF:Int = in.parentField
		val parentE:Int = in.parentEvent
		val parentT:Int = GenUtils.e2t(parentE, ws)
		val f:Counter[Feature] = new Counter[Feature]

		// Parent event type
		if(opts.featWordPEvent)
			f.incrementCount( new WordParentEvent(parentT), 1.0)
		
		// Parent field type
		if(opts.featWordPField)
			f.incrementCount( new WordParentField(parentT, parentF), 1.0)

		if(opts.featWordMarkov){
			if(in.wordHist.size > 0)
				f.incrementCount( new WordMarkov(parentT, parentF, in.wordHist(in.wordHist.size-1)), 1.0)
			else
				f.incrementCount( new WordMarkov(parentT, parentF, GenUtils.start_w), 1.0 )
		}

		// Field value
		if(parentE != GenUtils.none_e(ws) && parentF < ws.events(parentE).fields.size){
			val field:Field = ws.events(parentE).fields(parentF)
			RangeChecker.addFeaturesDynamic(	
								parentE, 
								parentF, 
								field.value, 
								f.asInstanceOf[Counter[Object]],
								"[word-value]:", 
								opts.featWordVal, 
								ws)
		}
		
		def isNoneF(f:Int) = {
			parentF == ws.events(parentE).fields.size
		}
		def isNoneE(e:Int) = {
			parentE < 0 || parentE >= ws.events.size
		}
		if(!isNoneE(parentE) && isNoneF(parentF)){
			val event:Event = ws.events(parentE)
			event.fields.foreach( (field:Field) => {
				val fI = field.f
				RangeChecker.addFeaturesDynamic(	
									parentE, 
									fI, 
									field.value, 
									f.asInstanceOf[Counter[Object]], 
									"[w-none_f-from-value]:", 
									opts.featWordEventVal, 
									ws)
			})
		}

		return f
	}

	override def outputFeatures(in:WordHistory, w:Int, ws:WorldState):Counter[Feature] = {
		val f:Counter[Feature] = new Counter[Feature]
		if(w == GenUtils.STOP_INT) {
			// Stop Term
			f.incrementCount(new WordMarker(GenUtils.STOP_INT), 1.0)
		} else {
			// Word Type
			f.incrementCount(new WordMarker(w), 1.0)
		}
		return f
	}
	
	override def jointFeatures(in:WordHistory, w:Int, ws:WorldState):Counter[Feature] = {
		val parentF:Int = in.parentField
		val parentE:Int = in.parentEvent
		val parentT:Int = GenUtils.e2t(parentE,ws)
		val f:Counter[Feature] = new Counter[Feature]
		// Language Model
		if(w != GenUtils.STOP_INT && opts.featWordLM) {
			val lmscore:Double = Math.log( lm(in.wordHist, w) )
			//val lmscore:Double = lm(in.wordHist, w)
			f.incrementCount(new LMScore(), lmscore * opts.lmFactor)
		}
		// Word Length
		if(opts.featWordLen && in.wordsGenerated > 0){
			if(w != GenUtils.STOP_INT){
				f.incrementCount(new WordLength(parentT,parentF,in.wordsGenerated,false), 1.0)	//generate some word given x words generated
			}else{
				f.incrementCount(new WordLength(parentT,parentF,in.wordsGenerated,true), 1.0)	//generate stop given x words generated
			}
		}
		return f
	}

	// defined as possibleOutputs(outputIndex) = eventIndex
	override def possibleOutputs(in:WordHistory, ws:WorldState):Array[Int] = {
		val rtn = (0 to (GenUtils.W)).toArray
		rtn(GenUtils.W) = GenUtils.STOP_INT
		return rtn
	}
	
	override def back(in:WordHistory, out:Int, ws:WorldState) : WordHistory = {
		if(out == stopTerm) throw new java.lang.IllegalArgumentException("Cannot feedback stop term")
		//--Efficient version
		val hist = new Array[Int]( if(in.wordHist.length >= opts.numGrams-1) opts.numGrams-1 else in.wordHist.length+1 )
		hist(hist.length-1) = out
		foreach(hist.length-1, (i:Int) => hist(hist.length-i-2) = in.wordHist(in.wordHist.length-i-1) )
		new WordHistory(hist, in.parentField, in.parentEvent, in.wordsGenerated+1)
		//--Safe version
		//val append = new Array[Int](1)
		//append(0) = out
		//new WordHistory(Array.concat(in.wordHist, append), in.parentField, in.parentEvent, in.wordsGenerated+1)
	}

	override def outputToString(in:WordHistory, out:Int, ws:WorldState) : String = {
		if(out==GenUtils.STOP_INT) "STOP" 
		else GenUtils.wstr(out)
	}

	override def toString:String = "WordChoice"
}


/*
*					-----------------------
*						FIELD SET CHOICE
*					-----------------------
*
*/
case class FieldSetHistory(eSoFar:Array[Int], parentE:Int, isFirst:Boolean, genSoFar:Array[String], generated:Boolean) 
		extends ChoiceHistory{
	override def tooLong:Boolean = generated
	override def toString = "[field set history]: generated=" + generated + " genSoFar=" + GenUtils.arrayToString(genSoFar)
}


case class FieldSetChoice(
		t:Int, 
		templates:Array[Template],
		factory:ProbabilisticClassifierFactory[FieldSetHistory,Feature,FieldSet], 
		lm:LanguageModel,
		opts:induction.Options	) 
	extends Event3Choice[FieldSetHistory,FieldSet](factory) {	

	override def stopTerm = GenUtils.STOP_FS
	override def noHist = new FieldSetHistory(new Array[Int](0),-1,true,Array[String](),false)
	
	//--Force single generation
	override def getProbabilities(in:(FieldSetHistory,WorldState)) : Counter[FieldSet] = {
		//println("fsHistory=" + in._1);
		if(in._1.generated){
			val rtn = new Counter[FieldSet]()
			rtn.setCount(stopTerm, 1.0)
			rtn
		}else{
			val cand:Counter[FieldSet] = super.getProbabilities(in)
			cand.setCount(stopTerm, 0.0)
			cand.normalize
			cand
		}
	}

	override def extractInputs(sent:AlignedSentence, ws:WorldState, add:(FieldSetHistory,Int)=>Any) : Unit = {		
		var isFirst = true
		var genSoFar:Array[String] = Array[String]()
		var eSoFar:Array[Int] = new Array[Int](0)
		Template.alignedSentence2Templates(sent,ws)
			.filter( (pair:(Template,Int,Array[String])) => pair._1.getT == t )	//keep only the templates in this event type
			.foreach( (pair:(Template,Int,Array[String])) => {
				val t = pair._1.getT
				val m = pair._1 //we only care about the fieldset, not the template
				val hist:FieldSetHistory = new FieldSetHistory(eSoFar, pair._2, isFirst, genSoFar, false)
				isFirst = false
				genSoFar = Array.concat(genSoFar, pair._3)
				eSoFar = Array.concat(eSoFar, Array(pair._2))
				val fs = registerOut(m.fieldSet)
				val index:Int = possibleOutputs(hist,ws).indexOf(fs)
				add(
					hist,
					index
				) //add each fieldset
			})
	}
	

	override def inputFeatures(in:FieldSetHistory, fs:FieldSet, ws:WorldState):Counter[Feature] = {
		val feats:Counter[Feature] = new Counter[Feature]
		//--Field Values
		if(t != GenUtils.none_t && fs != stopTerm){
			if(fs == null || fs.fields == null) throw new IllegalStateException("BOOO: " + fs)
			fs.fields.filter( (f:Int) => f != GenUtils.none_f(t)).foreach( (f:Int) => {
				//(for each field in template)
				val field:Field = ws.events(in.parentE).fields(f)
				RangeChecker.addFeaturesDynamic(	
								in.parentE, 
								f, 
								field.value, 
								feats.asInstanceOf[Counter[Object]],
								"[fieldSet-value]:", 
								opts.featFSVal, 
								ws)
			})
		}
		//--IsFirst
		if(opts.featFSFirst){
			feats.incrementCount(new StrFeature("is-first"), 1.0)
		}
		return feats
	}

	override def outputFeatures(in:FieldSetHistory, fs:FieldSet, ws:WorldState):Counter[Feature] = {
		val feats:Counter[Feature] = new Counter[Feature]
		feats.incrementCount(new FSMarker(fs), 1.0) //marker
		return feats
	}

	override def jointFeatures(in:FieldSetHistory, fs:FieldSet, ws:WorldState):Counter[Feature] = {
		val feats:Counter[Feature] = new Counter[Feature]
		//--Prior
		if(opts.featTemplPrior){
			feats.incrementCount(new FSMarker(fs), 1.0)
		}
		return feats
	}
	
	var possibleOuts:Array[FieldSet] = Array[FieldSet](stopTerm) //note: stop term is first possible output
	def fieldSetOutputs:Array[FieldSet] = possibleOuts.subArray(1,possibleOuts.length)
	def registerOut(fs:FieldSet):FieldSet = {
		if(possibleOuts contains fs) fs
		else {
			possibleOuts = Array.concat(possibleOuts, Array[FieldSet](fs))
			fs
		}
	}
	def isEmpty:Boolean = (possibleOuts.length == 1)
	override def possibleOutputs(in:FieldSetHistory, ws:WorldState):Array[FieldSet] = {
		if(isEmpty) {
			//throw new IllegalStateException("No Field Set outputs for t=" + GenUtils.tstr(t))
		}
		possibleOuts
	}
	
	override def back(in:FieldSetHistory, out:FieldSet, ws:WorldState) : FieldSetHistory = {
		if(in.generated) throw new java.lang.IllegalArgumentException("calling feedback on a generated template history")
		//println("\tFS back: " + GenUtils.arrayToString(in.genSoFar))
		new FieldSetHistory(in.eSoFar, in.parentE,in.isFirst,in.genSoFar,true)
	}

	override def outputToString(in:FieldSetHistory, out:FieldSet, ws:WorldState) : String = {
		if(out == stopTerm) "STOP" else out.toString
	}
	
	override def toString:String = "FieldSetChoice(" + t + ")" 
}

	





/*
*					----------------------
*						TEMPLATE CHOICE
*					----------------------
*
*/
case class TemplateHistory(eSoFar:Array[Int], parentE:Int, genSoFar:Array[String], generated:Boolean) extends ChoiceHistory{
	override def tooLong:Boolean = generated
	override def toString = "[template history]: generated=" + generated + ", parentE=" + parentE + " genSoFar=" + GenUtils.arrayToString(genSoFar)
}

object TemplateChoice {
	val stopTerm = GenUtils.STOP_TEMPLATE
}

case class TemplateChoice(
		t:Int, 
		templates:Array[Template],
		fieldSet:FieldSet,
		factory:ProbabilisticClassifierFactory[TemplateHistory,Feature,Template], 
		lm:LanguageModel,
		opts:induction.Options	) 
	extends Event3Choice[TemplateHistory,Template](factory) {	

	override def stopTerm = TemplateChoice.stopTerm
	override def noHist = new TemplateHistory(new Array[Int](0),-1,Array[String](), false)
	
	//--Force single template generation
	override def getProbabilities(in:(TemplateHistory,WorldState)) : Counter[Template] = {
		//println("template: " + in._1)
		if(in._1.generated){
			val rtn = new Counter[Template]()
			rtn.setCount(stopTerm, 1.0)
			rtn
		}else{
			val cand:Counter[Template] = super.getProbabilities(in)
			cand.setCount(stopTerm, 0.0)
			cand.normalize
			cand
		}
	}

	override def extractInputs(sent:AlignedSentence, ws:WorldState, add:(TemplateHistory,Int)=>Any) : Unit = {		
		def trim(x:Array[String]):Array[String] = {
			if(x.length > opts.numGrams) x.subArray(x.length-opts.numGrams, x.length)
			else x
		}
		var genSoFar:Array[String] = Array[String]()
		var eSoFar = new Array[Int](0)
		val templates = Template.alignedSentence2Templates(sent,ws)
		templates.foreach( (pair:(Template,Int,Array[String])) => { 
			//(variables)
			val m:Template = pair._1
			val e:Int = pair._2
			val str = pair._3
			//(add if applicable)
			if(m.getT == t && m.fieldSet.equals(fieldSet)) {
				val hist:TemplateHistory = new TemplateHistory(eSoFar, e, trim(genSoFar), false)
				//println(GenUtils.arrayToString(genSoFar) + "  ->  " + GenUtils.arrayToString(str))
				add(
					hist,
					possibleOutputs(hist,ws).indexOf(m)
					)
			}
			genSoFar = Array.concat(genSoFar, str)
			eSoFar = Array.concat(eSoFar, Array(e))
		})
		/*
		Template.alignedSentence2Templates(sent,ws)
			.filter( (pair:(Template,Int,Array[String])) => pair._1.getT == t )	//keep only the templates in this event type
			.filter( (pair:(Template,Int,Array[String])) => pair._1.fieldSet equals fieldSet )
			.foreach( (pair:(Template,Int,Array[String])) => {
				val hist:TemplateHistory = new TemplateHistory(eSoFar, pair._2, trim(genSoFar), false)
				if(templates contains pair._1) {
					add(
						hist,
						possibleOutputs(hist,ws).indexOf(pair._1) 
						) //add each template
				}else{
					logs("Skipping template: " + pair._1)
				}
				//(log for language model no matter what)
				genSoFar = Array.concat(genSoFar, pair._3)
				eSoFar = Array.concat(eSoFar, Array(pair._2))
			})
			*/
	}

	override def inputFeatures(in:TemplateHistory, m:Template, ws:WorldState):Counter[Feature] = {
		val feats:Counter[Feature] = new Counter[Feature]
		//--Field Values
		if(t != GenUtils.none_t && m != stopTerm){
			fieldSet.fields.filter( (f:Int) => f != GenUtils.none_f(t)).foreach( (f:Int) => {
				//(for each field in template)
				val field:Field = ws.events(in.parentE).fields(f)
				RangeChecker.addFeaturesDynamic(	
								in.parentE, 
								f, 
								field.value, 
								feats.asInstanceOf[Counter[Object]],
								"[template-value]:", 
								opts.featTemplVal, 
								ws)
			})
		}
		//--Field Diff
		if(opts.featTemplValDiff){
			val eHist:Array[Int] = in.eSoFar
			//(find last event of same type
			def findLastSameE(i:Int):Int = {
				if(i < 0) GenUtils.dead_e(ws)
				else if(GenUtils.e2t(eHist(i),ws) == t) eHist(i)
				else findLastSameE(i-1)
			}
			val lastSameE = findLastSameE(eHist.length-1)
			//(if exists, check it's value)
			if(lastSameE != GenUtils.dead_e(ws)){
					foreach(GenUtils.F(t), (f:Int) => {
					//(only applicable for numeric fields)
					if(ws.events(in.parentE).fields(f).isNumeric){
						val thisVal:Int = ws.events(in.parentE).fields(f).value
						val otherVal:Int = ws.events(lastSameE).fields(f).value
						//(check the two values)
						if( Math.abs(thisVal - otherVal) < opts.valDiffThreshold ){
							feats.incrementCount( new ValueDiff(t,f,0), 1.0 )
						} else if(thisVal < otherVal){
							feats.incrementCount( new ValueDiff(t,f,-1), 1.0 )
						} else {
							feats.incrementCount( new ValueDiff(t,f,1), 1.0 )
						}
					}
				})
			}
		}

		//--Return
		return feats
	}

	override def outputFeatures(in:TemplateHistory, m:Template, ws:WorldState):Counter[Feature] = {
		val feats:Counter[Feature] = new Counter[Feature]
		feats.incrementCount(new TemplateMarker(m), 1.0)
		//--Hierarchical Templates
		if(opts.featTemplHierarchical){
			m.parents.foreach( (parent:Template) => {
				feats.incrementCount(new ParentTemplate(parent), 1.0)
			})
		}
		return feats
	}

	override def jointFeatures(in:TemplateHistory, m:Template, ws:WorldState):Counter[Feature] = {
		val feats:Counter[Feature] = new Counter[Feature]
		//--Prior
		if(opts.featTemplPrior){
			feats.incrementCount(new TemplateMarker(m), 1.0)
		}
		//--Length
		if(opts.featTemplLength){
			feats.incrementCount(new LengthMarker(m.size), 1.0)
		}
		//--Language Model
		if(opts.featTemplLM && m != stopTerm){
			//try { 
				val hist:Array[String] = in.genSoFar
				val word:String = m.firstWord(in.parentE, ws)
				var lmScore:Double = Math.log( lm.nextWordProb(word, hist.toList) )
				if(lmScore < -1000){
					println("[WARNING]: Language Model Underflow")
					lmScore = -5
				}
				/*
				//different feature for lmFirst and lmMiddle
				val lowBound = GenUtils.lmStats.getMean - GenUtils.lmStats.getStandardDeviation
				val upBound = GenUtils.lmStats.getMean + GenUtils.lmStats.getStandardDeviation
				val str:String = 
					if(hist.length == 0){
						if(lmScore < lowBound) "lm-init-veryLow"
						else if(lmScore > upBound) "lm-init-veryHigh"
						else "lm-init"
					} else {
						if(lmScore < lowBound) "lm-normal-veryLow"
						else if(lmScore > upBound) "lm-normal-veryHigh"
						else "lm-normal"
					}
				feats.incrementCount(new StrFeature(str), 
					Math.pow( lmScore, opts.lmFactor) ) 
				*/
				feats.incrementCount(new StrFeature("lm-score"), Math.pow( lmScore, opts.lmFactor) )
			//} catch {
			//	case (e:Exception) => { 	e.printStackTrace();}
			//}
		}
		return feats
	}
	
	var possibleOuts:Array[Template] = null
	override def possibleOutputs(in:TemplateHistory, ws:WorldState):Array[Template] = {
		if(possibleOuts == null) {
			possibleOuts = Array.concat(	templates.filter( (m:Template) => {
												fieldSet equals m.fieldSet
											}), 
											Array[Template](stopTerm)  )
			if(possibleOuts.length == 1) throw new IllegalStateException("Only stop term registered in outputs")
		}
		possibleOuts
	}
	
	override def back(in:TemplateHistory, out:Template, ws:WorldState) : TemplateHistory = {
		if(out == stopTerm) throw new java.lang.IllegalArgumentException("calling feedback on a generated template history")
		//println("\t\tTemplate back: " + GenUtils.arrayToString(in.genSoFar))
		new TemplateHistory(in.eSoFar, in.parentE,in.genSoFar,true)
	}

	override def outputToString(in:TemplateHistory, out:Template, ws:WorldState) : String = {
		if(out == stopTerm) "STOP" else out.toString
	}
	
	override def toString:String = "TemplateChoice(" + t + ")" 
}


/*
*					---------------------------
*						TEMPLATE FILL CHOICE
*					---------------------------
*
*/

case class TemplateFillHistory(	parentE:Int, 
								parentTemplate:Template, 
								genSoFar:Array[String], 
								generated:Boolean) extends ChoiceHistory{
	override def tooLong:Boolean = generated
	override def toString = "[template fill history]: generated=" + generated + " genSoFar=" + GenUtils.arrayToString(genSoFar)
}
object TemplateFillChoice {
	val stopTerm = (GenUtils.STOP_INT,GenUtils.STOP_INT_ARRAY,GenUtils.STOP_STR_ARRAY)
}
case class TemplateFillChoice(opts:induction.Options) 
		extends AChoice[(TemplateFillHistory,WorldState),(Int,Array[Int],Array[String])] {
	override def stopTerm:(Int,Array[Int],Array[String]) = TemplateFillChoice.stopTerm
	override def output(in:(TemplateFillHistory,WorldState)):(Int,Array[Int],Array[String]) ={
		//println("fill: " + in._1)
		if(in._1.generated) {
			stopTerm 
		} else {
			val e = in._1.parentE
			val filled:(Array[Int],Array[String]) = in._1.parentTemplate.fill(e, in._2)
			val fields:Array[Int] = filled._1
			val words:Array[String] = filled._2
			(e, fields, words)
		}
	}
	override def noHistory:(TemplateFillHistory,WorldState) 
		= (new TemplateFillHistory(	-1339, 
									Template.defaultTemplate(GenUtils.none_t), 
									Array[String](),
									false), 
			null)
	override def feedback(	in:(TemplateFillHistory,WorldState),
							out:(Int,Array[Int],Array[String])
						):(TemplateFillHistory,WorldState) = {
		( new TemplateFillHistory(	in._1.parentE, 
								Template.defaultTemplate(GenUtils.none_t),
								Array.concat(in._1.genSoFar, out._3),
								true), 
			in._2)
	}
	override def outputToString(in:(TemplateFillHistory,WorldState), out:(Int,Array[Int],Array[String])):String 
		= GenUtils.arrayToString(out._3)
	override def toString:String = "TemplateFillChoice"
}
