package generation

import java.io._
import org.goobs.utils.Encodable
import tea.Utils.{map,foreach,fails}

case class AlignedDataset(size:Int) extends Serializable{
	import AlignedDataset.AlignedSentence	
	val aligns:Array[AlignedSentence] = new Array[AlignedSentence](size)
	val worldStates:Array[WorldState] = new Array[WorldState](size)
	val trueEvents:Array[Array[Int]] = new Array[Array[Int]](size)

	val trueWords:Array[Array[String]] = new Array[Array[String]](size)	//used in evaluation
	
	def addAlignment(	index:Int,
						sent:AlignedDataset.AlignedSentence,
						goldEvents:Array[Int],
						goldWords:Array[String],
						ws:WorldState ) = {
		//println("---------")
		//println(ws)
		//println(GenUtils.arrayToString(goldWords))
		//println("---------")
		aligns(index) = sent
		trueEvents(index) = goldEvents
		trueWords(index) = goldWords
		worldStates(index) = ws
	}

	def addAlignment(	index:Int,
						tracks:Array[Int], 
						eventTypes:Array[Int], 
						fields:Array[Int], 
						words:Array[Int], 
						events:Array[Int],
						worldState:WorldState,
						trueEvent:Array[Int]) = {
		val size:Int = tracks.size
		//(size check)
		if(eventTypes.size != size || fields.size != size || words.size != size || events.size != size) {
			throw new java.lang.IllegalArgumentException("Sizes do not match")
		}
		aligns(index) = new AlignedSentence(tracks, eventTypes, fields, words, events)
		worldStates(index) = worldState
		trueEvents(index) = trueEvent
	}

	def saveAlignment(path:String) = {
		val objStream:ObjectOutputStream = new ObjectOutputStream(new FileOutputStream(path))
		objStream.writeObject(this)
		objStream.close()
	}
}

object AlignedDataset {
	case class AlignedSentence( tracks:Array[Int], 
								eventTypes:Array[Int], 
								fields:Array[Int], 
								words:Array[Int],
								observedEvents:Array[Int]	) extends Encodable{
		var strings:Array[String] = null
		
		
		def getWords:Array[String] = {
			if(strings != null) strings
			else words.map( (w:Int) => GenUtils.wstr(w))
		}

		def toHtml:String = {
			"""<p class=aligned_sentence>""" + GenUtils.arrayToString(getWords) + """</p>"""
		}

		override def toString:String = {
			//--Get Lengths
			var tLen:Int = 0
			var fLen:Int = 0
			foreach(observedEvents.length, (i:Int) => {
				val tStr:String = GenUtils.tstr(eventTypes(i))
				val fStr:String = GenUtils.fstr(eventTypes(i), fields(i))
				tLen = Math.max(tLen, (tStr + "(" + observedEvents(i) + ")").length)
				fLen = Math.max(fLen, fStr.length)
			})
			//--Print
			val sb:StringBuilder = new StringBuilder
			foreach(observedEvents.length, (i:Int) => {
				//(get strings)
				val tStr:String = GenUtils.tstr(eventTypes(i)) + "(" + observedEvents(i) + ")"
				val fStr:String = GenUtils.fstr(eventTypes(i), fields(i))
				val wStr:String = (getWords)(i)
				//(get empty spaces)
				val tJumps = (tLen+3 - tStr.length)
				val fJumps = (fLen+3 - fStr.length)
				//(append strings)
				sb.append(tStr)
				foreach(tJumps, (k:Int) => sb.append(" "))
				sb.append(fStr)
				foreach(fJumps, (k:Int) => sb.append(" "))
				sb.append(wStr)
				if(i != observedEvents.length-1) sb.append("\n") //no newline on last item
			})
			sb.toString
		}
		
		/*
		override def toString:String = {
			try{
			val sb:StringBuilder = new StringBuilder
			//sb.append(GenUtils.arrayToString(eventTypes.map(GenUtils.tstr(_)))).append("\n")
			sb.append(GenUtils.arrayToString(observedEvents)).append("\n")
			try{
				sb.append(GenUtils.arrayToString((0 to fields.size-1).map { i:Int =>
            		GenUtils.fstr(eventTypes(i))(fields(i)) }.toArray)).append("\n")
			} catch {
				case _ => sb.append(GenUtils.arrayToString(fields)).append("\n")
			}
			sb.append(GenUtils.arrayToString(words.map(GenUtils.wstr(_))))
			sb.toString
			} catch {
				case _ => "Error caught in AlignedSentence toString"
			}
		}
		*/

		override def equals(other:Any):Boolean = {
			if(other.isInstanceOf[AlignedSentence]){
				val as:AlignedSentence = other.asInstanceOf[AlignedSentence]
				if(! (tracks deepEquals as.tracks) ) return false;
				if(! (eventTypes deepEquals as.eventTypes) ) return false;
				if(! (fields deepEquals as.fields) ) return false;
				if(! (words deepEquals as.words) ) return false;
				if(! (observedEvents deepEquals as.observedEvents) ) return false;
				return true
			} else {
				return false
			}
		}

		override def hashCode:Int = {
			var hc:Int = 0;
			hc += eventTypes.size + 500*fields.size + 10000*words.size
			if(eventTypes.size > 0){
				hc = hc ^ eventTypes(0).hashCode
			}
			hc
		}
	}
}


