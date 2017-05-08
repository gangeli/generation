package generation

import tea.Utils.{map,foreach,fails,fail}
import edu.berkeley.nlp.util.Counter
import AlignedDataset.AlignedSentence
import org.goobs.utils.Encodable

object FieldType {
	val NUMERIC:Int = 0
	val CATEGORICAL:Int = 1
	val SYMBOL:Int = 3
	val STRING:Int = 4
	val NONE:Int = -1
}

object WorldState{
	val types = new java.util.HashMap[(Int,Int),Int]
	def isNumeric(t:Int, f:Int) = {
		if(!types.containsKey((t,f))){
			false
		} else {
			types.get((t,f)) == FieldType.NUMERIC
		}
	}
	def digest(ws:WorldState) = {
		ws.events.foreach( (event:Event) => {
			event.fields.foreach( (field:Field) => {
				val key = (event.t, field.f)
				if(types.containsKey(key) && types.get(key) != field.fieldType)
					throw new IllegalStateException("Field types don't match")
				else types.put(key, field.fieldType)
			})
		})
	}

	def friendlyFilter(word:String) = {
		if( word equals "---" ) "none"
		else if( word equals "corner_kick_l" ) "corner kick (left)"
		else if( word equals "corner_kick_r" ) "corner kick (right)"
		else if( word equals "wind_10m" ) "wind at 10m"
		else if( word equals "mode-bucket-0-20-2" ) "bucket[0-20]"
		else if( word equals "mode-bucket-0-100-4" ) "bucket[0-100]"
		else word
	}
}

case class Field(f:Int, value:Int, fieldType:Int){
	def featureValue = value
	def isString = (fieldType == FieldType.STRING)
	def isNumeric = (fieldType == FieldType.NUMERIC)
	def isSymbol = (fieldType == FieldType.SYMBOL)
	def isCategorical = (fieldType == FieldType.CATEGORICAL)
}

case class Event(t:Int, fields:Array[Field]){
	override def equals(o:Any):Boolean = {
		if(o.isInstanceOf[Event]){
			(t == o.asInstanceOf[Event].t) && (fields deepEquals o.asInstanceOf[Event].fields)
		} else {
			false
		}
	}
	override def toString:String = {
		val sb:StringBuilder = new StringBuilder
		sb.append(GenUtils.tstr(t)).append("\t")
		//(append fields)
		fields.foreach( (field:Field) => {
			sb.append(GenUtils.fstr(t)(field.f)).append(":")
			sb.append(GenUtils.vstr(t,field.f,field.value)).append("\t")
		})
		sb.toString
	}
}

case class WorldState(events:Array[Event]) extends Encodable{
	def size = events.size
	
	def toHtml:String = {
		var maxSpan = 0
		foreach(events.size, (e:Int) => {
			maxSpan = Math.max(maxSpan, events(e).fields.length)
		})
		var s:String = """<table class="WorldState">"""
		s += """<tr><td class="wsHeader">Event</td><td class="wsHeader" colspan=" """ + maxSpan + """ ">Fields</td></tr>"""
		events.filter( (e:Event) => e.t != GenUtils.none_t).foreach( (e:Event) => {
			s += """<tr>"""
			s += """<td class="wsBody">""" + WorldState.friendlyFilter(GenUtils.tstr(e.t)) + """</td>"""
			var first = true
			e.fields.filter( (f:Field) => f.f != GenUtils.none_f(e.t)).foreach( (f:Field) => {
				s += """<td class="wsBody">""" + WorldState.friendlyFilter(GenUtils.fstr(e.t,f.f))
				s += "=" + WorldState.friendlyFilter( GenUtils.vstr(e.t,f.f,f.value) ) + """</td>"""
				first = false
			})
			//fill the rest
			foreach( e.fields.length, maxSpan, (i:Int) => {
				s += """<td class="wsBody"> </td>"""
			})
			s += """</tr>"""
		})
		s += """</table>"""
		s
	}
	
	override def toString:String = {
		val sb:StringBuilder = new StringBuilder
		//(for each event)
		events.foreach( (event:Event) => {
			sb.append(event.toString).append("\n")
		})
		return sb.toString
	
	}

	override def equals(o:Any):Boolean = {
		if(o.isInstanceOf[WorldState]){
			val rtn = (events deepEquals o.asInstanceOf[WorldState].events)
			rtn
		}else{
			false
		}
	}

	override def hashCode:Int = {
		var hc = events.size
		if(events.size > 0)
			hc = hc ^ events(0).t*42
		hc
	}
}

