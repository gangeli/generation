package features;

import induction._
import generation._
import generation.AlignedDataset.AlignedSentence
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import org.goobs.utils._
import org.goobs.classify._
import org.goobs.choices._
import org.goobs.foreign._

case class WindDirIndicator(first:Boolean,windDir:String) extends Feature {
	override def toString:String = "winddir=" + windDir + {if(first) "-first" else ""}
}
case class GustIndicator() extends Feature{
	override def toString:String = "gusts-present"
}
case class WindDirChange(first:Boolean,change:String) extends Feature{
	override def toString:String = "wind-" + change + {if(first) "-first" else ""}
}
case class VariableWind(first:Boolean, variable:String) extends Feature{
	override def toString:String = "variable-" + variable + {if(first) "-first" else ""}
}
case class WindSpeedChange(first:Boolean,change:String) extends Feature{
	override def toString:String = "wind-speed-" + change + {if(first) "-first" else ""}
}



case class WordIndicator(hasLast:Boolean,w:String) extends Feature{
	override def toString:String = {if(!hasLast) "~" else ""} + "word=" + w
}
case class KeywordIndicator(hasLast:Boolean,w:String) extends Feature{
	override def toString:String = {if(!hasLast) "~" else ""} + "keyword=" + w
}
case class KeyphraseIndicator(hasLast:Boolean, p:Array[String]) extends Feature{
	override def toString:String = {if(!hasLast) "~" else ""} + "keyphrase=" + GenUtils.arrayToString(p)
}
case class SumtimeParentTemplate(hasLast:Boolean, m:Template) extends Feature{
	override def toString:String = {if(!hasLast) "~" else ""} + "parent=" + m
}
case class TemplateIndicator(hasLast:Boolean,m:Template) extends Feature{
	override def toString:String = {if(!hasLast) "~" else ""} + "template=" + m
}

object FeatureUtils {

	var INT_CACHE:Int = -123489
	
	/*
	val times:Array[String] = Array[String]( //note: the order is important
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
	*/
	val times:Array[String] = Template.times

	val keywords:Array[String] = Array[String](
		"veering",
		"backing",
		"increasing",
		"decreasing",
		"rising",
		"easing",
		"falling",
		"becoming",
		"gradually",
		"steadily",
		"slowly",
		"soon",
		"ly"
		)
	val keyphrases:Array[Array[String]] = Array[Array[String]](
		Array[String]("or", "less"),
		Array[String]("less", "than"),
		Array[String]("variable"),
		Array[String]("cyclonic", "variable")
		)

	val windDirs = new Circle().equidistantPoints( {
//the list of wind directions
val rawDirs:Array[String] = Array[String](
"N",

"NNE-N",
"NNE-NE",
"N-NNE",
"NNE",
"N-NE",

"NE",

"NE-NNE",
"NE-N",
"NE-E",
"NE-ENE",

"ENE",
"E-ENE",
"E-NE",
"ENE-NE",

"E",

"E-ESE",
"ESE",
"E-SE",
"ESE-E",
"ESE-SE",

"SE",

"SE-E",
"SE-ESE",
"SE-S",
"SE-SSE",

"SSE",
"S-SE",
"SSE-S",
"SSE-SE",
"S-SSE",

"S",

"S-SSW",
"SSW",
"S-SW",
"SSW-S",
"SW-S",
"SSW-SW",

"SW",

"SW-SSW",
"SW-W",
"SW-WSW",

"WSW-SW",
"WSW-W",
"WSW",
"W-SW",
"W-WSW",

"W",

"WNW",
"W-NW",
"WNW-W",
"WNW-NW",
"W-WNW",
"NW-W",

"NW",

"NW-WNW",
"N-NNW",
"NNW",
"N-NW",
"NNW-N",
"NW-N",
"NW-NNW")

//add 'mainly' modifier to each direction
var retLst = List[String]()
rawDirs.foreach( (dir:String) => {
	retLst = dir :: "M-" + dir :: retLst
})
retLst.reverse.toArray

})

	def str2f(str:String,t:Int):Int = {
		for( f <- 0 until GenUtils.F(t) ){
			if(GenUtils.fstr(t,f).equalsIgnoreCase(str)) return f
		}
		throw new IllegalArgumentException("No field of name: " + str);
	}
	
	def addValue(feats:Counter[Feature],e:Int,f:Int,ws:WorldState) = {
		val field:Field = ws.events(e).fields(f)
		RangeChecker.addFeaturesDynamic(	
						e, 
						f, 
						field.value, 
						feats.asInstanceOf[Counter[Object]],
						"[fs-value]:", 
						Options.ValueType.fuzzy, 
						ws)
	}

	val wsSave:HashSet[WorldState] = new HashSet[WorldState]
	val mHistSave:HashSet[TemplateHistory] = new HashSet[TemplateHistory]
	val cwCache:HashMap[(String,String),Boolean] = new HashMap[(String,String),Boolean]
	val ccwCache:HashMap[(String,String),Boolean] = new HashMap[(String,String),Boolean]
}

class SumtimeEventChoice(
	lm:LanguageModel, // the trained language model
	factory:org.goobs.classify.ProbabilisticClassifierFactory[EventHistory,Feature,Int],
		// ^needed to instantiate AChoice class
	outputOK:Int=>Boolean, // prohibit certain event types to avoid dead ends
	opts:induction.Options // the generation options
		) extends EventChoice(lm, factory, outputOK, opts) {	
	

	override def inputFeatures(in:EventHistory, e:Int, ws:WorldState):Counter[Feature] = {		
		val feats:Counter[Feature] = super.inputFeatures(in, e, ws)
		return feats
	}

	override def outputFeatures(in:EventHistory, e:Int, ws:WorldState):Counter[Feature] = {
		val feats:Counter[Feature] = super.outputFeatures(in, e, ws)
		return feats
	}
	override def jointFeatures(in:EventHistory, e:Int, ws:WorldState):Counter[Feature] = {
		val feats:Counter[Feature] = super.jointFeatures(in, e, ws)
		return feats
	}
}


class SumtimeFieldSetChoice(
	t:Int, // the parent event
	templates:Array[Template], // the templates for that event
	factory:ProbabilisticClassifierFactory[FieldSetHistory,Feature,FieldSet],
	lm:LanguageModel, // the language model
	opts:induction.Options	// the generation options
		) extends FieldSetChoice(t, templates, factory, lm, opts) {

	override def inputFeatures(in:FieldSetHistory, fs:FieldSet, ws:WorldState):Counter[Feature] = {
		val feats:Counter[Feature] = super.inputFeatures(in, fs, ws)

		val t = GenUtils.e2t(in.parentE,ws)
		val e:Int = in.parentE
		def careAbout(t:Int,f:Int) = {
			val str:String = GenUtils.fstr(t,f)
			val rtn:Boolean = 
				//str.equals("gust_min") ||
				//str.equals("gust_max") || 
				//str.equals("wind_dir") ||
				//str.equals("wind_min") ||
				//str.equals("wind_max") ||
				str.equals("time")
			rtn
			
		}
		//--Field Value
		if(t != GenUtils.none_t && fs != stopTerm){
			fs.fields.filter( (f:Int) => { f != GenUtils.none_f(t) && careAbout(t,f) })
			.foreach( (f:Int) => {
				//we get here for fields that are:
				// not in the none record
				// not the stop template
				// not the none field
				// in the careAbout() relation
				FeatureUtils.addValue(feats,in.parentE,f,ws)
			})
		}

		//--Gust Exists
		val gustMinF:Int = FeatureUtils.str2f("gust_min", t)
		val gustMaxF:Int = FeatureUtils.str2f("gust_max", t)
		if(ws.events(e).fields(gustMinF).value > 0){
			if(ws.events(e).fields(gustMaxF).value > 0){
				feats.incrementCount(new StrFeature("gust-min+max-exists"), 1.0)
			} else {
				feats.incrementCount(new StrFeature("gust-min-notmax-exists"), 1.0)
			}
		}

		//--IsDirNone
		val windDirF:Int = FeatureUtils.str2f("wind_dir", t)
		val str:String = GenUtils.vstr(t,windDirF,ws.events(e).fields(windDirF).value)
		if(str.equalsIgnoreCase("none")) feats.incrementCount(new StrFeature("no-wind"), 1.0)
		else if(str.toLowerCase.contains("var")) feats.incrementCount(new StrFeature("variable-wind"), 1.0)

		
		//special wind speeds
		val windMinF:Int = FeatureUtils.str2f("wind_min", t)
		val windMaxF:Int = FeatureUtils.str2f("wind_max", t)
		val windMin:Int = ws.events(e).fields(windMinF).value
		val windMax:Int = ws.events(e).fields(windMaxF).value
		if(windMin == 0 && windMax == 0){
			feats.incrementCount(new StrFeature("wind-min-max=0"), 1.0)
		}else if(windMin == 0 && windMax > 0){
			feats.incrementCount(new StrFeature("wind-min-0-max>0"), 1.0)
		}else if(windMax == 0 && windMin > 0){
			feats.incrementCount(new StrFeature("wind-max-0-min>0"), 1.0)
		}

		return feats
	}

	override def outputFeatures(in:FieldSetHistory, fs:FieldSet, ws:WorldState):Counter[Feature] = {
		val feats:Counter[Feature] = super.outputFeatures(in, fs, ws)
		return feats
	}

	override def jointFeatures(in:FieldSetHistory, fs:FieldSet, ws:WorldState):Counter[Feature] = {
		val feats:Counter[Feature] = super.jointFeatures(in, fs, ws)
		return feats
	}
}

object TimeTracker{
	var timeCalcs:HashMap[String,StatCalc] = null
	val strings:Array[String] = FeatureUtils.times

	def register(ws:WorldState, e:Int, str:String):Unit = {
		//(setup)
		if( timeCalcs == null ){
			timeCalcs = new HashMap[String,StatCalc]
			strings.foreach( (s:String) => timeCalcs(s) = new StatCalc().trackMode())
		}
		//(get time)
		val timeF:Int = FeatureUtils.str2f("time", 0)	//TODO hack in assuming t=0
		var time:Int = ws.events(e).fields(timeF).value
		val timeStr:String = GenUtils.vstr(0,timeF,time)
		time = Integer.parseInt(timeStr)
		if(time == 1) time = 1300
		//(add time)
		var seen:Boolean = false
		strings.foreach( (target:String) => {
			if(!seen && str.contains(target)){
				seen = true
				val stats:StatCalc = timeCalcs(target)
				stats.enter(time)
			}
		})
	}
		
	def register(ws:WorldState, tm:Int, m:Template):Unit = {
		if( timeCalcs == null ){
			timeCalcs = new HashMap[String,StatCalc]
			strings.foreach( (s:String) => timeCalcs(s) = new StatCalc().trackMode())
		}
		val time:Int = {if(tm == 1) 1300 else tm}
		//(variables)
		val words:Array[String] = m.templateWords
		var str:String = ""
		words.foreach( (s:String) => str += s.toLowerCase + " ")
		str.replaceAll(" +", " ")
		var seen:Boolean = false
		//(add time)
		strings.foreach( (target:String) => {
			if(!seen && str.contains(target)){
				seen = true
				val stats:StatCalc = timeCalcs(target)
				stats.enter(time)
			}
		})
	}
	
	def infer(time:Int):Counter[Array[String]] = {
		val rtn:Counter[Array[String]] = new Counter[Array[String]]
		strings.foreach( (s:String) => {
			rtn.setCount(s.split(" +"), getScore(time, timeCalcs(s)))
		})
		rtn
	}
	def getScore(timeInt:Int, m:Template):Double = {
		//(get statistics)
		val words:Array[String] = m.templateWords
		var str:String = ""
		words.foreach( (s:String) => str += s.toLowerCase + " ")
		str.replaceAll(" +", " ")
		var seen:Boolean = false
		var stats:StatCalc = null
		strings.foreach( (target:String) => {
			if(!seen && str.contains(target)){
				seen = true
				stats = timeCalcs(target)
				if(stats.getCount() == 0){
					println("WARNING: no training examples for time string: " + target)
				}
			}
		})
		getScore(timeInt, stats)
	}
	private def getScore(tInt:Int, stats:StatCalc):Double = {
		if(stats == null || stats.getCount() == 0) return 0.0
		val timeInt:Int = {if(tInt == 1) 1300 else tInt}
		//(get terms)
		val time:Double = timeInt.asInstanceOf[Double]
		val mode:Double = stats.getMode()
		val lbound:Double = stats.getMin()
		val ubound:Double = stats.getMax()
		val weight:Double = stats.getModeCount()
		
		//--Heuristic Score
		//(vars)
		var absmin = lbound
		var absmode = mode
		var absmax = ubound
		var abstime = timeInt
		if(ubound < lbound) absmax = ubound + 2400
		if(mode < lbound) absmode = mode + 2400
		if(abstime < lbound) abstime = timeInt + 2400
		absmin = Math.max(absmin, absmode-400)
		absmax = Math.min(absmax, absmode+400)
		//(case: mode)
		val score = if(abstime == absmode){
			weight * 1.0
		//(case: below mode)
		} else if(abstime >= absmin && abstime <= absmode) {
			weight * Math.pow((abstime - absmin).asInstanceOf[Double] / (absmode - absmin).asInstanceOf[Double], 2.0)
		//(case: above mode)
		} else if(abstime >= absmode && abstime <= absmax){
			weight * Math.pow((absmax - abstime).asInstanceOf[Double] / (absmax - absmode).asInstanceOf[Double], 2.0)
		//(case: out of range)
		} else{
			0.0
		}
		if(score > weight) throw new IllegalStateException("INTERNAL: equations are wrong")
		score
	}
	def dump = {
		timeCalcs.foreach( (pair:(String,StatCalc)) => {
			println(pair._1)
			val stats:StatCalc = pair._2
			if(stats.getCount > 0){
				println("   min=" + stats.getMin())
				println("   max=" + stats.getMax())
				println("  mode=" + stats.getMode() + " (count=" + stats.getModeCount + ")")
			}else{
				println("  (none)")
			}
		})
	}
	val seen:HashSet[AlignedSentence] = new HashSet[AlignedSentence]
}

class SumtimeTemplateChoice(
	t:Int, // the parent event
	templates:Array[Template], // the templates for that event
	fieldSet:FieldSet, // the parent field set
	factory:ProbabilisticClassifierFactory[TemplateHistory,Feature,Template], 
	lm:LanguageModel, // the language model
	opts:induction.Options // the generation options
		) extends TemplateChoice(t, templates, fieldSet, factory, lm, opts) {
	
	def noneOrVar(e:Int, f:Int, ws:WorldState):Boolean = {
		val str:String = GenUtils.vstr(t,f,ws.events(e).fields(f).value)
		if(str.equalsIgnoreCase("none") || str.contains("var") || str.contains("VAR")) true
		else false
	}
	def noneOrVar(str:String):Boolean = {
		if(str.equalsIgnoreCase("none") || str.contains("var") || str.contains("VAR")) true
		else false
	}

	override def registerSentence(as:AlignedSentence, ws:WorldState):Unit = {
		if(TimeTracker.seen.contains(as)) return;
		TimeTracker.seen += as
		var e:Int = -1
		var str:String = ""
		for( i <- 0 until as.observedEvents.length) {
			val candE:Int = as.observedEvents(i)
			val w:Int = as.words(i)
			val s:String = GenUtils.wstr(w)
			if(candE == e){
				str += " " + s
			}else{
				if(!str.equals("")){
					TimeTracker.register(ws,e,str)
				}
				str = s
				e = candE
			}
		}
		if(!str.equals("")){
			TimeTracker.register(ws,e,str)
		}
	}

	override def inputFeatures(in:TemplateHistory, m:Template, ws:WorldState):Counter[Feature] = {	
		//TimeTracker.dump
		//System.exit(1)
		val feats:Counter[Feature] = super.inputFeatures(in, m, ws)
		if(m == stopTerm) return feats;
		//TimeTracker.dump
		//--Variables
		//(from history)
		val eHist = in.eSoFar
		val e:Int = in.parentE
		val t:Int = GenUtils.e2t(in.parentE,ws)
		val fs:FieldSet = this.fieldSet
		val hasLast:Boolean = (eHist.length > 1)
		val lastE:Int = if(hasLast) eHist(eHist.length-2) else -1
		//(field values)
		//wind dir
		var windDirF:Int = FeatureUtils.str2f("wind_dir", t)
		var windDir:String = GenUtils.vstr(t,windDirF,ws.events(e).fields(windDirF).value)
		var lastWindDir:String = if(hasLast) GenUtils.vstr(t,windDirF,ws.events(lastE).fields(windDirF).value) else null
		var lastWindDirNotNull:String 
			= {
				var i:Int = eHist.length-2
				var cont=true
				while(i>=0 && cont){
					val str:String = GenUtils.vstr(t,windDirF,ws.events(eHist(i)).fields(windDirF).value)
					if(!noneOrVar(eHist(i), windDirF, ws)){
						cont = false
					}else{
						i -= 1
					}
				}
				if(i < 0) null else GenUtils.vstr(t,windDirF,ws.events(eHist(i)).fields(windDirF).value)
			}
		//wind speed
		val windMinF:Int = FeatureUtils.str2f("wind_min", t)
		val windMaxF:Int = FeatureUtils.str2f("wind_max", t)
		val windMin:Int = ws.events(e).fields(windMinF).value
		val windMax:Int = ws.events(e).fields(windMaxF).value
		val windSpeed:Int = windMax
		val lastWindSpeed:Int = if(hasLast) ws.events(lastE).fields(windMaxF).value else -1
		val lastWindSpeedNotNull:Int 
			= {
				var i:Int = eHist.length-2
				var cont=true
				while(i>=0 && cont){
					val min:Int = ws.events(eHist(i)).fields(windMinF).value
					val max:Int = ws.events(eHist(i)).fields(windMaxF).value
					if(max > 0) cont = false
					else i -= 1
				}
				if(i < 0) -1 else ws.events(i).fields(windMaxF).value 
			  }
		//gust
		val gustMinF:Int = FeatureUtils.str2f("gust_min", t)
		//time
		val timeF:Int = FeatureUtils.str2f("time", t)
		val timeStr:String = GenUtils.vstr(t,timeF,ws.events(e).fields(timeF).value)
		var time:Int = Integer.parseInt(timeStr)
		if(time == 01) time = 1300
		val lastTimeStr:String = if(!hasLast) "-1" else GenUtils.vstr(t,timeF,ws.events(lastE).fields(timeF).value)
		var lastTime:Int = Integer.parseInt(lastTimeStr)
		if(lastTime == 01) time = 1300

		//--Wind Direction
		var windStr:String = ""
		if(fs.hasField(windDirF)){
			//(indicator)
			//feats.incrementCount( new WindDirIndicator(!hasLast, windDir), 1.0 )
			//(is variable or none?)
			if(noneOrVar(e, windDirF, ws)) feats.incrementCount( new StrFeature("no-wind-dir"), 1.0)
			if(windDir.equalsIgnoreCase("CVAR")) feats.incrementCount(new StrFeature("wind-is-cvar"), 1.0)
			if(windDir.equalsIgnoreCase("VAR")) feats.incrementCount(new StrFeature("wind-is-var"), 1.0)
			if(windDir.equalsIgnoreCase("NONE")) feats.incrementCount(new StrFeature("wind-is-none"), 1.0)
			//(is clockwise/ccw?)
			//we're not first, we're not the first actual direction, we have a direction, we have a last direction
			if(hasLast && lastWindDirNotNull != null && !noneOrVar(windDir) && !noneOrVar(lastWindDirNotNull)){
				val clockwise:Boolean = 
						FeatureUtils.windDirs.isClockwiseFrom(lastWindDirNotNull, windDir)
				val ccw:Boolean = 
						FeatureUtils.windDirs.isCounterClockwiseFrom(lastWindDirNotNull, windDir)
				if(clockwise) windStr += "clockwise-"
				else if(ccw) windStr += "counterclockwise-"
				else windStr += "nodirchange-"
				feats.incrementCount(new StrFeature(	if(clockwise) "clockwise-wind-dir-change"
														else if(ccw) "counter-clockwise-wind-dir-change"
														else "no-wind-dir-change"), 1.0)
			}else if(!hasLast || lastWindDirNotNull == null || noneOrVar(lastWindDirNotNull)){
				//[no previous wind direction]
				windStr += "noprevwinddir-"
			}else{
				//[variable or none wind direction]
				windStr += "none/vardir-"
			}
		} else {
			windStr += "dirfieldabsent-"
		}
	
		//--Wind Speed
		if(fs.hasField(windMinF) || fs.hasField(windMaxF)){
			//(indicator)
			//	FeatureUtils.addValue(feats,e,windMinF,ws)
			//	FeatureUtils.addValue(feats,e,windMaxF,ws)
			//(change)
			val windChange = windSpeed - lastWindSpeed
			//(special markers)
			if(windMin == 0 && windMax == 0){
				windStr += "speed0/0"
				feats.incrementCount(new StrFeature("wind-min-max=0"), 1.0)
			}else if(windMin == 0 && windMax > 0){
				windStr += "speed0/>0"
				feats.incrementCount(new StrFeature("wind-min-0-max>0"), 1.0)
			}else if(windMax == 0 && windMin > 0){
				windStr += "speed>0/0"
				feats.incrementCount(new StrFeature("wind-max-0-min>0"), 1.0)
			}else if(lastWindSpeedNotNull >= 0){
				if(windChange > 0) windStr += "speedincreased"
				else if(windChange == 0) windStr += "speedsteady"
				else windStr += "speeddecreased"
				val feat:Feature = new WindSpeedChange(!hasLast, 
													 if(windChange > 0) "increased"
													 else if(windChange == 0) "stayed-steady"
													 else "decreased")
				feats.incrementCount(feat, 1.0);
			}else if(lastWindSpeed >= 0){
				windStr += "speedincreasedfromnull"
				val feat:Feature = new WindSpeedChange(!hasLast, 
													 if(windChange > 0) "increased-fromnull"
													 else if(windChange == 0) "stayed-steady-fromnull?"
													 else "decreased-fromnull?")
				feats.incrementCount(feat, 1.0);
			}else{
				windStr += "noprevwindspeed"
			}
		} else {
			windStr += "nospeedfield"
		}

		//--Joint Wind
		if((fs.hasField(windMinF) || fs.hasField(windMaxF)) && fs.hasField(windDirF)){
			feats.incrementCount(new StrFeature(windStr), 1.0)
		}

		//--Gust
		if(fs.hasField(gustMinF)){
			if(ws.events(e).fields(gustMinF).value > 0) feats.incrementCount(new GustIndicator(), 1.0)
		}


		//-- Capture 'LY
		if(windDir.length == 1){
			feats.incrementCount(new StrFeature("length-1-winddir"), 1.0)
		}
		
		//--Time Difference (capture 'soon,' 'gradually')
		if(hasLast){
			val timeDiff:Int = (2400 + (time-lastTime)) % 2400
			if(timeDiff == 0) feats.incrementCount(new StrFeature("timeDiff-[0-1)"), 1.0)
			else if(timeDiff <= 300) feats.incrementCount(new StrFeature("timeDiff-[1-301)"), 1.0)
			else if(timeDiff < 600) feats.incrementCount(new StrFeature("timeDiff-[301-600)"), 1.0)
			else if(timeDiff < 1200) feats.incrementCount(new StrFeature("timeDiff-[600-1200)"), 1.0)
			else if(timeDiff < 1800) feats.incrementCount(new StrFeature("timeDiff-[1200-1800)"), 1.0)
			else if(timeDiff < 2400) feats.incrementCount(new StrFeature("timeDiff-[1800-2400)"), 1.0)
			else feats.incrementCount(new StrFeature("timeDiff>=2400"), 1.0)
		}
		
		//--First
		//if(!hasLast) feats.incrementCount(new StrFeature("is-first-template"), 1.0)
		
		//--Debug
		if(false && !in.generated && (!(FeatureUtils.wsSave contains ws) || !(FeatureUtils.mHistSave contains in))){
			FeatureUtils.mHistSave += in
			FeatureUtils.wsSave += ws

			println(m)
			println("\te=" + e + " lastE=" + lastE + " hasLast=" + hasLast)
			println("\teHist=" + GenUtils.arrayToString(eHist))
			println("\twindDir: " + lastWindDirNotNull + " (" + lastWindDir + ") -> " + windDir)
			println("\t(events)")
			println("\t" + {if(hasLast) ws.events(lastE) else "[[first]]"})
			println("\t" + ws.events(e))
			println("\t(features)")
			val iter:java.util.Iterator[Feature] = feats.keySet.iterator
			while(iter.hasNext()){
				val key = iter.next
				println("\t" + new java.text.DecimalFormat("0.000").format(feats.getCount(key)) + " :  " + key)
			}
		}

		return feats
	}

	override def outputFeatures(in:TemplateHistory, m:Template, ws:WorldState):Counter[Feature] = {
		//val feats:Counter[Feature] = super.outputFeatures(in, m, ws)
		val feats:Counter[Feature] = new Counter[Feature]
		val hasLast:Boolean = (in.eSoFar.length > 1)
		if(m == stopTerm) return feats;
		
		//--Parent Template
		m.parents.foreach( (parent:Template) => {
			feats.incrementCount(new SumtimeParentTemplate(hasLast,parent), 1.0)
		})
		
		//--All Words
		/*
		m.templateWords.foreach( (w:String) => {
			feats.incrementCount(WordIndicator(hasLast,w), 1.0)
		})
		*/
		
		//--Keywords
		/*
		if(m != stopTerm){
			val words:Array[String] = m.templateWords
			words.foreach( (w:String) => {
				if(FeatureUtils.keywords contains w) feats.incrementCount(KeywordIndicator(hasLast,w), 1.0)
			})
			FeatureUtils.keyphrases.foreach( (phrase:Array[String]) => {
				if(Utils.indexOf(words, phrase) >= 0){
					feats.incrementCount(new KeyphraseIndicator(hasLast,phrase), 1.0)
				}
			})
		}
		*/
		return feats
	}
	
	override def jointFeatures(in:TemplateHistory, m:Template, ws:WorldState):Counter[Feature] = {
		val feats:Counter[Feature] = super.jointFeatures(in, m, ws)
		if(m == stopTerm) return feats;
		val eHist = in.eSoFar
		val e:Int = in.parentE
		val t:Int = GenUtils.e2t(in.parentE,ws)
		val fs:FieldSet = this.fieldSet
		val hasLast:Boolean = (eHist.length > 1)
		val timeF:Int = FeatureUtils.str2f("time", t)
		/*
		//--Time
		if(fs.hasField(timeF) && m != stopTerm){
			val timeStr:String = GenUtils.vstr(t,timeF,ws.events(e).fields(timeF).value)
			val time:Int = Integer.parseInt(timeStr)
			val score:Double = TimeTracker.getScore(time, m)
			if(score > 0){
				feats.incrementCount(new StrFeature("time-score"), score)
			}
		}
		*/
		//--Repeated Keyword
		/*
		if(m != stopTerm){
			m.templateWords.foreach( (w:String) => {
				if((FeatureUtils.keywords contains w) && (in.genSoFar contains w)) 
					feats.incrementCount(StrFeature("repeated-keyword-" + w), 1.0)
			})
			
		}
		*/
		//--Starting Field
		/*
		val startField:Int = m.getFields(0)
		if(hasLast)
			feats.incrementCount(new StrFeature("template-starts-with-field-"+GenUtils.fstr(0,startField)), 1.0)
		else
			feats.incrementCount(new StrFeature("first-template-starts-with-field-"+GenUtils.fstr(0,startField)), 1.0)
		*/

		return feats
	}
}


class SumtimeTemplateFillChoice(opts:induction.Options)
        extends TemplateFillChoice(opts) {

	val times:Array[String] = FeatureUtils.times

	override def output(in:(TemplateFillHistory,WorldState)):(Int,Array[Int],Array[String]) ={
		val m:Template = in._1.parentTemplate
		val fs:FieldSet = m.fieldSet
		val tuple:(Int,Array[Int],Array[String]) = super.output(in)
		val genSoFar:Array[String] = in._1.genSoFar
		if(tuple == stopTerm) return tuple
		val ws = in._2
		//--Variables
		//(get variables)
		val e:Int = tuple._1
		var fields:Array[Int] = tuple._2
		var words:Array[String] = tuple._3
		//println(GenUtils.arrayToString(words))

		//--Replace Time
		var cont = true
		for( tIndex <- 0 until times.length){
			if(cont){
			val time:String = times(tIndex)
			val timeArray:Array[String] = time.split(" +")
			var index:Int = Utils.indexOf(words, timeArray)
			if(index >= 0){
				//(fill up to the time)
				var fList:List[Int] = List[Int]()
				var wList:List[String] = List[String]()
				for( i <- 0 until index ){
					fList = fields(i) :: fList
					wList = words(i) :: wList
				}
				//(replace time)
				val timeF:Int = FeatureUtils.str2f("time", GenUtils.e2t(e,ws))
				val time:Int = ws.events(e).fields(timeF).value
				val timeStr:String = GenUtils.vstr(GenUtils.e2t(e,ws), timeF, time)
				val rplCounts:Counter[Array[String]] = TimeTracker.infer(Integer.parseInt(timeStr))
				while(Utils.indexOf(genSoFar, rplCounts.argMax) >= 0){
					rplCounts.incrementCount(rplCounts.argMax, -5.0)
				}
				val replace:Array[String] = rplCounts.argMax
				replace.foreach( (w:String) => {
					fList = timeF :: fList
					wList = w :: wList
				})
				//(fill the rest)
				var start:Int = index+timeArray.length
				if(start < words.length && words(start).equals("later")) start += 1
				for( i <- start until words.length ){
					fList = fields(i) :: fList
					wList = words(i) :: wList
				}
				//println("\t" + GenUtils.arrayToString(wList.reverse.toArray))
				
				//(update variables)
				//##println("Replaced: " + GenUtils.arrayToString(words) + "   with: " + GenUtils.arrayToString(wList.reverse.toArray))
				words = wList.reverse.toArray
				fields = fList.reverse.toArray
				cont = false
			}
			}
		}

		//--Replace WindDir
		val dirF = FeatureUtils.str2f("wind_dir", 0)
		for(i <- 0 until words.length){
			val s:String = words(i)
			val f:Int = fields(i)
			words(i) =	if(s.equals("cyclonic")) "*DEL*"
						else if(s.equals("variable")) "*DIR*"
						else if(f == dirF) "*DIR*"
						else s
		}
		var wList:List[String] = List[String]()
		var fList:List[Int] = List[Int]()
		for(i <- 0 until words.length){
			val s:String = words(i)
			val f:Int = fields(i)
			if(words(i).equals("*DEL*")){
				//do nothing: we ignore this
			}else if(words(i).equals("*DIR*")){
				var dirStr:String = GenUtils.vstr(0,dirF,ws.events(e).fields(dirF).value).toLowerCase
				if(dirStr.equalsIgnoreCase("CVAR")){
					wList = "cyclonic" :: wList
					fList = dirF :: fList
					wList = "variable" :: wList
					fList = dirF :: fList
				}else if(dirStr.contains("VAR") || dirStr.contains("var")){
					wList = "variable" :: wList
					fList = dirF :: fList
				}else{
					if(dirStr.length > 2 && dirStr.substring(0,2).equalsIgnoreCase("M-")){
						wList = "mainly" :: wList
						fList = dirF :: fList
						dirStr = dirStr.substring(2)
					}
					wList = dirStr :: wList
					fList = dirF :: fList
				}
			}else{
				wList = s :: wList
				fList = f :: fList
			}
		}
		words = wList.reverse.toArray
		fields = fList.reverse.toArray


		//--Fix UNKs
		for( x <- 0 until words.length ){
			var i = x
			val w:String = words(i)
			val f:Int = fields(i)
			if(w.equals("[[UNK]]")){
				val v = ws.events(e).fields(f).value
				var vstr = GenUtils.vstr(GenUtils.e2t(e,ws), f, v)
				//(hack for mainly)
				/*
				if(fields(i) == FeatureUtils.str2f("wind_dir", GenUtils.e2t(e,ws)) && vstr.charAt(0) == 'M'){
					vstr = vstr.substring(2)
					val newWords = new Array[String](words.length+1)
					val newFields = new Array[Int](fields.length+1)
					for( j <- 0 until i){
						newWords(j) = words(j)
						newFields(j) = fields(j)
					}
					newWords(i) = "mainly"
					newFields(i) = fields(i)
					for( j <- i+1 until newWords.length){
						newWords(j) = words(j-1)
						newFields(j) = fields(j-1)
					}
					i += 1
					words = newWords
					fields = newFields
				}
				*/
				//(end hack)
				words(i) = vstr.toLowerCase
			}
		}

		//--Fix 'Or Less' bug
		/*
		val t = GenUtils.e2t(e,ws)
		if(fs.hasField(FeatureUtils.str2f("wind_min", t)) && !fs.hasField(FeatureUtils.str2f("wind_max", t))){
			val templWords:Array[String] = m.templateWords
			if(Utils.indexOf(templWords, Array[String]("or", "less")) < 0){
				//(add an 'or less' after the wind_min
				val newFields:Array[Int] = new Array[Int](fields.length+2)
				val newWords:Array[String] = new Array[String](words.length+2)
				var bigIndex = 0;
				for(littleIndex <- 0 until fields.length){
					val f:Int = fields(littleIndex)
					val w:String = words(littleIndex)
					newWords(bigIndex) = w
					newFields(bigIndex) = f
					if(f == FeatureUtils.str2f("wind_min",t)){
						newFields(bigIndex+1) = GenUtils.none_f(t)
						newFields(bigIndex+2) = GenUtils.none_f(t)
						newWords(bigIndex+1) = "or"
						newWords(bigIndex+2) = "less"
						bigIndex += 2
					}
					bigIndex += 1
				}
				words = newWords
				fields = newFields
			}
		}
		*/


		(e, fields, words)
	}

}


