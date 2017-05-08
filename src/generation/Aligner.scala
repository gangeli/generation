package generation

import fig.basic.LogInfo.{logs,dbg,fail,error,track,end_track}
import fig.record.Record
import fig.basic.Indexer

import tea.Utils.{map,foreach,fails}

import cortex._

import java.io._

import org.goobs.utils.StatCalc
import org.goobs.foreign.Counter
import org.goobs.classify.FeatureExtractor
import org.goobs.distributions._
import AlignedDataset.AlignedSentence

import induction.Event3Problem

case class Aligner(problem:Event3Problem){
	def align(model:problem.Model,
			opts:induction.Options
			):AlignedDataset = {
		var alignments = new AlignedDataset(model.examples.size)
		GenUtils.fillGenUtils(problem)
		
		def loadAlignments():Unit = {
			val path:String = opts.alignSavePath
			val objStream:ObjectInputStream = new ObjectInputStream(new FileInputStream(path))
			alignments = objStream.readObject().asInstanceOf[AlignedDataset]
			objStream.close()
		}
		
		def logAlignments_!(exNum:Int,
							events:Array[Int], 
							eventTypes:Array[Int],
							fields:Array[Int], 
							words:Array[Int], 
							worldState:WorldState, 
							trueEvents:Array[Int],
							confidence:Double		):Unit = {
			//--Tweaks
			val observedEvents = events.map( (e:Int) => {if(e<0) worldState.size else e} )
			//--Sanity Checks
			//(eventTypes out of range)
			eventTypes.foreach( (t:Int) => {
				if(t > GenUtils.none_t || t < 0) 
					throw new java.lang.IllegalArgumentException("bad t value: t= " + t + " T=" + problem.T)
			})
			//(events out of range)
			observedEvents.foreach( (e:Int) => {
				if(e != GenUtils.fail_e && ( e < 0  || e > GenUtils.none_e(worldState)))
					throw new java.lang.IllegalArgumentException("bad e value: e= " + e + " E=" + worldState.size)
			})
			//--Log Alignment
			alignments.addAlignment(exNum, new Array[Int](events.size), eventTypes, fields, words, observedEvents, worldState, trueEvents)	
		}
		
		def exampleToWorldState(ex:problem.Example):WorldState = {
			//--Get WorldState
			var e = 0;
			val events = new Array[Event](ex.events.size)
			ex.events.foreach( (ev) => {
				val fields = new Array[Field](ev.F)
				foreach( ev.F, (f:Int) => {
					fields(f) = new Field(f, ev.values(f), model.fieldType(ex.events(e).fields(f)) )
				})
				events(e) = new Event(ev.t, fields)
				e+=1
			})
			val worldState:WorldState = new WorldState( events )
			worldState
		}
		
		if(opts.loadAligns){
			loadAlignments()
		} else {
		
			var parsedCount:Int = 0;
			foreach(model.examples.size, (exNum:Int) => {
				val ex = model.examples(exNum)
				//--Get WorldState
				val worldState:WorldState = exampleToWorldState(ex) 
				//--Infer Alignments
				val inferState = model.newInferState(ex, model.params, model.newParams, 
									model.newInferSpec(1,false,true,false,false,false,false,0,opts.stage2.numIters-1) )
				/*
				print("" + ex.trueWidget.events.size + ">>>"); GenUtils.printArray(ex.trueWidget.events)
				if(ex.trueWidget.events.size > 0)
					GenUtils.printArray(ex.trueWidget.events(0))
				*/
				val trueEvents 
					= if(ex.trueWidget == null) new Array[Int](0)
					else if(ex.trueWidget.events.size == 0){
							val te = new Array[Int](1)
							te(0) = GenUtils.fail_e
							te
						} else {
							ex.trueWidget.events(0)
						}
			
				val widget = inferState.bestWidget
				//(expected versus actual events)
				//val expEvents 
				//	= widget.events(0).map( (e:Int) => {if(e == -1) none_t else widget.e2t(e)} )
				//val actEvents:Array[Int] 
				//	= ex.trueWidget.events(0).map( (e:Int) => { if(e == -1) none_t else ex.trueWidget.e2t(e) } )
				//(get the values)
				//val events = widget.events.map(_(0))
				val events = widget.events(0)
				val eventTypes:Array[Int] = events.map( (e:Int) => {if(e == -1) GenUtils.none_t else widget.e2t(e)} )
				val fields:Array[Int] = if(!opts.forceActualAlignment) widget.fields(0)
								else new Array[Int](events.size)
				val words:Array[Int]  = ex.text
				val confidence = 1.0
				//--Log the result
				//print("        aligned> "); GenUtils.printArray(GenUtils.toMarkov(expEvents).map(tstr(_)))
				//print("        actual > "); GenUtils.printArray(GenUtils.toMarkov(actEvents).map(tstr(_)))
				//logGeomStop_!(events, fields, words, 0, confidence)
				if( !( opts.ignoreNone && trueEvents.size == 1 && trueEvents(0) == -1 ) ){
					logAlignments_!(parsedCount, events, eventTypes, fields, words, worldState, trueEvents, confidence)
					parsedCount = parsedCount + 1
					logs("parsed example   " + parsedCount + "/" + model.examples.size)
				}else{
					logs("skipping example " + parsedCount + "/" + model.examples.size)
				}
			})
			logs("Examples parsed")

			if(opts.alignSavePath != null){
				alignments.saveAlignment(opts.alignSavePath)
			}
		}
		
		
		if(alignments.size < opts.trainEnd)
			throw new java.lang.IllegalArgumentException("Too few alignments loaded for training!")
		end_track

		val rand = new scala.util.Random
		def shuffle = {
			foreach(alignments.size * 10, (i:Int) => {
				val a:Int = (rand.nextDouble * alignments.size).asInstanceOf[Int]
				val b:Int = (rand.nextDouble * alignments.size).asInstanceOf[Int]
				val tmpA = alignments.aligns(a)
				val tmpT = alignments.trueEvents(a)
				val tmpW = alignments.worldStates(a)
				alignments.aligns(a) = alignments.aligns(b)
				alignments.trueEvents(a) = alignments.trueEvents(b)
				alignments.worldStates(a) = alignments.worldStates(b)
				alignments.aligns(b) = tmpA
				alignments.trueEvents(b) = tmpT
				alignments.worldStates(b) = tmpW
			})
		}

		if(opts.shuffleAlignments){
			shuffle
		}
		
		return alignments
	}
	
	
}
