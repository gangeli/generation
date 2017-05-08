package generation

import scala.collection.mutable.HashMap
import java.util.Random

import fig.basic.{IOUtils,Indexer,StatFig}
import fig.exec.Execution
import fig.exec.Execution.putLogRec
import fig.record.Record
import fig.basic.LogInfo.{logs,dbg,fail,error,track,end_track}

import tea._
import tea.Utils.{map,foreach,fmt,fmt1,returnFirst,assertValid}

import induction._

class AlignmentRunner extends Runnable {
	var opts = new induction.Options

	def run = {
		//--Create Models
		val prob:WordProblem = opts.modelType match {
			case induction.Options.ModelType.gmm => new GMMProblem(opts)
			case induction.Options.ModelType.pmmm => new ProdMultMixtureProblem(opts)
			case induction.Options.ModelType.hmm => new HMMProblem(opts)
			case induction.Options.ModelType.pcfg => new PCFGProblem(opts)
			case induction.Options.ModelType.dmv => new DMVProblem(opts)
			case induction.Options.ModelType.seg => new SegmentationProblem(opts)
			case induction.Options.ModelType.event3 => new Event3Problem(opts)
			case _ => fail("Unknown or Invalid model type: " + opts.modelType); null
		}
		
		val model = prob.asInstanceOf[Event3Problem].newModel
		
		track("Reading Examples")
		//(read examples)
		if(opts.maxExamples < opts.testEnd)
			throw new java.lang.IllegalArgumentException("Not reading enough examples for test data!")
		opts.maxExamples = opts.testEnd
		model.readExamples
		if(opts.testEnd > model.examples.size) 
			opts.testEnd = model.examples.size
		if(opts.testStart > model.examples.size)
			opts.testStart = model.examples.size
		if(opts.trainEnd > model.examples.size)
			opts.trainEnd = model.examples.size
		if(opts.trainStart < 0 || opts.trainEnd < opts.trainStart || opts.testStart < opts.trainEnd || opts.testEnd < opts.testStart)
			throw new java.lang.IllegalArgumentException("Training and test data is not well partitioned: " + opts.trainStart + ":" + opts.trainEnd + ", " + opts.testStart + ":" + opts.testEnd)
		end_track
		
		//(train model)
		track("Training Semantics", true)
		if(opts.loadParams){
			//(load parameters from file)
			if(opts.paramSavePath == null) fail("No path given to load parameters from")
			model.loadParams(opts.paramSavePath)
		} else {
			//(stats)	
			Record.begin("stats")
			track("Stats", true)
			model.logStats
			end_track
			Record.end
			//(train)
			model.preInit
			model.init(opts.initType, opts.initRandom)
			model.learn("stage1", opts.stage1)
			model.learn("stage2", opts.stage2)
			//(save parameters)
			if(opts.paramSavePath != null) model.saveParams(opts.paramSavePath)
		}
		end_track
		
		val aligner = new Aligner(prob.asInstanceOf[Event3Problem])
		val aligns:AlignedDataset = aligner.align(model.asInstanceOf[aligner.problem.Model], opts)
		println("Aligned")
	}
}

object Alignment {
	def main(args:Array[String]) = {
		val x = new AlignmentRunner
		fig.exec.Execution.run(args, x, x.opts)
	}
}
