package induction

import scala.collection.mutable.HashMap
import java.util.Random

import fig.basic.{IOUtils,Indexer,StatFig}
import fig.exec.Execution
import fig.exec.Execution.putLogRec
import fig.record.Record
import fig.basic.LogInfo.{logs,dbg,fail,error,track,end_track}

import tea._
import tea.Utils.{map,foreach,fmt,fmt1,returnFirst,assertValid}

import InductionUtils._

class InductionRunner extends Runnable {
  val opts = new Options

  def run = {
    val prob = opts.modelType match {
      case Options.ModelType.gmm => new GMMProblem(opts)
      case Options.ModelType.pmmm => new ProdMultMixtureProblem(opts)
      case Options.ModelType.hmm => new HMMProblem(opts)
      case Options.ModelType.pcfg => new PCFGProblem(opts)
      case Options.ModelType.dmv => new DMVProblem(opts)
      case Options.ModelType.seg => new SegmentationProblem(opts)
      case Options.ModelType.align => new WordAlignmentProblem(opts)
      case Options.ModelType.event3 => new Event3Problem(opts)
      case _ => fail("Unknown model type: " + opts.modelType); null
    }
    val model = prob.newModel
    model.readExamples

    Record.begin("stats")
    track("Stats", true)
    model.logStats
    end_track
    Record.end

    if (opts.genNumExamples > 0) { // Use generated examples instead
      model.preInit
      model.init(opts.genInitType, opts.genInitRandom)
      model.genExamples
    }
    model.preInit
    model.init(opts.initType, opts.initRandom)
    model.learn("stage1", opts.stage1)
    model.learn("stage2", opts.stage2)

	// Dump alignments
	if(opts.alignedPath != null){
		val e3prob = prob.asInstanceOf[Event3Problem]
		model.asInstanceOf[e3prob.Model].output(opts.alignedPath)
	}
  }
}

object Induction {
  def main(args:Array[String]) = {
    val x = new InductionRunner
    fig.exec.Execution.run(args, x, x.opts)
  }
}
