package generation

import fig.basic.{IOUtils,Indexer,StatFig}
import fig.exec.Execution
import fig.exec.Execution.putLogRec
import fig.record.Record
import fig.basic.LogInfo.{logs,dbg,fail,error,track,end_track}
import scala.collection.mutable.HashMap
import javax.script.CompiledScript

class GenerationRunner() extends Runnable {
	var opts = new induction.Options

	override def run = {
		if(!opts.loadParams || !opts.loadAligns)
			throw new java.lang.IllegalArgumentException("Run alignment before running generation")
		
		//--Language Model
		val indexer:Indexer[String] = new Indexer[String]
		val lm:LanguageModel = opts.lm match {
			case induction.Options.LanguageModel.simple => new LanguageModel(opts, opts.numGrams, indexer)
			case induction.Options.LanguageModel.kneserney => new KneserNeyLM(opts, opts.numGrams, indexer)
			case induction.Options.LanguageModel.event3 => new Event3LM(opts, opts.numGrams, indexer)
			case _ => fail("Unknown language model type: " + opts.lm); null
		}
			
		
		val generator:Generator = new Generator(lm, opts)
		generator.init
		generator.trainLM
		if(opts.useTemplates){
			generator.templateTrain
			generator.templateGenerate
		}else{
			generator.simpleTrain
			generator.simpleGenerate
		}
		
	}
}

object Generation {
	def main(args:Array[String]) = {
		val x = new GenerationRunner
		fig.exec.Execution.run(args, x, x.opts)
	}
}
