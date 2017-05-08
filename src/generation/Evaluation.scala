package generation

import fig.basic.{IOUtils,Indexer,StatFig}
import fig.exec.Execution
import fig.exec.Execution.putLogRec
import fig.record.Record
import fig.basic.LogInfo.{logs,dbg,fail,error,track,end_track}

import tea._
import tea.Utils.{map,foreach,fmt,fmt1,returnFirst,assertValid}

class EvaluationRunner extends Runnable {
	val opts = new EvalOptions

	def run = {
		new Evaluator(opts).evaluate
	}
}

object Evaluation {
	def main(args:Array[String]) = {
		val x = new EvaluationRunner
		fig.exec.Execution.run(args, x, x.opts)
	}
}
