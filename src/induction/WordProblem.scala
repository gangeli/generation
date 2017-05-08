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

// For monolingual tasks
trait WordProblem extends AProblem {
  val wordIndexer = new Indexer[String]
  def W = wordIndexer.size // Number of words
  def wstr(w:Int) = wordIndexer.getObject(w)
  var wordFreqs : Array[Int] = null // Word frequencies

  case class WordExample[Widget](words:Array[Int], trueWidget:Widget) extends AExample[Widget] {
    def numTokens = words.size
  }

  trait WordModel[Widget,Params<:AParams,Performance<:APerformance[Widget],Example<:WordExample[Widget],InferState<:AInferState[Widget,Example,Params]]
      extends AModel[Widget,Params,Performance,Example,InferState] {
    override def logStats = {
      super.logStats
      putLogRec("numWords", W)
    }

    override def readExamples = {
      super.readExamples
      wordFreqs = new Array[Int](W)
      examples.foreach { ex => ex.words.foreach(wordFreqs(_) += 1) }
    }
    
  }
  
}
