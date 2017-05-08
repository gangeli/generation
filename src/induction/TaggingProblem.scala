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

trait TaggingProblem extends WordProblem { // FUTURE: should only need to derive from AProblem
  val tagIndexer = new Indexer[String] // The observed tags
  def T = tagIndexer.size // Number of observed tags
  def tstr(t:Int) = tagIndexer.getObject(t)
  def K = opts.K // Number of hidden states

  abstract class TaggingPerformance[Widget] extends APerformance[Widget] {
    val counts = new Array[Array[Int]](T, K)
    var totalCount = 0

    def incr(trueTag:Int, predState:Int) : Unit = {
      counts(trueTag)(predState) += 1
      totalCount += 1
    }
    def greedyMap(k:Int) = Utils.argmax(T, { t:Int => counts(t)(k) }) // state to best tag
    def accuracy = {
      if (T == 0) Double.NaN
      else {
        // Greedily map each predicted state k to the best true tag t
        1.0 * Utils.sum(K, { k:Int => counts(greedyMap(k))(k) }) / totalCount
      }
    }

    def output(puts:(String=>Any)) = {
      if (T != 0) {
        // Print confusion matrix: predicted states are rows, true tags are columns
        val ts = Utils.sortedIndices(Utils.toArray(tagIndexer.getObjects), false)
        val table =
          Array(Array("") ++ ts.map(tstr)) ++
          map(K, { k:Int =>
            Array(k+"("+tstr(greedyMap(k))+")") ++ ts.map { t:Int => fmt(counts(t)(k)) }
          })
        puts("accuracy: "+fmt(accuracy))
        Utils.formatTable(table, { (r:Int,c:Int) => if (c == 0) -1 else if (r == 0) 0 else 1 }).foreach(puts(_))
      }
    }
  }

  trait TaggingModel[Widget,Params<:AParams,Performance<:APerformance[Widget],Example<:AExample[Widget],InferState<:AInferState[Widget,Example,Params]]
      extends AModel[Widget,Params,Performance,Example,InferState] {
    override def logStats = {
      super.logStats
      putLogRec("numTags", T)
      putLogRec("numStates", K)
    }
  }
}
