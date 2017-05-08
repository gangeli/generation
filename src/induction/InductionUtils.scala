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

// Random utilties
object InductionUtils {
  def getIndex(indexer:Indexer[String], v:Array[String]) = v.map { s => indexer.getIndex(s) }
  def getObject(indexer:Indexer[String], v:Array[Int]) = v.map { i => if (i == -1) "*NONE*" else indexer.getObject(i) }

  type Tree = edu.berkeley.nlp.ling.Tree[String]

  def getIf(b:Boolean, x: =>Int) = if (b) x else -1
  def getIf(b:Boolean, x: =>Double) = if (b) x else Double.NaN
  def getIf[A](b:Boolean, x: =>Array[A]) = if (b) x else null

  def foreachTree(opts:Options, path:String, maxTrees:Int, f:(Tree => Any)) = {
    import edu.berkeley.nlp.ling._
    val treeTransformer = new Trees.StandardTreeNormalizer
    val in = IOUtils.openIn(path)
    val treeIterator = new Trees.PennTreeReader(in)
    var n = 0
    while(n < maxTrees && treeIterator.hasNext) {
      val rawTree = treeTransformer.transformTree(treeIterator.next)
      val tree = Utils.applyIf(opts.removePunctuation, TreeUtils.removePunctuation, rawTree)
      f(tree)
      n += 1
    }
    in.close
  }

  def sampleDirichlet(random:Random, n:Int, alpha:Double) =
    fig.prob.Dirichlet.sample(random, Utils.set_!(new Array[Double](n), alpha))

  def stopWatch[A](name:String)(f: =>A) = {
    import fig.basic.StopWatchSet
    StopWatchSet.begin(name)
    val x = f
    StopWatchSet.end
    x
  }
}
