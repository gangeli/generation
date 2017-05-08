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

case class GMMProblem(opts:Options) extends TaggingProblem {
  type Widget = Int
  val D = opts.genMaxTokens

  // WARNING: means might be treated by like a probability vector when it's not
  case class Params(starts:ProbVec, means:Array[ProbVec]) extends AParams {
    def foreachVec(f:(ProbVec => Any)) = {
      f(starts)
      means.foreach(f(_))
    }
    def output(puts:(String => Any)) = {
      Utils.foreachSorted(starts.getProbs, true, { (a:Int,v:Double) =>
        puts(String.format("S %s\t%s", fmt(a), fmt(v)))
      })
      puts("")

      foreach(K, { a:Int =>
        puts(String.format("M %s\t%s", fmt(a), fmt1(means(a).getProbs)))
      })
      puts("")
    } 
  }

  class Performance extends TaggingPerformance[Widget] {
    def add(trueTag:Widget, predState:Widget) : Unit = if (trueTag != -1) incr(trueTag, predState)
  }

  case class Example(x:Array[Double], trueWidget:Widget) extends AExample[Widget] {
    def numTokens = x.length
  }

  case class InferState(override val ex:Example, override val params:Params, override val counts:Params, override val ispec:InferSpec) extends
      AHypergraphInferState[Widget,Example,Params](ex, params, counts, ispec) {
    def newWidget = -1

    def createHypergraph(H:Hypergraph[Widget]) = {
      def allowed(a:Int) = !trueInfer || ex.trueWidget == a
      val N = ex.x.length

      def gen(a:Int) = { // Generate words given tag is a; hypergraph node is a
        H.addProdNode(a)
        H.addEdge(a, new LogInfo {
          def getLogWeight =
            (1.0/temperature) * (-Utils.distL2Squared(params.means(a).getProbs, ex.x)) / (2*opts.gmmVariance)
          def setPosterior(v:Double) = update(counts.means(a), ex.x, v)
          def choose(widget:Widget) = widget
        })
        a
      }

      foreach(K, { a:Int => if (allowed(a))
        H.addEdge(H.sumStartNode, gen(a), new Info {
          def getWeight = get(params.starts, a)
          def setPosterior(v:Double) = update(counts.starts, a, v)
          def choose(widget:Widget) = a
        })
      })
    }
  }

  class Model extends TaggingModel[Int,Params,Performance,Example,InferState] {
    def widgetToIntSeq(widget:Widget) = Array(widget)
    def exampleToString(ex:Example) = tstr(ex.trueWidget) + " " + fmt1(ex.x)

    override def tokensToExample(tokens:Array[String], add:Example=>Any) = {
      opts.inputFormat match {
        case Options.InputFormat.raw =>
          // <x_1> ... <x_d>
          add(new Example(tokens.map(_.toDouble), -1))
        case Options.InputFormat.tag =>
          // <y> <x_1> ... <x_d>
          add(new Example(tokens.subArray(1, tokens.length).map(_.toDouble), tagIndexer.getIndex(tokens(0))))
      }
    }

    def newInferState(ex:Example, params:Params, counts:Params, ispec:InferSpec) = new InferState(ex, params, counts, ispec)
    def newPerformance = new Performance
    def newParams = new Params(ProbVec.zeros(K), ProbVec.zeros2(K, D))
    def genExample = {
      val y = genSample(params.starts)
      val x = map(D, { d:Int => fig.prob.Gaussian.sample(opts.genRandom,params.means(y).getProb(d), opts.gmmVariance) })
      new Example(x, y)
    }

    override def baitInitParams = {
      params.starts.set_!(1.0/K)

      // For each cluster, pick its center to be a point furthest away from previous points
      params.means(0).addCount_!(examples(opts.initRandom.nextInt(examples.length)).x, 1)

      foreach(1, K, { k:Int => // For each cluster...
        val m = Utils.argmax(examples.length, { i:Int => // choose the point that maximizes the...
          Utils.min(k, { j:Int => Utils.distL2(examples(i).x, params.means(j).getProbs) }) // minimum distance from a previous center
        })
        params.means(k).addCount_!(examples(m).x, 1)
      })
    }

    override def artificialInitParams = {
      // Replace tags with what we want
      tagIndexer.clear; foreach(K, { a:Int => tagIndexer.getIndex("t"+a)  })

      params = newParams
      params.starts.set_! { w:Int => 1 } // Uniform

      // Draw means
      foreach(K, { a:Int =>
        params.means(a).addCount_!(map(D, { d:Int => (opts.genInitRandom.nextDouble*2-1)*opts.gmmGenRange }), 1)
      })
      //dbg(fmt1(params.means(0).getProbs))
    }
  }
  def newModel = new Model
}
