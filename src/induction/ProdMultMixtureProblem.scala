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

case class ProdMultMixtureProblem(opts:Options) extends TaggingProblem {
  type Widget = Int

  case class Params(starts:ProbVec, emissions:Array[ProbVec]) extends AParams {
    def foreachVec(f:(ProbVec => Any)) = {
      f(starts)
      emissions.foreach(f(_))
    }
    def output(puts:(String => Any)) = {
      Utils.foreachSorted(starts.getProbs, true, { (a:Int,v:Double) =>
        puts(String.format("S %s\t%s", fmt(a), fmt(v)))
      })
      puts("")

      foreach(K, { a:Int =>
        Utils.foreachSorted(emissions(a).getProbs, opts.numOutputParams, true, { (w:Int,v:Double) =>
          puts(String.format("E %s %s\t%s", fmt(a), wstr(w), fmt(v)))
        })
        puts("")
      })
    } 
  }

  class Performance extends TaggingPerformance[Widget] {
    def add(trueTag:Widget, predState:Widget) : Unit = if (trueTag != -1) incr(trueTag, predState)
  }

  type Example = WordExample[Widget]

  case class InferState(override val ex:Example, override val params:Params, override val counts:Params, override val ispec:InferSpec) extends
      AHypergraphInferState[Widget,Example,Params](ex, params, counts, ispec) {
    def words = ex.words
    def N = words.length
    def newWidget = -1

    def createHypergraph(H:Hypergraph[Widget]) = {
      def allowed(a:Int) = !trueInfer || ex.trueWidget == a
      def gen(a:Int) = { // Generate words given tag is a; hypergraph node is a
        if (N == 0)
          H.endNode
        else {
          H.addProdNode(a)
          foreach(N, { i:Int =>
            H.addEdge(a, new Info {
              def getWeight = get(params.emissions(a), words(i))
              def setPosterior(v:Double) = update(counts.emissions(a), words(i), v)
              def choose(widget:Widget) = widget
            })
          })
          a
        }
      }

      foreach(K, { a:Int => if (allowed(a))
        H.addEdge(H.sumStartNode, gen(a), new Info {
          def getWeight = get(params.starts, a)
          def setPosterior(v:Double) = update(counts.starts, a, v)
          def choose(widget:Widget) = a
        })
        /*H.addEdge(H.sumStartNode, gen(a), new Info {
          def getWeight = 1.0
          def setPosterior(v:Double) = { }
          def choose(widget:Widget) = a
        })*/
      })
    }
  }

  class Model extends TaggingModel[Int,Params,Performance,Example,InferState] {
    def widgetToIntSeq(widget:Widget) = Array(widget)
    def exampleToString(ex:Example) =
      tstr(ex.trueWidget) + " " + getObject(wordIndexer, ex.words).mkString(" ")

    override def tokensToExample(tokens:Array[String], add:Example=>Any) = {
      opts.inputFormat match {
        case Options.InputFormat.raw =>
          // word word word ...
          if(tokens.length <= opts.maxExampleLength)
            add(new Example(getIndex(wordIndexer, tokens), -1))
        case Options.InputFormat.tag =>
          // <document label> word word word ...
          if(tokens.length-1 <= opts.maxExampleLength)
            add(new Example(getIndex(wordIndexer, tokens.subArray(1, tokens.length)), tagIndexer.getIndex(tokens(0))))
      }
    }

    def newInferState(ex:Example, params:Params, counts:Params, ispec:InferSpec) = new InferState(ex, params, counts, ispec)
    def newPerformance = new Performance
    def newParams = new Params(ProbVec.zeros(K), ProbVec.zeros2(K, W))
    def genExample = {
      val k = genSample(params.starts)
      val words = map(opts.genMaxTokens, genSample(params.emissions(k)))
      new Example(words, k)
    }

    override def baitInitParams = {
      // For mixture example, really works
      // Find the most common word and put it in a cluster
      // initNoise = 0 for everything else
      val (fw, c) = Utils.argmaxmax(map(W, { w:Int =>
        var n = 0.0
        examples.foreach { ex => n += Utils.sum(ex.words.map { ww => if (ww == w) 1.0 else 0.0 }) }
        n
      }))
      logs("Bait initializer: most frequent word %s occurred %s times, adding an extra count to first cluster", wstr(fw), fmt(c))
      params.starts.set_!(1)
      foreach(K, { a:Int => params.emissions(a).set_!(1) })
      params.emissions(0).addCount_!(fw, 0.001)
    }

    override def artificialInitParams = {
      // Replace tags and words what we want
      wordIndexer.clear; foreach(opts.artNumWords, { w:Int => wordIndexer.getIndex("w"+w)  })
      tagIndexer.clear; foreach(K, { a:Int => tagIndexer.getIndex("t"+a)  })

      params = newParams
      params.starts.set_! { w:Int => 1 } // Uniform

      // Each cluster has a emission distribution over some set of words
      // Partition the words based on clusters; a cluster generates a proper word with probability 1-artAlpha
      if (true) {
        foreach(K, { a:Int =>
          foreach(W, { w:Int =>
            params.emissions(a).addCount_!(w, if (w % K == a) 1-opts.artAlpha else opts.artAlpha)
          })
        })
      }

      if (false) {
        // Each word picks a posterior distribution over tags; then renormalize
        // Control peakiness
        val random = new Random(42)
        foreach(W, { w:Int =>
          val posteriors = sampleDirichlet(random, K, opts.artAlpha)
          foreach(K, { a:Int => params.emissions(a).addCount_!(w, posteriors(a)) })
        })
      }
      params.optimize_!(0)
    }
  }
  def newModel = new Model
}
