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

case class HMMProblem(opts:Options) extends TaggingProblem {
  type Widget = Array[Int]

  case class Params(starts:ProbVec, transitions:Array[ProbVec], emissions:Array[ProbVec]) extends AParams {
    def foreachVec(f:(ProbVec => Any)) = {
      f(starts)
      transitions.foreach(f(_))
      emissions.foreach(f(_))
    }
    def output(puts:(String => Any)) = {
      Utils.foreachSorted(starts.getProbs, true, { (a:Int,v:Double) =>
        puts(String.format("S %s\t%s", fmt(a), fmt(v)))
      })
      puts("")

      foreach(K, { a:Int =>
        Utils.foreachSorted(transitions(a).getProbs, true, { (b:Int,v:Double) =>
          puts(String.format("T %s %s\t%s", fmt(a), fmt(b), fmt(v)))
        })
        puts("")
      })

      foreach(K, { a:Int =>
        Utils.foreachSorted(emissions(a).getProbs, opts.numOutputParams, true, { (w:Int,v:Double) =>
          puts(String.format("E %s %s\t%s", fmt(a), wstr(w), fmt(v)))
        })
        puts("")
      })
    }
  }

  class Performance extends TaggingPerformance[Widget] {
    def add(trueTags:Widget, predStates:Widget) : Unit =
      if(trueTags != null)
        foreach(trueTags, predStates, { (t:Int,k:Int) => incr(t, k) })
  }

  type Example = WordExample[Widget]

  case class InferState(override val ex:Example, override val params:Params, override val counts:Params, override val ispec:InferSpec) extends
      AHypergraphInferState[Widget,Example,Params](ex, params, counts, ispec) {
    def words = ex.words
    def N = words.length
    def newWidget = new Widget(N)

    def createHypergraph(H:Hypergraph[Widget]) = {
      def allowed(i:Int, a:Int) = !trueInfer || ex.trueWidget(i) == a
      def gen(i:Int, a:Int) : Object = { // Generate the rest of the sequence from position i with state a
        if (i == N-1)
          H.endNode
        else {
          val node = (i,a)
          if (H.addSumNode(node)) {
            foreach(K, { b:Int => if (allowed(i+1, b))
              H.addEdge(node, gen(i+1, b), new Info {
                def getWeight = get(params.transitions(a), b) * get(params.emissions(b), words(i+1))
                def setPosterior(v:Double) = {
                  update(counts.transitions(a), b, v)
                  update(counts.emissions(b), words(i+1), v)
                }
                def choose(widget:Widget) = { widget(i+1) = b; widget }
              })
            })
          }
          node
        }
      }

      foreach(K, { a:Int => if (allowed(0, a))
        H.addEdge(H.sumStartNode, gen(0, a), new Info { // Generate the state at position 0
          def getWeight = get(params.starts, a) * get(params.emissions(a), words(0))
          def setPosterior(v:Double) = {
            update(counts.starts, a, v)
            update(counts.emissions(a), words(0), v)
          }
          def choose(widget:Widget) = { widget(0) = a; widget }
        })
      })
    }
  }

  class Model extends TaggingModel[Widget,Params,Performance,Example,InferState] {
    def widgetToIntSeq(widget:Widget) = widget

    override def tokensToExample(tokens:Array[String], add:(Example => Any)) = {
      opts.inputFormat match {
        case Options.InputFormat.raw =>
          // word word word ...
          if(tokens.length <= opts.maxExampleLength)
            add(new Example(getIndex(wordIndexer, tokens), null))
        case Options.InputFormat.tag =>
          // word tag word tag word ...
          if(tokens.length/2 <= opts.maxExampleLength)
            add(new Example(getIndex(wordIndexer, Utils.slice(tokens, 0, tokens.size, 2)),
                            getIndex(tagIndexer, Utils.slice(tokens, 1, tokens.size, 2))))
      }
    }
    def exampleToString(ex:Example) =
      Utils.createArray({add:(String => Any) =>
        foreach(ex.words.length, { i:Int =>
          add(wstr(ex.words(i)))
          add(tstr(ex.trueWidget(i)))
        })
      }).mkString(" ")

    override def readExamples(path:String, maxExamples:Int, add:(Example=>Any)) = {
      if (opts.inputFormat == Options.InputFormat.mrg)
        foreachTree(opts, path, maxExamples, { tree:Tree =>
          val words = tree.getYield
          if (words.size <= opts.maxExampleLength)
            add(new Example(getIndex(wordIndexer, Utils.toArray(words)),
                            getIndex(tagIndexer, Utils.toArray(tree.getPreTerminalYield))))
        })
      else
        super.readExamples(path, maxExamples, add)
    }

    def newInferState(ex:Example, params:Params, counts:Params, ispec:InferSpec) = new InferState(ex, params, counts, ispec)
    def newPerformance = new Performance
    def newParams = new Params(ProbVec.zeros(K), ProbVec.zeros2(K, K), ProbVec.zeros2(K, W))

    def genExample = {
      val N = opts.genMaxTokens
      val states = new Array[Int](N)
      states(0) = genSample(params.starts)
      foreach(1, N, { i:Int => states(i) = genSample(params.transitions(states(i-1))) })
      val words = map(N, { i:Int => genSample(params.emissions(states(i))) })
      new Example(words, states)
    }
  }
  def newModel = new Model
}
