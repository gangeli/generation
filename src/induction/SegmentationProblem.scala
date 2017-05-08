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

case class SegmentationProblem(opts:Options) extends WordProblem {
  type Widget = Array[Int] // For each position i, indicator of whether there is a segment boundary separating i-1 and i
  type Phrase = SubArray[Int]

  var phraseIndexer = new Indexer[Phrase]
  def P = phraseIndexer.size
  def pstr(p:Int) = phraseIndexer.getObject(p).map(wstr(_)).mkString(" ")
  def L = opts.maxPhraseLength
  def end(i:Int, N:Int) = ((i+L) min N) + 1 // Largest start of following
  val unkPhrase = new SubArray[Int](new Array[Int](0), 0, 0)
  var unkp = -1 // Set later

  val segPenalty = map(L+1, { l:Int => Math.exp(-Math.pow(l, opts.segPenalty)) })

  val F_CONT = 0; val F_STOP = 1; val F = 2 // Continue, stop
  val fstr = Array("continue", "stop")

  // continues = probability of generating an additional segment 
  // emissions[p] = probability of generating phrase p
  case class Params(continues:ProbVec, emissions:ProbVec) extends AParams {
    def foreachVec(f:(ProbVec => Any)) = {
      f(continues)
      f(emissions)
    }
    def output(puts:(String => Any)) = {
      Utils.foreachSorted(continues.getProbs, true, { (f:Int,v:Double) =>
        puts(String.format("C %s\t%s", fstr(f), fmt(v)))
      })
      puts("")

      Utils.foreachSorted(emissions.getProbs, opts.numOutputParams, true, { (p:Int,v:Double) =>
        puts(String.format("E %s\t%s", pstr(p), fmt(v)))
      })
      puts("")
    }
  }

  class Performance extends APerformance[Widget] {
    val result = new fig.basic.EvalResult

    // Return a set of segments represented by (start,end) pairs
    private def getSegs(widget:Widget) = {
      val segs = new java.util.HashSet[(Int,Int)]
      val N = widget.length
      var start = 0
      foreach(1, N, { i:Int =>
        if (widget(i) == 1) { // Beginning of segment
          segs.add((start,i))
          start = i
        }
      })
      segs.add(start, N)
      segs
    }

    def add(trueWidget:Widget, predWidget:Widget) {
      if (trueWidget != null) {
        val trueSegs = getSegs(trueWidget)
        val predSegs = getSegs(predWidget)
        //dbg(trueSegs.size + " " + predSegs.size)
        result.add(trueSegs, predSegs)
        //dbg(result.count)
      }
    }
    def output(puts:(String=>Any)) = puts(result.toString)
    def accuracy = result.f1
  }

  type Example = WordExample[Widget]

  case class InferState(override val ex:Example, override val params:Params, override val counts:Params, override val ispec:InferSpec) extends
      AHypergraphInferState[Widget,Example,Params](ex, params, counts, ispec) {
    def words = ex.words
    def N = words.length
    def newWidget = new Widget(N)

    def createHypergraph(H:Hypergraph[Widget]) = {
      // Get phrase indices
      val phrases = map(N, { i:Int =>
        map(i+1, end(i, N), { j:Int =>
          val k = phraseIndexer.indexOf(new SubArray(ex.words, i, j))
          if (k == -1) unkp else k
        })
      })

      //H.debug = true

      // Generate segmentation starting from i
      def gen(i:Int) : Any = {
        if (i == N) H.endNode
        else {
          var node = i
          if(H.addSumNode(node)) {
            foreach(i+1, end(i, N), { j:Int =>
              if (!trueInfer || ((j == N || ex.trueWidget(j) == 1) && H.numEdges(i) == 0)) {
                H.addEdge(node, gen(j), new Info {
                  val p = phrases(i)(j-i-1)
                  def getWeight = get(params.continues, F_CONT) *
                                  get(params.emissions, p) *
                                  segPenalty(j-i)
                  def setPosterior(v:Double) = {
                    //dbg("incr %s %s %s", fmt(p), pstr(p), fmt(v))
                    update(counts.continues, F_CONT, v)
                    update(counts.emissions, p, v)
                  }
                  def choose(widget:Widget) = { widget(i) = 1; widget }
                })
              }
            })
          }
          node
        }
      }

      H.addEdge(H.prodStartNode, gen(0), new Info {
        def getWeight = get(params.continues, F_STOP)
        def setPosterior(v:Double) = update(counts.continues, F_STOP, v)
        def choose(widget:Widget) = widget
      })
    }
  }

  class Model extends WordModel[Widget,Params,Performance,Example,InferState] {
    override def tokensToExample(tokens:Array[String], add:Example=>Any) = {
      if (opts.segChars) {
        // <char><char><space><char><char>
        val N = Utils.sum(tokens.length, {i:Int => tokens(i).length})
        if (N <= opts.maxExampleLength) {
          val words = new Array[Int](N)
          val segmentation = new Array[Int](N)
          var i = 0
          tokens.foreach { s =>
            segmentation(i) = 1
            foreach(s.length, { j:Int => words(i) = wordIndexer.getIndex(""+s(j)); i += 1 })
          }
          assert (i == N)
          add(new Example(words, segmentation))
        }
      }
      else {
        // word word segBoundary word word 
        val numBoundaries = Utils.count(tokens, {s:String => s == opts.segBoundary})
        val N = tokens.length - numBoundaries
        if (N <= opts.maxExampleLength) {
          val words = new Array[Int](N)
          val segmentation = new Array[Int](N)
          var i = 0
          tokens.foreach { s =>
            if (s == opts.segBoundary)
              segmentation(i) = 1
            else {
              words(i) = wordIndexer.getIndex(s)
              i += 1
            }
          }
          add(new Example(words, segmentation))
        }
      }
    }

    override def readExamples(path:String, maxExamples:Int, add:(Example=>Any)) = {
      if (opts.inputFormat == Options.InputFormat.mrg)
        foreachTree(opts, path, maxExamples, { tree:Tree =>
          tokensToExample(Utils.toArray(tree.getYield), add)
        })
      else
        super.readExamples(path, maxExamples, add)
    }

    def widgetToIntSeq(widget:Widget) = widget
    def exampleToString(ex:Example) = {
      Utils.createArray({ add:(String=>Any) =>
        foreach(ex.words.length, { i:Int =>
          if (ex.trueWidget(i) == 1) add(opts.segBoundary)
          add(wstr(ex.words(i)))
        })
      }).mkString(" ")
    }

    override def logStats = {
      super.logStats
      putLogRec("numPhrases", P)
    }

    override def readExamples = {
      super.readExamples

      val numToUse = examples.length min opts.maxExamplesForPhrases
      track("Create phrase index from %s examples", fmt(numToUse))
      unkp = phraseIndexer.getIndex(unkPhrase)
      foreach(numToUse, { i:Int =>
        val ex = examples(i)
        val N = ex.words.length
        foreach(N, { i:Int =>
          foreach(i+1, end(i, N), { j:Int =>
            phraseIndexer.getIndex(new SubArray(ex.words, i, j))
          })
        })
      })
      end_track
    }

    def newInferState(ex:Example, params:Params, counts:Params, ispec:InferSpec) = new InferState(ex, params, counts, ispec)
    def newPerformance = new Performance
    def newParams = new Params(ProbVec.zeros(F), ProbVec.zeros(P))

    def genExample = {
      val N = opts.genMaxTokens
      // TODO
      val words = null
      new Example(words, null)
    }
  }
  def newModel = new Model
}
