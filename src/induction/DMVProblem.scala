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

case class DMVProblem(opts:Options) extends WordProblem {
  type Widget = DepTree

  var localWordIndexer : Array[Indexer[Int]] = null // Set later
  def WI(w:Int) = localWordIndexer(w).size
  def wistr(w1:Int, w2i:Int) = wstr(localWordIndexer(w1).getObject(w2i))

  val D_LEFT = 0; val D_RIGHT = 1; val D = 2 // Left, right
  val dstr = Array("left", "right")
  val R_LEFT0 = 0; val R_LEFT1 = 1; val R_RIGHT0 = 2; val R_RIGHT1 = 3; val R = 4 // {left, right} x {adj, non-adj}
  val rstr = Array("left0", "left1", "right0", "right1")
  val F_CONT = 0; val F_STOP = 1; val F = 2 // Continue, stop
  val fstr = Array("continue", "stop")

  // starts[w] = probability of starting with word w
  // deps[w1][dir][w2] = probability of w1 generating w2 in direction dir
  // continues[w1][dir/adj][flag] = probability of w1 continuing in direction dir with adjacency adj and flag
  case class Params(starts:ProbVec, continues:Array[Array[ProbVec]], deps:Array[Array[ProbVec]]) extends AParams {
    def foreachVec(f:(ProbVec => Any)) = {
      f(starts)
      continues.foreach { v => v.foreach(f(_)) }
      deps.foreach { v => v.foreach(f(_)) }
    }
    def output(puts:(String => Any)) = {
      Utils.foreachSorted(starts.getProbs, opts.numOutputParams, true, { (w:Int,v:Double) =>
        puts(String.format("S %s\t%s", wstr(w), fmt(v)))
      })
      puts("")

      Utils.foreachSorted(wordFreqs, true, { (w:Int,freq:Int) =>
        foreach(R, { r:Int =>
          Utils.foreachSorted(continues(w)(r).getProbs, true, { (f:Int,v:Double) =>
            puts(String.format("C %s %s %s\t%s", wstr(w), rstr(r), fstr(f), fmt(v)))
          })
          puts("")
        })
      })
      foreach(W, { w1:Int =>
        foreach(D, { d:Int =>
          Utils.foreachSorted(deps(w1)(d).getProbs, opts.numOutputParams, true, { (w2i:Int,v:Double) =>
            puts(String.format("D %s %s %s\t%s", wstr(w1), dstr(d), wistr(w1, w2i), fmt(v)))
          })
          puts("")
        })
      })
    }
  }

  class Performance extends APerformance[Widget] {
    val directed = new StatFig
    val undirected = new StatFig
    def add(trueTree:DepTree, predTree:DepTree) {
      if (trueTree != null) {
        foreach(Utils.same(trueTree.N, predTree.N), { i:Int =>
          if (!trueTree.isRoot(i)) {
            val pi = trueTree.parent(i)
            directed.add(predTree.parent(i) == pi)
            undirected.add(predTree.parent(i) == pi || predTree.parent(pi) == i)
          }
        })
      }
    }
    def output(puts:(String=>Any)) = {
      puts("Directed: " + directed)
      puts("Undirected: " + undirected)
    }
    def accuracy = directed.mean
  }

  type Example = WordExample[Widget]

  case class InferState(override val ex:Example, override val params:Params, override val counts:Params, override val ispec:InferSpec, harmonic:Boolean) extends
      AHypergraphInferState[Widget,Example,Params](ex, params, counts, ispec) {
    def words = ex.words
    def N = words.length
    def newWidget = new DepTree(N)

    def createHypergraph(H:Hypergraph[Widget]) = {
      // Map indices 
      val wi = map(N, { i:Int =>
        map(N, { j:Int => localWordIndexer(words(i)).indexOf(words(j)) })
      })

      //H.debug = true

      val tol = 1e-10

      // By default, assume we're going to stop on adjacent (i.e., generate no children)
      // Later, if we decide to generate children, we're going to divide out the potential for stopping
      // NOTE: if the probability of stopping on adjacent is 0, then we ignore it
      if (!harmonic) {
        val defaultNode = "default"
        H.addProdNode(defaultNode)
        H.addEdge(H.prodStartNode, defaultNode)
        foreach(N, { i:Int => if (!trueInfer || ex.trueWidget.parent(i) == i)
          H.addEdge(defaultNode, new Info {
            val lcont = get(params.continues(words(i))(R_LEFT0), F_STOP)
            val rcont = get(params.continues(words(i))(R_RIGHT0), F_STOP)
            //def getWeight = 1.0
            def getWeight = (if (lcont > tol) lcont else 1.0) *
                            (if (rcont > tol) rcont else 1.0)
            def setPosterior(v:Double) = {
              //dbg("DEFAULT: SET " + i + " " + v)
              if (lcont > tol) update(counts.continues(words(i))(R_LEFT0), F_STOP, v)
              if (rcont > tol) update(counts.continues(words(i))(R_RIGHT0), F_STOP, v)
            }
            def choose(widget:Widget) = widget
          })
        })
      }

      def harmonicWeight(i:Int, j:Int, adj:Boolean) = 1.0/((i-j).abs+2) // * (if (adj) 1.0 else 1.0/((i-j).abs+3))

      def link(i:Int, j:Int, r:Int) = {
        val d = { // Direction
          if (r == R_LEFT0 || r == R_LEFT1) D_LEFT
          else if(r == R_RIGHT0 || r == R_RIGHT1) D_RIGHT
          else { fail("Bad: "+r); -1 }
        }
        if (r == R_LEFT0 || r == R_RIGHT0) { // Adjacent
          val rr = { // Non-adjacent version
            if (r == R_LEFT0) R_LEFT1
            else if (r == R_RIGHT0) R_RIGHT1
            else { fail("Bad: "+r); -1 }
          }
          new Info {
            val cont = get(params.continues(words(i))(r), F_STOP)
            //def getWeight = 1.0
            def getWeight = {
              if (harmonic) harmonicWeight(i, j, true)
              else {
                get(params.deps(words(i))(d), wi(i)(j)) *
                get(params.continues(words(i))(r), F_CONT) *
                // Trade in non-adjacent stop for adjacent stop
                get(params.continues(words(i))(rr), F_STOP) /
                (if (cont > tol) cont else 1.0)
              }
            }
            def setPosterior(v:Double) = {
              //dbg("ADJ: SET " + (i,j,r) + " " + v)
              update(counts.deps(words(i))(d), wi(i)(j), v)
              update(counts.continues(words(i))(r), F_CONT, v)
              update(counts.continues(words(i))(rr), F_STOP, v)
              if (cont > tol) update(counts.continues(words(i))(r), F_STOP, -v)
            }
            def choose(widget:Widget) = { widget.parent(j) = i; widget }
          }
        }
        else if(r == R_LEFT1 || r == R_RIGHT1) { // Non-adjacent
          new Info {
            //def getWeight = 1.0
            def getWeight = {
              if (harmonic) harmonicWeight(i, j, false)
              else {
                get(params.deps(words(i))(d), wi(i)(j)) *
                get(params.continues(words(i))(r), F_CONT)
              }
            }
            def setPosterior(v:Double) = {
              //dbg("NON-ADJ: SET " + (i,j,r) + " " + v)
              update(counts.deps(words(i))(d), wi(i)(j), v)
              update(counts.continues(words(i))(r), F_CONT, v)
            }
            def choose(widget:Widget) = { widget.parent(j) = i; widget }
          }
        }
        else
          throw new RuntimeException("Bad")
      }

      // For each span [i,j] and state of the endpoints, (zi, zj), sum over sub-dependency structures
      // Convention: first attach left argments from j
      val Z0 = 0 // Needs to be generated by someone in this span
      val Z1 = 1 // Already generated by someone outside the span
      val Z1m = 2 // Already generated by someone outside the span, needs to generate someone in this span
      val Z = 3
      def gen(i:Int, j:Int, zi:Int, zj:Int) : Object = {
        assert (i <= j)
        if (i == j) { // Base case
          if ((zi == Z1 || zj == Z1) && // i is generated by someone
              (zi != Z1m && zj != Z1m)) // i doesn't need to generate
            H.endNode
          else
            H.invalidNode
        }
        else if (i < j && zi == Z0 && zj == Z0) // No one to generate i and j
          H.invalidNode
        /*else if (i+1 == j && ((zi == Z1m && zj != Z0) || (zj == Z1m && zi != Z0)))
          // i wants to generate, but j already generated or vice-versa
          // Not strictly necessary, but shortcuts useless logic later
          H.invalidNode*/
        else {
          val node = (i, j, zi, zj)
          if (H.addSumNode(node)) {
            zj match {
              case Z1 =>
                H.addEdge(node, gen(i, j-1, zi, Z0)) // No link from j
                H.addEdge(node, gen(i, j, zi, Z1m)) // Link (somewhere) from j
              case Z1m =>
                foreach((if (zi == Z0) i else i+1), j, { k:Int => // i ... k <- j
                  if (!trueInfer || ex.trueWidget.parent(k) == j) {
                    H.addEdge(node, gen(i, k, zi, Z1), gen(k, j-1, Z1, Z0), link(j, k, R_LEFT0)) // First link
                    H.addEdge(node, gen(i, k, zi, Z1), gen(k, j, Z1, Z1m), link(j, k, R_LEFT1)) // Not first link
                  }
                })
              case Z0 =>
                assert (zi != Z0)
                foreach(i+1, j+1, { k:Int => // i -> k ... j
                  if (!trueInfer || ex.trueWidget.parent(k) == i) {
                    H.addEdge(node, gen(i+1, k, Z0, Z1), gen(k, j, Z1, zj), link(i, k, R_RIGHT0)) // First link
                    H.addEdge(node, gen(i, k, Z1m, Z1), gen(k, j, Z1, zj), link(i, k, R_RIGHT1)) // Not first link
                  }
                })
            }
            //if (H.numEdges(node) == 0) fail(node)
          }
          node
        }
      }

      // Choose the root
      val headNode = "head"
      H.addSumNode(headNode)
      H.addEdge(H.prodStartNode, headNode)
      foreach(N, { i:Int =>
        H.addEdge(headNode, gen(0, i, Z0, Z1), gen(i, N-1, Z1, Z0), new Info {
          def getWeight = get(params.starts, words(i))
          def setPosterior(v:Double) = update(counts.starts, words(i), v)
          def choose(widget:Widget) = { widget.parent(i) = i; widget }
        })
      })
    }
  }

  class Model extends WordModel[Widget,Params,Performance,Example,InferState] {
    override def tokensToExample(tokens:Array[String], add:Example=>Any) = {
      opts.inputFormat match {
        case Options.InputFormat.raw =>
          // word word word ...
          if(tokens.length <= opts.maxExampleLength)
            add(new Example(getIndex(wordIndexer, tokens), null))
      }
    }
    def widgetToIntSeq(widget:Widget) = widget.parent
    def exampleToString(ex:Example) =
      map(ex.words.length, { i:Int =>
        i+wstr(ex.words(i))+"->"+ex.trueWidget.parent(i)
      }).mkString(" ")

    override def readExamples(path:String, maxExamples:Int, add:(Example=>Any)) = {
      if (opts.inputFormat == Options.InputFormat.mrg)
        foreachTree(opts, path, maxExamples, { tree:Tree =>
          val words = (if (opts.useTagsAsWords) tree.getPreTerminalYield else tree.getYield)
          if (words.size <= opts.maxExampleLength)
            add(new Example(getIndex(wordIndexer, Utils.toArray(words)),
                            (new DepTree.Converter).toDepTree(tree)))
        })
      else
        super.readExamples(path, maxExamples, add)
    }

    override def preInit = {
      // Form the bilexical index mapping based on examples
      localWordIndexer = map(W, new Indexer[Int])
      foreach(examples, { ex:Example =>
        val words = ex.words
        val N = words.length
        foreach(N, { i:Int =>
          foreach(N, { j:Int =>
            localWordIndexer(words(i)).getIndex(words(j))
          })
        })
      })
    }

    override def baitInitParams = {
      // Initialize with an E-step which puts a uniform distribution over z
      // This works for models with natural asymmetries such as word alignment and DMV,
      // but not for cluster-based models such as GMMs, PMMMs, HMMs,
      // where random initialization is preferred (need noise)
      track("baitInitParams: using harmonic initializer")
      val counts = newParams
      params.setUniform_!(1)
      Utils.parallel_foreach(opts.numThreads, examples, { (i:Int,ex:Example,log:Boolean) =>
        if(log) track("Example %s/%s", fmt(i), fmt(examples.length))
        new InferState(ex, params, counts, InferSpec(1, true, false, false, false, false, false, 1, -1), true).updateCounts
        if(log) end_track
      })
      params = counts
      params.optimize_!(opts.initSmoothing)
      end_track
    }

    def newInferState(ex:Example, params:Params, counts:Params, ispec:InferSpec) = new InferState(ex, params, counts, ispec, false)
    def newPerformance = new Performance
    def newParams = new Params(ProbVec.zeros(W), ProbVec.zeros3(W, R, F), ProbVec.zeros3(W, D, { (w:Int,d:Int) => WI(w) }))

    def genExample = {
      val N = opts.genMaxTokens
      // TODO
      val words = null
      new Example(words, new DepTree(null))
    }
  }
  def newModel = new Model
}
