package induction

import scala.collection.mutable.HashMap
import java.util.Random

import fig.basic.{IOUtils,Indexer,StatFig}
import fig.exec.Execution
import fig.exec.Execution.putLogRec
import fig.record.Record
import fig.basic.LogInfo.{logs,dbg,fail,error,track,end_track}

import tea._
import tea.Utils.{map,foreach,fmt,fmt1,fmt2,returnFirst,assertValid}

import InductionUtils._

// Types of rules:
//   nonterminal -> preterminal preterminal
//   nonterminal -> preterminal nonterminal
//   nonterminal -> nonterminal preterminal
//   nonterminal -> nonterminal nonterminal
//   preterminal -> terminal
case class PCFGProblem(opts:Options) extends WordProblem {
  type Widget = Array[Array[Int]] // For each span [i,j], store the state (-1 if no span there)
  // For the CCM, each constituent span generates a content and a context
  type Content = SubArray[Int]
  type Context = (Int,Int)

  val nonTagIndexer = new Indexer[String] // Nonterminal tags
  val preTagIndexer = new Indexer[String] // Preterminal tags
  def nT = nonTagIndexer.size
  def pT = preTagIndexer.size
  def ntstr(t:Int) = nonTagIndexer.getObject(t)
  def ptstr(t:Int) = if (t == -1) "(none)" else preTagIndexer.getObject(t)
  def nK = opts.K
  def pK = if (opts.fixPreTags) pT else opts.pK

  // For CCM potentials
  val ccm_? = opts.useCCMPotentials
  val contentIndexer = new Indexer[Content]
  val contextIndexer = new Indexer[Context]
  def CON = contentIndexer.size
  def CXT = contextIndexer.size
  def constr(con:Int) = contentIndexer.getObject(con).map(ptstr(_)).mkString(" ")
  def cxtstr(cxt:Int) = {
    val pair = contextIndexer.getObject(cxt)
    ptstr(pair._1)+","+ptstr(pair._2)
  }
  val unkContent = new SubArray[Int](new Array[Int](0), 0, 0)
  val unkContext = (-1, -1)
  var unkcon = -1 // Set later
  var unkcxt = -1 // Set later
  def numCCMClasses = 1 + (if (opts.ccmDependsOnState) nK else 1)

  val nonTagPartitions = Array(
    Array("S", "SINV", "SQ"),
    Array("SBAR", "SBARQ"),
    Array("NP", "NX"),
    Array("PP"),
    Array("VP"),
    Array("ADVP"),
    Array("QP"),
    Array("ADJP"),
    Array("WHNP", "WHPP", "WHADJP", "WHADVP"),
    Array("PRN", "CONJP", "RRC", "UCP"),
    Array("X", "NAC", "FRAG", "INTJ", "PRT"))

  val nonCollapsedTagMap = new HashMap[String,String]
  nonTagPartitions.foreach { a => a.foreach { tag => nonCollapsedTagMap(tag) = a(0) } }

  def treeToTagMatrix(N:Int, tree:Tree) = {
    var i = 0
    val matrix = Utils.set_!(new Array[Array[Int]](N, N), -1)
    // Right binarize
    // Replace unary chains with the lowest tag
    def recurseChildren(tag:String, children:Array[Tree], k:Int) : Unit = {
      if (k == children.length-1) // Only one child
        recurseNode(children(k))
      else {
        val left = i
        recurseNode(children(k))
        recurseChildren(tag, children, k+1)
        val right = i
        matrix(left)(right-1) = nonTagIndexer.getIndex(if (k > 0) "@"+tag else tag)
      }
    }
    def recurseNode(node:Tree) : Unit = {
      val tag = node.getLabel
      if(node.isPreTerminal) {
        matrix(i)(i) = preTagIndexer.getIndex(tag)
        i += 1
      }
      else
        recurseChildren(if (opts.collapseNonTags) nonCollapsedTagMap.getOrElse(tag, tag) else tag, Utils.toArray(node.getChildren), 0)
    }
    recurseNode(tree)
    //dbg(tree)
    //dbg(fmt2(matrix.map({a => getObject(tagIndexer, a)})))
    matrix
  }

  def intermediateTag_?(t:Int) = ntstr(t)(0) == '@'

  def renderTree(words:Array[Int], matrix:Array[Array[Int]]) = {
    def create(i:Int, j:Int) : Tree = {
      val k = matrix(i)(j)
      if (i == j) {
        val tag = if (opts.fixPreTags) ptstr(k) else "p"+k
        new Tree(tag, false, fig.basic.ListUtils.newList(new Tree(wstr(words(i)), false)))
      }
      else {
        val tag = "n"+k
        foreach(i, j, { k:Int => // Find the split point
          if (matrix(i)(k) != -1 && matrix(k+1)(j) != -1)
            return new Tree(tag, false,
              fig.basic.ListUtils.newList(create(i, k), create(k+1, j)))
        })
        throw new RuntimeException("Bad")
      }
    }
    edu.berkeley.nlp.ling.Trees.PennTreeRenderer.render(create(0, words.length-1))
  }

  // Types of production rules (children are either preterminal or nonterminal)
  val R_PP = 0; val R_PN = 1; val R_NP = 2; val R_NN = 3; val R = 4
  val rstr = Array("pp", "pn", "np", "nn")
  val decode = new Array[Int => (Int,Int)](R)
  val encode = new Array[(Int,Int) => Int](R)

  // emissions: distribution over words
  // types: distribution over rule types
  // productions: rule type -> distribution over pairs of states
  // contents, contexts: CCM phrase potentials
  case class Params(starts:ProbVec, emissions:Array[ProbVec], types:Array[ProbVec], productions:Array[Array[ProbVec]],
      contents:Array[ProbVec], contexts:Array[ProbVec]) extends AParams {
    def foreachVec(f:(ProbVec => Any)) = {
      f(starts)
      emissions.foreach(f(_))
      types.foreach(f(_))
      productions.foreach { a => a.foreach(f(_)) }
      contents.foreach(f(_))
      contexts.foreach(f(_))
    }
    def output(puts:(String => Any)) = {
      Utils.foreachSorted(starts.getProbs, true, { (a:Int,v:Double) =>
        puts(String.format("S %s\t%s", fmt(a), fmt(v)))
      })
      puts("")

      foreach(pK, { a:Int =>
        Utils.foreachSorted(emissions(a).getProbs, opts.numOutputParams, true, { (w:Int,v:Double) =>
          puts(String.format("E %s %s\t%s", fmt(a), wstr(w), fmt(v)))
        })
        puts("")
      })

      foreach(nK, { a:Int =>
        Utils.foreachSorted(types(a).getProbs, true, { (r:Int,v:Double) =>
          puts(String.format("T %s %s\t%s", fmt(a), rstr(r), fmt(v)))
        })
        puts("")
      })

      foreach(nK, { a:Int =>
        foreach(R, { r:Int =>
          val p1 = r == R_PP || r == R_PN
          val p2 = r == R_PP || r == R_NP
          Utils.foreachSorted(productions(a)(r).getProbs, opts.numOutputParams, true, { (bc:Int,v:Double) =>
            val (b,c) = decode(r)(bc)
            val bstr = if (opts.fixPreTags && p1) preTagIndexer.getObject(b) else ""+b
            val cstr = if (opts.fixPreTags && p2) preTagIndexer.getObject(c) else ""+c
            puts(String.format("P %s %s %s,%s\t%s", fmt(a), rstr(r), bstr, cstr, fmt(v)))
          })
          puts("")
        })
      })

      foreach(numCCMClasses, { a:Int =>
        Utils.foreachSorted(contents(a).getProbs, opts.numOutputParams, true, { (p:Int,v:Double) =>
          puts(String.format("Con %s %s\t%s", fmt(a), constr(p), fmt(v)))
        })
        puts("")
      })

      foreach(numCCMClasses, { a:Int =>
        Utils.foreachSorted(contexts(a).getProbs, opts.numOutputParams, true, { (p:Int,v:Double) =>
          puts(String.format("Cxt %s %s\t%s", fmt(a), cxtstr(p), fmt(v)))
        })
        puts("")
      })
    }
  }

  class Performance extends APerformance[Widget] {
    // Last dimensions correspond to not-a-span
    val counts = new Array[Array[Int]](nT+1, nK+1)
    val unlabeledResult = new fig.basic.EvalResult

    def add(trueWidget:Widget, predWidget:Widget) : Unit = {
      if(trueWidget != null) {
        val N = Utils.same(trueWidget.length, predWidget.length)
        foreach(N, { i:Int =>
          foreach(i, N, { j:Int =>
            val isRoot = i == 0 && j == N-1
            val isLeaf = i == j
            if (!isRoot && !isLeaf) { // Skip root and leaves
              val t = trueWidget(i)(j)
              val p = predWidget(i)(j)
              // The intermediate tags are not really there
              counts(if (t == -1 || intermediateTag_?(t)) nT else t)(if (p == -1) nK else p) += 1
              unlabeledResult.add(t != -1 && !intermediateTag_?(t), p != -1)
            }
          })
        })
      }
    }

    def greedyMap(k:Int) = Utils.argmax(nT, { t:Int => counts(t)(k) }) // state to best tag

    def labeledResult = {
      new fig.basic.EvalResult(
        Utils.sum(nK, { k:Int => counts(greedyMap(k))(k) }),
        unlabeledResult.numTrue,
        unlabeledResult.numPred)
    }

    def accuracy = unlabeledResult.f1

    def output(puts:(String=>Any)) = {
      // Evaluate only nonterminal tags
      if (nT != 0) {
        // Print confusion matrix: predicted states are rows, true tags are columns
        val ts = Utils.sortedIndices(Utils.toArray(nonTagIndexer.getObjects), false)
        val table =
          Array(Array("") ++ ts.map(ntstr) ++ Array("(NONE)")) ++
          map(nK, { k:Int =>
            Array(k+"("+ntstr(greedyMap(k))+")") ++ ts.map { t:Int => fmt(counts(t)(k)) } ++ Array(fmt(counts(nT)(k)))
          }) ++
          Array(Array("(NONE)") ++ ts.map { t:Int => fmt(counts(t)(nK)) } ++ Array(""))
        //dbg(unlabeledResult.numPred + " " + unlabeledResult.numTrue)
        puts("Unlabeled: "+unlabeledResult)
        puts("Labeled: "+labeledResult)
        Utils.formatTable(table, { (r:Int,c:Int) => if (c == 0) -1 else if (r == 0) 0 else 1 }).foreach(puts(_))
      }
    }
  }

  case class Example(override val words:Array[Int], preTags:Array[Int], override val trueWidget:Widget) extends WordExample[Widget](words, trueWidget) {
    def content(i:Int, j:Int) = new SubArray(preTags, i, j+1)
    def context(i:Int, j:Int) = ((if (i==0) -1 else preTags(i-1)), (if (j==words.length-1) -1 else preTags(j+1)))
    def con(i:Int, j:Int) = contentIndexer.indexOf(content(i, j))
    def cxt(i:Int, j:Int) = contextIndexer.indexOf(context(i, j))
  }

  case class InferState(override val ex:Example, override val params:Params, override val counts:Params, override val ispec:InferSpec) extends
      AHypergraphInferState[Widget,Example,Params](ex, params, counts, ispec) {
    def words = ex.words
    def N = words.length
    def newWidget = Utils.set_!(new Widget(N, N), -1)

    def createHypergraph(H:Hypergraph[Widget]) = {
      //H.debug = true
      def spanAllowed(i:Int, j:Int) = {
        if(trueInfer || opts.fixBracketing) ex.trueWidget(i)(j) != -1
        else true
      }
      def tagAllowed(i:Int, j:Int, a:Int) = {
        if(trueInfer) ex.trueWidget(i)(j) == a
        else if(opts.fixPreTags && i == j) ex.trueWidget(i)(j) == a
        else if(opts.fixBracketing) ex.trueWidget(i)(j) != -1
        else true
      }

      val dis_a = numCCMClasses-1 // Distituent class

      val headNode = "head"
      val defaultNode = "default"
      val tol = 1e-10
      H.addSumNode(headNode)
      H.addProdNode(defaultNode)
      H.addEdge(H.prodStartNode, headNode)
      H.addEdge(H.prodStartNode, defaultNode)

      // Default CCM potentials
      if (ccm_?) {
        // By default, everything is a distituent
        foreach(N, { i:Int =>
          foreach(i+1, N, { j:Int =>
            H.addEdge(defaultNode, new Info {
              val conw = get(params.contents(dis_a), ex.con(i, j))
              val cxtw = get(params.contexts(dis_a), ex.cxt(i, j))
              def getWeight = (if (conw > tol) conw else 1.0) *
                              (if (cxtw > tol) cxtw else 1.0)
              def setPosterior(v:Double) = {
                if (conw > tol) update(counts.contents(dis_a), ex.con(i, j), v)
                if (cxtw > tol) update(counts.contexts(dis_a), ex.cxt(i, j), v)
              }
              def choose(widget:Widget) = widget
            })
          })
        })
      }

      def gen(i:Int, j:Int, a:Int) : Object = { // Generate the rest of the sequence from position i with state a
        val node = (i,j,a)

        // CCM
        val con = if (ccm_?) ex.con(i, j) else unkcon
        val cxt = if (ccm_?) ex.cxt(i, j) else unkcxt
        val cons_a = if (opts.ccmDependsOnState) a else 0

        if(H.addSumNode(node)) {
          if (i == j) { // Emission
            H.addEdge(node, new Info {
              def getWeight = get(params.emissions(a), words(i))
              def setPosterior(v:Double) = update(counts.emissions(a), words(i), v)
              def choose(widget:Widget) = { widget(i)(j) = a; widget }
            })
          }
          else { // Production
            foreach(i, j, { k:Int =>
              if (spanAllowed(i, k) && spanAllowed(k+1,j)) {
                val p1 = i == k // Left child is preterminal?
                val p2 = k+1 == j // Right child is preterminal?
                val r = if (p1) (if (p2) R_PP else R_PN) else (if (p2) R_NP else R_NN)
                foreach((if (p1) pK else nK), { b:Int => if (tagAllowed(i, k, b))
                  foreach((if (p2) pK else nK), { c:Int => if (tagAllowed(k+1, j, c))
                    H.addEdge(node, gen(i, k, b), gen(k+1, j, c), new Info {
                      val conw = if (ccm_?) get(params.contents(dis_a), ex.con(i, j)) else Double.NaN
                      val cxtw = if (ccm_?) get(params.contexts(dis_a), ex.cxt(i, j)) else Double.NaN
                      def getWeight = get(params.types(a), r) *
                                      get(params.productions(a)(r), encode(r)(b, c)) *
                                      (if (ccm_?) {
                                        (get(params.contents(cons_a), con) * get(params.contexts(cons_a), cxt)) /
                                        ((if (conw > tol) conw else 1.0) * (if (cxtw > tol) cxtw else 1.0))
                                      } else 1.0)
                      def setPosterior(v:Double) = {
                        //dbg("POST %s %s %s", fmt(a), rstr(r), fmt(v))
                        update(counts.types(a), r, v)
                        update(counts.productions(a)(r), encode(r)(b, c), v)
                        if (ccm_?) {
                          update(counts.contents(cons_a), con, v)
                          update(counts.contexts(cons_a), cxt, v)
                          if (conw > tol) update(counts.contents(dis_a), con, -v)
                          if (cxtw > tol) update(counts.contexts(dis_a), cxt, -v)
                        }
                      }
                      def choose(widget:Widget) = { widget(i)(j) = a; widget }
                    })
                  })
                })
              }
            })
          }
        }
        node
      }

      assert (N >= 2) // Must have length 2 because first state is always nonterminal
      foreach(nK, { a:Int => if (tagAllowed(0, N-1, a))
        H.addEdge(headNode, gen(0, N-1, a), new Info { // Generate the state at root
          def getWeight = get(params.starts, a)
          def setPosterior(v:Double) = update(counts.starts, a, v)
          def choose(widget:Widget) = widget
        })
      })
    }
  }

  class Model extends WordModel[Widget,Params,Performance,Example,InferState] {
    def widgetToIntSeq(widget:Widget) = widget.flatMap({x => x})
    override def widgetToFullString(ex:Example, widget:Widget) = renderTree(ex.words, widget)

    override def tokensToExample(tokens:Array[String], add:(Example => Any)) = {
      opts.inputFormat match {
        case Options.InputFormat.raw =>
          // word word word ...
          if(2 <= tokens.length && tokens.length <= opts.maxExampleLength)
            add(new Example(getIndex(wordIndexer, tokens), null, null))
        case Options.InputFormat.tag =>
          if(2 <= tokens.length/2 && tokens.length/2 <= opts.maxExampleLength)
            add(new Example(getIndex(wordIndexer, Utils.slice(tokens, 0, tokens.size, 2)),
                            getIndex(preTagIndexer, Utils.slice(tokens, 1, tokens.size, 2)),
                            null))
      }
    }

    def exampleToString(ex:Example) = renderTree(ex.words, ex.trueWidget)

    override def readExamples(path:String, maxExamples:Int, add:(Example=>Any)) = {
      if (opts.inputFormat == Options.InputFormat.mrg)
        foreachTree(opts, path, maxExamples, { tree:Tree =>
          val words = tree.getYield
          if (2 <= words.size && words.size <= opts.maxExampleLength)
            add(new Example(getIndex(wordIndexer, Utils.toArray(words)),
                            getIndex(preTagIndexer, Utils.toArray(tree.getPreTerminalYield)),
                            treeToTagMatrix(words.size, tree)))
        })
      else
        super.readExamples(path, maxExamples, add)
    }

    override def preInit = {
      decode(R_PP) = { bc:Int => (bc/pK, bc%pK) }
      decode(R_PN) = { bc:Int => (bc/nK, bc%nK) }
      decode(R_NP) = { bc:Int => (bc/pK, bc%pK) }
      decode(R_NN) = { bc:Int => (bc/nK, bc%nK) }
      encode(R_PP) = { (b:Int,c:Int) => b*pK+c }
      encode(R_PN) = { (b:Int,c:Int) => b*nK+c }
      encode(R_NP) = { (b:Int,c:Int) => b*pK+c }
      encode(R_NN) = { (b:Int,c:Int) => b*nK+c }

      contentIndexer.clear
      contextIndexer.clear

      unkcon = contentIndexer.getIndex(unkContent)
      unkcxt = contextIndexer.getIndex(unkContext)
      if (ccm_?) {
        val numToUse = examples.length min opts.maxExamplesForPhrases
        track("Create content/context index of preterminal tags from %s examples", fmt(numToUse))
        foreach(numToUse, { i:Int =>
          val ex = examples(i)
          val N = ex.words.length
          foreach(N, { i:Int =>
            foreach(i+1, N, { j:Int =>
              contentIndexer.getIndex(ex.content(i, j))
              contextIndexer.getIndex(ex.context(i, j))
            })
          })
        })
        end_track
      }
    }

    override def logStats = {
      super.logStats
      putLogRec("numPreTags", pT)
      putLogRec("numPreStates", pK)
      putLogRec("numNonTags", nT)
      putLogRec("numNonStates", nK)
      putLogRec("numContents", CON)
      putLogRec("numContexts", CXT)
      putLogRec("numCCMClasses", numCCMClasses)
    }

    override def baitInitParams = {
      track("baitInitParams: upweighting PN to encourage right-branching structures and NN to encourage generalization")
      params.randomize_!(opts.initRandom, opts.initNoise)
      foreach(nK, { a:Int =>
        params.types(a).addCount_!(R_NN, 2)
        //params.types(a).addCount_!(R_PN, 1)
      })
      params.optimize_!(opts.initSmoothing)
      end_track
    }

    override def artificialInitParams = {
      // Read in a toy grammar
      preTagIndexer.clear
      nonTagIndexer.clear
      wordIndexer.clear

      // Read in the rules
      case class Rule(lhs:String, rhs:Array[String])
      val rules = Utils.createArray({ add:(Rule=>Any) =>
        Utils.foreachLine(opts.artificialDescriptionPath, { rawLine:String =>
          val line = rawLine.trim
          if (line.length > 0 && !line.startsWith("#")) {
            // Format: A -> B C or A -> w
            val tokens = line.split("\\s+") 
            assert (tokens.length >= 3 && tokens(1) == "->")
            if (tokens.length == 3) { // Emission: we know it's preterminal -> word
              preTagIndexer.getIndex(tokens(0))
              wordIndexer.getIndex(tokens(2))
            }
            else // Production: nonterminal -> nonterminal/preterminal (figure this out later)
              nonTagIndexer.getIndex(tokens(0))
            add(Rule(tokens(0), tokens.subArray(2, tokens.length)))
          }
          true
        })
      })
      if (rules.length == 0) fail("No rules read from "+ opts.artificialDescriptionPath)

      if (nT > nK) fail("More nonterminal tags (%d) than states (%d)", fmt(nT), fmt(nK))
      if (pT > pK) fail("More preterminal tags (%d) than states (%d)", fmt(pT), fmt(pK))

      params = newParams

      // Set the parameters with these rules
      def id(s:String) = {
        val a = preTagIndexer.indexOf(s)
        if (a != -1) (0, a)
        else (1, nonTagIndexer.indexOf(s))
      }
      foreach(rules, { rule:Rule =>
        rule.rhs.length match {
          case 1 => // Emission
            params.emissions(preTagIndexer.indexOf(rule.lhs)).addCount_!(wordIndexer.indexOf(rule.rhs(0)), 1)
          case 2 => // Binary production
            val a = nonTagIndexer.indexOf(rule.lhs)
            val (rb, b) = id(rule.rhs(0)); if (b == -1) fail(rule)
            val (rc, c) = id(rule.rhs(1)); if (c == -1) fail(rule)
            val r = rb*2+rc
            params.types(a).addCount_!(r, 1)
            params.productions(a)(r).addCount_!(encode(r)(b, c), 1)
        }
      })
      params.starts.addCount_!(0, 1)

      track("Created", true)
      logs("Nonterminals: " + nonTagIndexer.getObjects())
      logs("Preterminals: " + preTagIndexer.getObjects())
      logs("Words: " + wordIndexer.getObjects())
      end_track

      params.optimize_!(0)
    }

    def newInferState(ex:Example, params:Params, counts:Params, ispec:InferSpec) = new InferState(ex, params, counts, ispec)
    def newPerformance = new Performance
    def newParams = new Params(ProbVec.zeros(nK), ProbVec.zeros2(pK, W), ProbVec.zeros2(nK, R),
      ProbVec.zeros3(nK, R, { (a:Int, r:Int) => r match {
        case R_PP => pK*pK; case R_PN => pK*nK; case R_NP => nK*pK; case R_NN => nK*nK; case _ => -1 } }),
      ProbVec.zeros2(numCCMClasses, CON), ProbVec.zeros2(numCCMClasses, CXT))

    def genExample = {
      var n = 0
      val words = new Array[Int](opts.genMaxTokens)
      val preTags = new Array[Int](opts.genMaxTokens)
      val widget = Utils.set_!(new Widget(opts.genMaxTokens, opts.genMaxTokens), -1)
      var tooLong = false
      def emit(a:Int) = {
        preTags(n) = a
        words(n) = genSample(params.emissions(a))
        widget(n)(n) = a
        n += 1
      }
      def gen(a:Int) : Unit = {
        val r = genSample(params.types(a))
        val bc = genSample(params.productions(a)(r))
        val (b, c) = decode(r)(bc)
        val left = n
        if (n < opts.genMaxTokens) // Left child
          (if (r == R_PP || r == R_PN) emit(b) else gen(b))
        else tooLong = true
        if (n < opts.genMaxTokens) // Right child
          (if (r == R_PP || r == R_NP) emit(c) else gen(c))
        else tooLong = true
        val right = n-1
        widget(left)(right) = a
      }
      gen(genSample(params.starts))
      if (tooLong) genExample // Try again (WARNING: might infinite loop)
      else new Example(words.subArray(0, n), preTags.subArray(0, n), widget.subArray(0, n).map(_.subArray(0, n)))
    }
  }
  def newModel = new Model
}
