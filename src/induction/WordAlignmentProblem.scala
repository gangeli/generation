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

case class WordAlignmentProblem(opts:Options) extends AProblem {
  // FUTURE: make this more memory efficient
  type Widget = Array[Array[Double]] // (source i, target j) => weight (posterior probability)
  // Values for alignments
  val SURE = 1.0; val POSSIBLE = 0.5
  val L = 2 // Number of languages
  val nulls = 0 // index of the designated null word

  // A direction specifies the information regarding translation from a source language to a target language
  // Right now, there will only be two directions (d) indexed by the source language (l)
  case class Direction(sourceFreqs:Array[Int],
      sourceIndexer:Indexer[String], targetIndexer:Indexer[String], transIndexer:Array[Indexer[Int]]) {
    def S = sourceIndexer.size
    def sstr(s:Int) = sourceIndexer.getObject(s)
    def T(s:Int) = transIndexer(s).size
    def tstr(t:Int) : String = targetIndexer.getObject(t)
    def tstr(s:Int, ti:Int) : String = tstr(transIndexer(s).getObject(ti))
    def ti(s:Int, t:Int) = transIndexer(s).indexOf(t)
  }
  // The two directions are: source -> target, target -> source
  var dirs : Array[Direction] = null // Set later

  // Distortion (i-previ) is bucketed into <=-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, >=5
  val distortTruncation = 5
  val numDistorts = distortTruncation * 2 + 1
  def distort(di:Int) = {
    if (di < -distortTruncation) 0
    else if (di > +distortTruncation) numDistorts-1
    else di+distortTruncation
  }
  val distortNames = map(-distortTruncation, distortTruncation+1, { di:Int =>
    (if (di == -distortTruncation) "<="
    else if (di == distortTruncation) ">="
    else "") + di
  })

  // d = direction
  // distorts = distribution over distortion buckets
  // emissions[s] = distribution over target words t
  case class DirParams(d:Int, distorts:ProbVec, emissions:Array[ProbVec]) {
    def foreachVec(f:(ProbVec => Any)) = {
      f(distorts)
      emissions.foreach(f(_))
    }
    def output(puts:(String => Any)) = {
      Utils.foreachSorted(distorts.getProbs, true, { (u:Int,v:Double) =>
        puts(String.format("D%s %s\t%s", fmt(d), distortNames(u), fmt(v)))
      })
      puts("")
      Utils.foreachSorted(dirs(d).sourceFreqs, true, { (s:Int,freq:Int) =>
        Utils.foreachSorted(emissions(s).getProbs, opts.numOutputParams, true, { (ti:Int,v:Double) =>
          puts(String.format("E%s %s %s\t%s", fmt(d), dirs(d).sstr(s), dirs(d).tstr(s, ti), fmt(v)))
        })
        puts("")
      })
    }
  }

  // A set of parameters, one for each direction
  case class Params(dirs:Array[DirParams]) extends AParams {
    def foreachVec(f:(ProbVec => Any)) = dirs.foreach(_.foreachVec(f))
    def output(puts:(String => Any)) = dirs.foreach(_.output(puts))
  }

  class Performance extends APerformance[Widget] {
    var numSure = 0
    var numSureAndPred = 0
    var numPossible = 0
    var numPossibleAndPred = 0
    var numPred = 0

    def add(trueWidget:Widget, predWidget:Widget) {
      if (trueWidget != null) {
        // Compute AER
        // Note: sure is a subset of possible alignment edges
        val I = Utils.same(trueWidget.length, predWidget.length)
        val J = Utils.same(trueWidget(0).length, predWidget(0).length)
        foreach(I, { i:Int =>
          foreach(J, { j:Int =>
            val t = trueWidget(i)(j)
            val p = predWidget(i)(j) >= opts.posteriorThreshold
            if (t >= POSSIBLE) {
              if (p) numPossibleAndPred += 1
              numPossible += 1
            }
            if (t >= SURE) {
              if (p) numSureAndPred += 1
              numSure += 1
            }
            if (p) numPred += 1
          })
        })
      }
    }
    def output(puts:(String=>Any)) = 
      puts(String.format("Precision: %s (%s/%s), recall: %s (%s/%s), AER: %s",
        fmt(1.0 * numPossibleAndPred / numPred), fmt(numPossibleAndPred), fmt(numPred),
        fmt(1.0 * numSureAndPred / numSure), fmt(numSureAndPred), fmt(numSure),
        fmt(aer)))
    def aer = 1 - 1.0*(numSureAndPred + numPossibleAndPred) / (numPred + numSure)
    def accuracy = 1 - aer
  }

  // wordsSet: language (0 or 1) -> sequence of words
  case class Example(wordsSet:Array[Array[Int]], trueWidget:Widget) extends AExample[Widget] {
    def numTokens = Utils.sum(wordsSet.length, { i:Int => wordsSet(i).length }) // Total number of tokens
    def N(d:Int) = wordsSet(d).length
  }

  case class InferState(override val ex:Example, override val params:Params, override val counts:Params, override val ispec:InferSpec) extends
      AHypergraphInferState[Widget,Example,Params](ex, params, counts, ispec) {
    def newWidget = new Widget(ex.N(0), ex.N(1))

    def createHypergraph(H:Hypergraph[Widget]) = {
      //H.debug = true
      assert (!trueInfer)
      val m1_? = opts.alignmentModel == Options.AlignmentModel.m1
      val m2_? = opts.alignmentModel == Options.AlignmentModel.m2
      val hmm_? = opts.alignmentModel == Options.AlignmentModel.hmm
      assert (!m2_?)

      // Generate the model in that direction
      def genDir(d:Int) : Any = {
        val I = ex.N(d)
        val J = ex.N(1-d)

        // For each previous position previ, we have a distribution over current i's.
        // These i's are bucketed into bins; count the number in each bin
        val countDistorts = {
          if (m1_?) null
          else if (hmm_?) {
            map(-1, I, { previ:Int =>
              val counts = new Array[Int](numDistorts)
              foreach(-1, I, { i:Int =>
                val virtuali = (if (i == -1) previ else i)
                counts(distort(virtuali-previ)) += 1
              })
              counts
            })
          }
          else
            throw new RuntimeException("Unknown case")
        }

        // previ is what the previous non-null aligned source word
        def gen(j:Int, previ:Int) : Any = {
          if (j == J)
            H.endNode
          else {
            var node = if (hmm_?) (d, j, previ) else (d, j)
            if (H.addSumNode(node)) {
              foreach(-1, I, { i:Int => // For each possible source position i... (-1 is null) to align to j
                val virtuali = (if (i == -1) previ else i) // basically i unless i is null
                H.addEdge(node, gen(j+1, virtuali), new Info {
                  val null_? = (i == -1)
                  val s = (if (null_?) nulls else ex.wordsSet(d)(i)) // Source word
                  val t = ex.wordsSet(1-d)(j) // Target word
                  val ti = dirs(d).ti(s, t) // Index of target
                  //dbg(dirs(d).sstr(s) + " " + dirs(d).tstr(t))
                  //dbg("i=%s, j=%s, s=%s, t=%s, ti=%s", fmt(i), fmt(j), fmt(s), fmt(t), fmt(ti))
                  val si = (if (null_?) -1 else dirs(1-d).ti(t, s)) // Index of source (if applicable)
                  val dis = if (hmm_?) distort(virtuali-previ) else -1 // Distortion bucket
                  def getWeight =
                    (if (m1_?) 1.0/(I+1) // Uniform distortion
                     else if (hmm_?) get(params.dirs(d).distorts, dis) / countDistorts(previ+1)(dis)
                     else throw new RuntimeException("Unknown case")) *
                    get(params.dirs(d).emissions(s), ti) *
                    (if (!null_? && opts.alignAgreement) get(params.dirs(1-d).emissions(t), si) else 1.0)
                  def setPosterior(v:Double) = {
                    if (!m1_?) update(counts.dirs(d).distorts, dis, v)
                    update(counts.dirs(d).emissions(s), ti, v)
                    if (!null_? && opts.alignAgreement) update(counts.dirs(1-d).emissions(t), si, v)
                  }
                  def choose(widget:Widget) = {
                    if (!null_?) {
                      val f = if (opts.alignAgreement) 0.5 else 1.0
                      if (d == 0) widget(i)(j) += f
                      else if (d == 1) widget(j)(i) += f
                    }
                    widget
                  }
                })
              })
            }
            node
          }
        }
        gen(0, -1)
      }

      H.addEdge(H.prodStartNode, genDir(0))
      if (opts.alignAgreement)
        H.addEdge(H.prodStartNode, genDir(1))
    }
  }

  class Model extends AModel[Widget,Params,Performance,Example,InferState] {
    def widgetToIntSeq(widget:Widget) = widget.flatMap({x => x}).map(_.toInt)
    def exampleToString(ex:Example) = "TODO"

    override def logStats = {
      super.logStats
      putLogRec("numSourceWords", dirs(0).S)
      putLogRec("numTargetWords", dirs(1).S)
    }

    var processFile : (String,Int,(Example=>Any)) => Unit = null
    override def readExamples(path:String, maxExamples:Int, add:(Example=>Any)) = processFile(path, maxExamples, add)
    override def readExamples = {
      val indexers = map(L, new Indexer[String]) // Language -> indexer for the words of that language
      indexers.foreach { indexer => indexer.getIndex("(NULL)") } // Always include null word

      processFile = { (path:String, maxExamples:Int, add:(Example=>Any)) =>
        // Each line is a sentence.
        // The sentence might have an ID (the test set), which would be specified like this:
        // <s snum=1016> it would have been easy to say that these sanctions have to be followed rather than making them voluntary .  </s>
        // Return a list of (id, list of words)
        case class Sentence(id:Int, words:Array[Int])
        def readSentences(path:String, indexer:Indexer[String]) = {
          Utils.mapLine(path, maxExamples, { line:String =>
            val tokens = line.split("\\s+")
            if (tokens.length > 3 && tokens(0) == "<s" && tokens(1).startsWith("snum=") && tokens(tokens.length-1) == "</s>")
              Sentence(tokens(1).substring(5, tokens(1).length-1).toInt,
                       getIndex(indexer, tokens.subArray(2, tokens.length-1)))
            else
              Sentence(-1, getIndex(indexer, tokens))
          })
        }
        val sourcePath = path // <prefix>.en
        val targetPath = path.replaceAll("\\."+opts.inputFileExt+"$", "."+opts.inputFileExt2)
        val waPath = path.replaceAll("\\."+opts.inputFileExt+"$", ".wa")
        val source = readSentences(sourcePath, indexers(0))
        val target = readSentences(targetPath, indexers(1))
        val sourceLengths = new HashMap[Int,Int]; source.foreach { pair => sourceLengths(pair.id) = pair.words.length }
        val targetLengths = new HashMap[Int,Int]; target.foreach { pair => targetLengths(pair.id) = pair.words.length }

        val alignments = new HashMap[Int,Widget]
        if (new java.io.File(waPath).exists) {
          Utils.foreachLine(waPath, { line:String =>
            // Format: each line has 1001 28 29 <S|P>
            val tokens = line.split("\\s+")
            val id = tokens(0).toInt
            val i = tokens(1).toInt - 1
            val j = tokens(2).toInt - 1
            val weight = if (tokens.length > 2 && tokens(3) == "P") POSSIBLE else SURE

            val I = sourceLengths.getOrElse(id, -1)
            val J = targetLengths.getOrElse(id, -1)
            if (I != -1 && J != -1) {
              val alignment = alignments.getOrElseUpdate(id, new Widget(I, J))
              alignment(i)(j) = weight
            }
            true
          })
        }

        foreach(Utils.same(source.length, target.length), { i:Int =>
          val id = Utils.same(source(i).id, target(i).id)
          add(new Example(Array(source(i).words, target(i).words), alignments.getOrElse(id, null)))
        })
      }

      super.readExamples

      track("Create indices")
      dirs = map(L, { l:Int => // For each language...
        val freqs = new Array[Int](indexers(l).size) // Compute word frequencies
        examples.foreach { ex => ex.wordsSet(l).foreach(freqs(_) += 1) }
        // Compute translation indexer (allow any translation that has been seen)
        val transIndexer = map(indexers(l).size, new Indexer[Int])
        examples.foreach { ex =>
          ex.wordsSet(1-l).foreach { t:Int =>
            transIndexer(nulls).getIndex(t)
            ex.wordsSet(l).foreach { s:Int => transIndexer(s).getIndex(t) }
          }
        }
        new Direction(freqs, indexers(l), indexers(1-l), transIndexer)
      })
      end_track
    }

    def newInferState(ex:Example, params:Params, counts:Params, ispec:InferSpec) = new InferState(ex, params, counts, ispec)
    def newPerformance = new Performance
    def newParams = new Params(map(L, { l:Int =>
      new DirParams(l, ProbVec.zeros(numDistorts),
        if (dirs == null) null // Not ready
        else map(dirs(l).S, { s:Int => ProbVec.zeros(dirs(l).T(s)) })) // Emissions
    }))

    def genExample = {
      val N = opts.genMaxTokens
      // TODO
      val words = null
      new Example(words, null)
    }
  }
  def newModel = new Model
}
