package induction

import scala.collection.mutable.HashMap
import java.util.Random

import fig.basic.{IOUtils,Indexer,StatFig}
import fig.exec.Execution
import fig.exec.Execution.putLogRec
import fig.record.Record

import tea._
import tea.Utils.{map,foreach,fmt,fmt1,fmt2,fmts,returnFirst,assertValid}
import tea.Utils.{track,begin_track,end_track,logs,logss,warnings,dbg,dbgs,fail,fails,unknownCase}

import InductionUtils._

// A problem is defined by params, performance, example, inferState, model
trait AProblem {
  def opts : Options // To override

  trait ModelInterface {
    def readExamples : Unit
    def logStats : Unit
    def genExamples : Unit
    def preInit : Unit
    def init(initType:Options.InitType, initRandom:Random) : Unit
    def learn(name:String, lopts:LearnOptions) : Unit
    def generate() : Array[List[String]]
	def interactive : Unit
	def loadParams(filename:String) : Unit
	def saveParams(filename:String) : Unit
  }

  trait AParams {
    def foreachVec(f:(ProbVec => Any)) : Unit

    def setUniform_!(x:Double) = foreachVec({ v:ProbVec => v.set_!(x) })
    def randomize_!(random:Random, noise:Double) = foreachVec({ v:ProbVec => v.set_! { i:Int => Math.pow(1+random.nextDouble, noise) } })
    def addNoise_!(random:Random, noise:Double) = foreachVec({ v:ProbVec => v.set_! { i:Int => random.nextDouble*noise } })
    def optimizeIfTooBig_! = foreachVec { v:ProbVec => v.normalizeIfTooBig_! }
    def optimize_!(smoothing:Double) = foreachVec { v:ProbVec => v.addCount_!(smoothing).normalize_! }
    def optimizeVar_!(smoothing:Double) = foreachVec { v:ProbVec => v.addCount_!(smoothing).expDigamma_! }
    def saveSum_! = foreachVec { v:ProbVec => v.saveSum_! }
    def div_!(scale:Double) = foreachVec { v:ProbVec => v.div_!(scale) }

    def getVecs = Utils.createArray({ add:(ProbVec => Any) => foreachVec(add(_)) })

    def add_!(scale:Double, that:AParams) = {
      val thisVecs = this.getVecs
      val thatVecs = that.getVecs
      foreach(thisVecs, thatVecs, { (v1:ProbVec,v2:ProbVec) =>
        v1.addCount_!(v2, scale)
      })
    }

    def output(puts:(String=>Any)) : Unit
    def output(path:String) : Unit = {
	  begin_track("AParams.output(%s)", path)
      Utils.writeLines(path, output)
      end_track
    }

    // Helper for output(): display probabilities in sorted order
    def foreachProb(v:ProbVec, puts:(String => Any), str:(Int=>String)) = {
      Utils.foreachSorted(v.getProbs, opts.outputNumParamsPerVec, true, { (i:Int,prob:Double) =>
        // Display probability and posterior count
        // (note that this count is computed before optimize - which might introduce smoothing)
        puts(str(i)+"\t"+fmt(prob)+"\t"+fmt(v.getOldSum*prob))
      })
    }
  }

  case class ProbStats(
    var n:Int, // |x|: just for normalization
    // q(z|x) \propto p(x,z)^{1/temperature}
    // z^* = argmax_z q(z|x)
    var logZ:Double, // \sum_z p(x, z)^{1/temperature}
    var logVZ:Double, // p(x, z^*)
    var logCZ:Double, // q(z^* | x)
    var elogZ:Double, // \E_q \log p(x, z)
    var entropy:Double, // H(q)
    var objective:Double) { // objective = \E_q p(z | x) + T H(q)

    def add(that:ProbStats) = {
      n += that.n
      def assertValid(x:Double) = x // Don't be picky
      logZ += assertValid(that.logZ)
      logVZ += assertValid(that.logVZ)
      logCZ += assertValid(that.logCZ)
      elogZ += assertValid(that.elogZ)
      entropy += assertValid(that.entropy)
      objective += assertValid(that.objective)
    }

    def avg_logZ = logZ / n
    def avg_logVZ = logVZ / n
    def avg_logCZ = logCZ / n
    def avg_elogZ = elogZ / n
    def avg_entropy = entropy / n
    def avg_objective = objective / n
  }

  trait APerformance[Widget] {
    val stats = ProbStats(0, 0, 0, 0, 0, 0, 0)

    def add(newStats:ProbStats) = stats.add(newStats)

    def foreachStat(f:((String,String) => Any)) = {
      f("logZ", fmt(stats.avg_logZ))
      f("logVZ", fmt(stats.avg_logVZ))
      f("logCZ", fmt(stats.avg_logCZ))
      f("elogZ", fmt(stats.avg_elogZ))
      f("entropy", fmt(stats.avg_entropy))
      f("objective", fmt(stats.avg_objective))
      f("accuracy", fmt(accuracy))
    }

    def summary = Utils.createArray({ add:(String=>Any) =>
        foreachStat({ (a:String,b:String) => add(a+" = "+b) })
      }).mkString(", ")
    def record(name:String) = {
      logs("%s: %s", name, summary)
      Record.begin(name)
      foreachStat({ (a:String,b:String) =>
        Execution.putOutput(name+"."+a, b)
        Record.add(a, b)
      })
      Record.end
    }

    def empty_? = stats.n == 0

    def add(trueWidget:Widget, predWidget:Widget)
    def accuracy : Double

    def output(puts:(String => Any)) : Unit
    def output(path:String) : Unit = {
      Utils.writeLines(path, { (puts:(String => Any)) =>
        foreachStat { (a:String,b:String) => puts(a+"\t"+b) }
        output(puts)
      })
    }
  }

  trait AExample[Widget] {
    def numTokens : Int
    def trueWidget : Widget
  }

  case class InferSpec(temperature:Double, softInfer:Boolean, hardInfer:Boolean,
    hardUpdate:Boolean, trueUpdate:Boolean, mixParamsCounts:Boolean, useVarUpdates:Boolean, var updateScale:Double,
    iter:Int)

  // State needed to perform soft and/or MAP inference on the examples
  abstract case class AInferState[Widget, Example<:AExample[Widget], Params<:AParams](ex:Example, params:Params, counts:Params, ispec:InferSpec) {
    val temperature = ispec.temperature
    val softInfer = ispec.softInfer
    val hardInfer = ispec.hardInfer
    val trueInfer = ispec.trueUpdate

    def get(v:ProbVec, i:Int) = {
      if(i >= v.size) 1e-20
	  else if (ispec.useVarUpdates) v.getCount(i) // Explicitly use weights which are not normalized
      else if (temperature == 1) v.getProb(i)
      else Math.pow(v.getProb(i), 1.0/temperature)
    }
    def update(v:ProbVec, i:Int, x:Double) = {
      //dbg("update " + fmt1(v.getProbs) + " " + i + " " + fmt(x))
      if (ispec.mixParamsCounts)
        v.addProb_!(i, x*ispec.updateScale)
      else
        v.addCount_!(i, x*ispec.updateScale)
    }
    def updateKeepNonNegative(v:ProbVec, i:Int, x:Double) = { // Use this only as a safety net
      if (ispec.mixParamsCounts)
        v.addProbKeepNonNegative_!(i, x*ispec.updateScale)
      else
        v.addCountKeepNonNegative_!(i, x*ispec.updateScale)
    }
    def update(v:ProbVec, phi:Array[Double], x:Double) = {
      if (ispec.mixParamsCounts)
        v.addProb_!(phi, x*ispec.updateScale)
      else
        v.addCount_!(phi, x*ispec.updateScale)
    }

    // When an infer state is created, the following quantities should be available
    def logZ : Double
    def logVZ : Double
    def logCZ = if (temperature == 1) logVZ - logZ else Double.NaN // q(z^*|x) = p(z^*, x)/p(x) only when temperature = 1
    def elogZ : Double
    def entropy : Double
    def objective = elogZ + temperature*entropy
    def stats = ProbStats(ex.numTokens, logZ, logVZ, logCZ, elogZ, entropy, objective)
    def bestWidget : Widget
    def complexity : Int

    // Update the parameters based on soft inference
    def updateCounts : Unit
  }

  abstract case class AHypergraphInferState[Widget,Example<:AExample[Widget],Params<:AParams](override val ex:Example, override val params:Params, override val counts:Params, override val ispec:InferSpec) extends
      AInferState[Widget,Example,Params](ex, params, counts, ispec) {
    type Info = Hypergraph.HyperedgeInfo[Widget]
    type LogInfo = Hypergraph.LogHyperedgeInfo[Widget]

    // Soft inference
    val hypergraph = new Hypergraph[Widget]
    stopWatch("createHypergraph") {
      createHypergraph(hypergraph)
    }
    stopWatch("computePosteriors") {
      hypergraph.computePosteriors(ispec.hardUpdate)
    }
    if (opts.computeELogZEntropy)
      hypergraph.computeELogZEntropy(ispec.hardUpdate)
    val logZ = hypergraph.getLogZ
    val elogZ = hypergraph.getELogZ * temperature
    val entropy = hypergraph.getEntropy

    // Hard inference
    val (bestWidget, logVZ) = {
      if (hardInfer) {
        val result = hypergraph.fetchBestHyperpath(newWidget)
        (result.widget, result.logWeight)
      }
      else (newWidget, Double.NaN)
    }

    def updateCounts = {
      counts.synchronized {
        if(ispec.mixParamsCounts) counts.saveSum_!
        stopWatch("fetchPosteriors") {
          hypergraph.fetchPosteriors(ispec.hardUpdate)
        }
      }
    }

    // Main functions to override: specifies the entire model
    def createHypergraph(hypergraph:Hypergraph[Widget]) : Unit
    def newWidget : Widget
    def complexity = hypergraph.numNodes
  }

  trait AModel[Widget,
      Params <: AParams,
      Performance <: APerformance[Widget],
      Example <: AExample[Widget],
      InferState <: AInferState[Widget, Example, Params]] extends ModelInterface {
    var paramsOpt : Option[Params] = None
    def params : Params = paramsOpt match { case Some(x) => x; case None => throw fails("No params") }
    def params_=(x:Params) = { paramsOpt = Some(x) }
	def newInferSpec(temperature:Double, softInfer:Boolean, hardInfer:Boolean,
    	hardUpdate:Boolean, trueUpdate:Boolean, mixParamsCounts:Boolean, useVarUpdates:Boolean, updateScale:Double,
    	iter:Int) =
			InferSpec(temperature, softInfer, hardInfer, hardUpdate, trueUpdate, mixParamsCounts, useVarUpdates, updateScale, iter)

    var examples : Array[Example] = null

    // To override
    def newParams : Params
    def newPerformance : Performance
    def tokensToExample(tokens:Array[String], add:(Example=>Any)) : Unit = throw fails("Not supported")
    def newInferState(ex:Example, params:Params, counts:Params, ispec:InferSpec) : InferState
    def logStats = putLogRec("numExamples", examples.size)
    def genExample : Example
    def genSample(v:ProbVec) = v.sample(opts.genRandom)
    def genExamples = {
      params.output(Execution.getFile("gen.params"))
      begin_track("Generating %s examples", fmt(opts.genNumExamples))
      examples = map(opts.genNumExamples, genExample)
      Utils.writeLines(Execution.getFile("gen.examples"), { (puts:String=>Any) =>
        examples.foreach { ex => puts(exampleToString(ex)) }
      })
      end_track
    }
    def widgetToIntSeq(widget:Widget) : Array[Int]
    def widgetToFullString(ex:Example, widget:Widget) = exampleToString(ex) + " " + widgetToIntSeq(widget).mkString(" ")
    def exampleToString(ex:Example) : String

    def supervisedInitParams = {
      begin_track("supervisedInitParams")
      val counts = newParams
      params.setUniform_!(1)
      Utils.parallel_foreach(opts.numThreads, examples, { (i:Int,ex:Example,log:Boolean) =>
        if(log) begin_track("Example %s/%s", fmt(i), fmt(examples.length))
        newInferState(ex, params, counts, InferSpec(1, false, false, false, true, false, false, 1, 0)).updateCounts
        if(log) end_track
      })
      params = counts
      params.optimize_!(opts.initSmoothing)
      end_track
    }

    def uniformzInitParams = {
      // Initialize with an E-step which puts a uniform distribution over z
      // This works for models with natural asymmetries such as word alignment and DMV,
      // but not for cluster-based models such as GMMs, PMMMs, HMMs,
      // where random initialization is preferred (need noise)
      begin_track("uniformzInitParams")
      val counts = newParams
      params.setUniform_!(1)
      Utils.parallel_foreach(opts.numThreads, examples, { (i:Int,ex:Example,log:Boolean) =>
        if(log) begin_track("Example %s/%s", fmt(i), fmt(examples.length))
        // 02/15/09: just realized this; but we've been using uniformzInitParams for NAACL 2009 - need to check what happened there
        // Hack: set useVarUpdates = true so that get() uses getCount rather than getProb
        // Otherwise, this does the same thing as uniform on the parameters, which introduces strange biases
        newInferState(ex, params, counts, InferSpec(1, true, false, false, false, false, true, 1, 0)).updateCounts
        //newInferState(ex, params, counts, InferSpec(1, true, false, false, false, false, false, 1, 0)).updateCounts
        if(log) end_track
      })
      params = counts
      params.addNoise_!(opts.initRandom, opts.initNoise)
      //params.div_!(examples.length) // This is more principled but does worse
      params.optimize_!(opts.initSmoothing)
      end_track
    }

    def artificialInitParams : Unit = throw fails("Not supported; please override")

    // Default thing to do with a file: assume each example is a line
    // Override if want different behavior (e.g., WSJ)
    def readExamples(path:String, maxExamples:Int, add:(Example=>Any)) : Unit = {
      Utils.foreachLine(path, maxExamples, { line:String =>
        val tokens = line.split("\\s+")
        tokensToExample(tokens, add)
        true
      })
    }

    def readExamples : Unit = track("Reading examples") {
      examples = Utils.createArray({ add:(Example => Any) =>
        var maxExamples = opts.maxExamples
        var numExamples = 0 // Number of examples we've read in so far
        def needMoreExamples_? = numExamples < maxExamples
        
        // Recursively add all files in the directory
        def validName(path:String) = Utils.empty_?(opts.inputFileExt) || path.endsWith(opts.inputFileExt)
        def addPath(path:String) : Unit = {
          import java.io.File
          if (new File(path).isDirectory) {
            Utils.sortWithEmbeddedInt_!(new File(path).list).foreach { f:String =>
              if (needMoreExamples_?) addPath(path+"/"+f)
            }
          }
          else if (needMoreExamples_? && validName(path)) {
            track("%s (%s examples so far)", path, numExamples) {
              readExamples(path, maxExamples-numExamples, { ex:Example => add(ex); numExamples += 1 })
            }
          }
        }

        def read(inputPaths:Array[String], inputLists:Array[String]) = {
          inputPaths.foreach(addPath(_))
          inputLists.foreach { path =>
            def pathName(f:String) = {
              if (f(0) == '/') f // Absolute path
              else new java.io.File(path).getParent()+"/"+f // Relative path
            }
            Utils.readLines(path).foreach { f:String => if (needMoreExamples_?) addPath(pathName(f)) }
            //val perm = fig.prob.SampleUtils.samplePermutation(new Random(12345), files.length)
            //foreach(perm, { i:Int => if (needMoreExamples_?) addPath(pathName(files(i))) })
          }
        }

        // If we've specified testing data, we should mark that as testing data
        val setTrainTest_? = opts.testInputPaths.size > 0 || opts.testInputLists.size > 0

        if (setTrainTest_?) opts.trainStart = numExamples
        read(Utils.toArray(opts.inputPaths), Utils.toArray(opts.inputLists))
        if (setTrainTest_?) opts.trainEnd = numExamples

        // Allow for the test examples
        maxExamples = Utils.safeAdd(maxExamples, opts.testMaxExamples)

        if (setTrainTest_?) opts.testStart = numExamples
        read(Utils.toArray(opts.testInputPaths), Utils.toArray(opts.testInputLists))
        if (setTrainTest_?) opts.testEnd = numExamples

        if (setTrainTest_?)
          logss("readExamples: train: %s...%s; test: %s...%s", opts.trainStart, opts.trainEnd, opts.testStart, opts.testEnd)
      })
    }

    def preInit = { }

    def init(initType:Options.InitType, initRandom:Random) = {
      begin_track("Init parameters: %s", initType)
      params = newParams
      initType match {
        case Options.InitType.random =>
          params.randomize_!(initRandom, opts.initNoise)
          params.optimize_!(opts.initSmoothing)
        case Options.InitType.bait =>
          baitInitParams
        case Options.InitType.supervised =>
          supervisedInitParams
        case Options.InitType.uniformz =>
          uniformzInitParams
        case Options.InitType.artificial =>
          artificialInitParams
        case _ => throw fails("Invalid init type")
      }
	  params.output(Execution.getFile("init.params"))
      end_track
    }

    def baitInitParams : Unit = throw fails("Not supported")

    def learn(name:String, lopts:LearnOptions) = {
      opts.alignmentModel = lopts.alignmentModel // HACK

      var numUpdates = 0 // For online learning

      // For a convex combination update:
      // We do [(1-alpha)*old + alpha*new], where alpha = 1/T^{stepSizeReductionPower}
      // However, all our updates still look like [old + beta*new]
      // beta = alpha * suffStatsScale
      var suffStatsScale = 1.0 // Usually a large number

      // For incremental EM
      type SuffStats = InferState
      val suffStats = map[Option[SuffStats]](examples.length, None)
      //val suffStatsOnDisk = new SerializedList[Option[Double]](1000, SerializedList.getTempPath("iEM"))

      Record.begin(name)
      begin_track("Train: "+name)
      var iter = 0
      while (iter < lopts.numIters) {
        val complexity = new fig.basic.FullStatFig // Complexity inference

        // Gradually reduce temperature
        val temperature =  {
          if (lopts.numIters == 1)
            lopts.initTemperature
          else
            lopts.initTemperature + (lopts.finalTemperature-lopts.initTemperature) * iter / (lopts.numIters - 1)
        }

        begin_track("Iteration %s/%s: temperature = %s", fmt(iter), fmt(lopts.numIters), fmt(temperature))
        Record.begin("iteration", iter)
        Execution.putOutput("currIter", iter)

        val trainPerformance = newPerformance
        val testPerformance = newPerformance

        def isTrain(i:Int) = opts.trainStart <= i && i < opts.trainEnd
        def isTest(i:Int) = opts.testStart <= i && i < opts.testEnd

        val existsTrain_? = Utils.exists(examples.length, { i:Int => isTrain(i) })
        val existsTest_? = Utils.exists(examples.length, { i:Int => isTest(i) })
        val output_? = opts.outputIterFreq != 0 && iter % opts.outputIterFreq == 0
        val fullOutput_? = output_? && opts.outputFullPred
        val trainPredOut = if (output_? && existsTrain_?) Some(IOUtils.openOut(Execution.getFile(name+".train.pred."+iter))) else None
        val testPredOut = if (output_? && existsTest_?) Some(IOUtils.openOut(Execution.getFile(name+".test.pred."+iter))) else None
        val trainFullPredOut = if (fullOutput_? && existsTrain_?) Some(IOUtils.openOut(Execution.getFile(name+".train.full-pred."+iter))) else None
        val testFullPredOut = if (fullOutput_? && existsTest_?) Some(IOUtils.openOut(Execution.getFile(name+".test.full-pred."+iter))) else None

        def record(ext:String, params:Params) = { // ext specifies the iteration or example number
          // Use the given params (which are actually counts so we can evaluate even in batch EM)
          logs("Inference complexity: %s", complexity)
          if (!trainPerformance.empty_?) {
            trainPerformance.record("train")
            if (output_?)
              trainPerformance.output(Execution.getFile(name+".train.performance."+ext))
          }
          if (!testPerformance.empty_?) {
            testPerformance.record("test")
            if (output_?)
              testPerformance.output(Execution.getFile(name+".test.performance."+ext))
          }
          if (output_?)
            params.output(Execution.getFile(name+".params."+ext))

          if (opts.outputCurrentState) { // Only makes sense for online learning
            /*track("Output training objective on current parameters") // This is slow
            val performance = newPerformance 
            foreach(examples, { (i:Int,ex:Example) =>
              if (isTrain(i)) {
                val inferState = newInferState(ex, params, params,
                  InferSpec(temperature, true, false, false, false, false, 0))
                performance.add(inferState.stats)
              }
            })
            performance.output(Execution.getFile(name+".current.train.performance."+ext))
            end_track*/
            begin_track("Output test predictions on current parameters")
            val performance = newPerformance
            Utils.writeLines(Execution.getFile(name+".current.test.pred."+ext), { puts:(String=>Any) =>
              foreach(examples, { (i:Int,ex:Example) =>
                if (isTest(i)) {
                  val inferState = newInferState(ex, params, params,
                    InferSpec(temperature, false, true, false, false, false, false, 0, -1))
                  puts(widgetToIntSeq(inferState.bestWidget).mkString(" "))
                  performance.add(ex.trueWidget, inferState.bestWidget)
                }
              })
            })
            performance.record("current.test")
            performance.output(Execution.getFile(name+".current.test.performance."+ext))
              
            end_track
          }
        }

        def processExample(i:Int, ex:Example, stepSize:Double, counts:Params) = {
          val inferState = createInferState(ex, stepSize, counts)
          processInferState(inferState, i, ex)
        }

        def createInferState(ex:Example, stepSize:Double, counts:Params) = {
          val inferState = newInferState(ex, params, counts,
            InferSpec(temperature, !lopts.hardUpdate, true, lopts.hardUpdate, false,
              lopts.mixParamsCounts, lopts.useVarUpdates, stepSize, iter))
          complexity.synchronized { complexity.add(inferState.complexity) }
          inferState
        }

        def processInferState(inferState:InferState, i:Int, ex:Example) = {
          if (isTrain(i)) {
            inferState.updateCounts
            trainPerformance.synchronized {
              trainPerformance.add(inferState.stats)
              trainPerformance.add(ex.trueWidget, inferState.bestWidget)
              trainPredOut match {
                case Some(out) => out.println(widgetToIntSeq(inferState.bestWidget).mkString(" "))
                case _ =>
              }
              trainFullPredOut match {
                case Some(out) => out.println(widgetToFullString(ex, inferState.bestWidget))
                case _ =>
              }
            }
          }
          if (isTest(i)) {
            testPerformance.synchronized {
              testPerformance.add(inferState.stats)
              testPerformance.add(ex.trueWidget, inferState.bestWidget)
              testPredOut match {
                case Some(out) => out.println(widgetToIntSeq(inferState.bestWidget).mkString(" "))
                case _ =>
              }
              testFullPredOut match {
                case Some(out) => out.println(widgetToFullString(ex, inferState.bestWidget))
                case _ =>
              }
            }
          }
        }

        def summary(i:Int) = {
          if (isTrain(i)) "train: "+trainPerformance.summary
          else if (isTest(i)) "test: "+testPerformance.summary
          else "(skip)"
        }

        // Permutation of the examples
        val perm = {
          if (opts.onlinePerm)
            fig.prob.SampleUtils.samplePermutation(opts.onlinePermRandom, examples.length)
          else
            map(examples.length, {i:Int => i})
        }

        if (lopts.miniBatches) { // Stepwise EM with minibatches
          assert (lopts.online)
          assert (!lopts.mixParamsCounts) // I don't think this is handled properly in this case
          begin_track("Mini-batch online: stepSizeReductionPower = %s", fmt(lopts.stepSizeReductionPower))

          // Segment the examples into minibatches of the appropriate size
          val numBatches = (examples.length+lopts.miniBatchSize-1)/lopts.miniBatchSize // Round up
          var currExample = 0
          foreach(numBatches, { b:Int => // For each batch...
            // Prevent overflow
            if (suffStatsScale > 1e100) {
              params.div_!(suffStatsScale)
              suffStatsScale = 1
            }
            val stepSize = { // Practical step size
              val alpha = 1.0/Math.pow(numUpdates+lopts.stepSizeOffset, lopts.stepSizeReductionPower) // Logical step size
              if (lopts.convexCombUpdate) {
                assert (alpha < 1)
                suffStatsScale /= (1-alpha)
              }
              alpha * suffStatsScale
            }

            //val batch = map(lopts.miniBatchSize, opts.onlinePermRandom.nextInt(examples.length)) // Select randomly
            val batch = map(currExample, (currExample+lopts.miniBatchSize) min examples.length, { pi:Int => perm(pi) })
            begin_track("Batch %s/%s: stepSize = %s; %s", fmt(b), fmt(numBatches), fmt(stepSize), summary(batch(0)))

            // Two ways: if batch sizes are small, store infer states; otherwise store counts
            if (lopts.miniBatchSize < opts.batchSizeNewCounts) { // (1) store infer states
              begin_track("Inference")
              val inferStates = Utils.parallel_map(opts.numThreads, batch, { (pi:Int,i:Int,log:Boolean) => val ex = examples(i)
                if (log) begin_track("Example %s/%s: %s", fmt(i), fmt(examples.length), summary(i))
                if (log) Execution.putOutput("currExample", currExample)
                this.synchronized { currExample += 1 }
                val inferState = createInferState(ex, stepSize, params)
                if (log) end_track
                inferState
              })
              end_track

              begin_track("Update")
              // WARNING: for mixParamsCounts which depends on oldSum, this is not an atomic update
              foreach(batch, { (pi:Int,i:Int) => val ex = examples(i); val inferState = inferStates(pi)
                processInferState(inferState, i, ex)
              })
              end_track
            }
            else { // (2) store counts
              val counts = newParams

              begin_track("Inference and update")
              Utils.parallel_foreach(opts.numThreads, batch, { (pi:Int,i:Int,log:Boolean) => val ex = examples(i)
                if (log) begin_track("Example %s/%s: %s", fmt(pi), fmt(batch.length), summary(i))
                if (log) Execution.putOutput("currExample", currExample)
                this.synchronized { currExample += 1 }
                processExample(i, ex, 1, counts)
                if (log) end_track
              })
              end_track

              params.add_!(stepSize, counts)
            }

            numUpdates += 1 // Number of mini-batches
            end_track
          })
          end_track
        }
        else if (lopts.incremental) { // Incremental EM
          assert (lopts.online)
          begin_track("Incremental")

          foreach(perm, { (pi:Int,i:Int) => val ex = examples(i)
            begin_track("Example %s/%s: %s", fmt(i), fmt(examples.length), summary(i))
            Execution.putOutput("currExample", pi)
            // Add new
            val newInferState = createInferState(ex, 1, params)
            newInferState.updateCounts
            processInferState(newInferState, i, ex)
            // Subtract old
            suffStats(i) match {
              case Some(oldInferState) => 
                oldInferState.ispec.updateScale = -1
                oldInferState.updateCounts
              case None =>
            }
            // Replace old with new
            suffStats(i) = Some(newInferState)

            if (isTrain(i) && (opts.outputExampleFreq != 0 && i % opts.outputExampleFreq == 0)) {
              Record.begin("example", i)
              record(iter+"-"+i, params)
              Record.end
            }
            end_track
          })
          end_track
        }
        else if (lopts.online) { // Stepwise EM
          begin_track("Online: stepSizeReductionPower = %s", fmt(lopts.stepSizeReductionPower))

          foreach(perm, { (pi:Int,i:Int) => val ex = examples(i)
            // Prevent overflow
            if (suffStatsScale > 1e100) {
              params.div_!(suffStatsScale)
              suffStatsScale = 1
            }

            val stepSize = { // Practical step size
              val alpha = 1.0/Math.pow(numUpdates+lopts.stepSizeOffset, lopts.stepSizeReductionPower) // Logical step size
              if (lopts.convexCombUpdate) {
                assert (alpha < 1)
                suffStatsScale /= (1-alpha)
              }
              alpha * suffStatsScale
            }

            begin_track("Example %s/%s: stepSize = %s; %s", fmt(i), fmt(examples.length), fmt(stepSize), summary(i))
            Execution.putOutput("currExample", pi)
            processExample(i, ex, stepSize, params)
            numUpdates += 1
            if (lopts.mixParamsCounts) params.optimizeIfTooBig_! // Keep parameters small: doesn't change results
            if (isTrain(i) && (opts.outputExampleFreq != 0 && i % opts.outputExampleFreq == 0)) {
              Record.begin("example", i)
              record(iter+"-"+i, params)
              Record.end
            }
            end_track
          })
          end_track
        }
        else { // Batch
          val counts = newParams

          // E-step
          begin_track("E-step")
          Utils.parallel_foreach(opts.numThreads, examples, { (i:Int,ex:Example,log:Boolean) =>
            if (log) begin_track("Example %s/%s: %s", fmt(i), fmt(examples.length), summary(i))
            if (log) Execution.putOutput("currExample", i)
            processExample(i, ex, 1, counts)
            if (log) end_track
          })
          end_track

          // M-step
          params = counts
          params.saveSum_! // 02/07/09: for printing out posterior mass (see AParams.foreachProb)
          if (lopts.useVarUpdates)
            params.optimizeVar_!(lopts.smoothing)
          else
            params.optimize_!(lopts.smoothing)
        }

        record(""+iter, params)
        trainPredOut match { case Some(out) => out.close; case _ => }
        testPredOut match { case Some(out) => out.close; case _ => }
        trainFullPredOut match { case Some(out) => out.close; case _ => }
        testFullPredOut match { case Some(out) => out.close; case _ => }

        Execution.putOutput("currExample", examples.length)

        end_track
        Record.end

        // Final
        if (iter == lopts.numIters-1) {
          Utils.track_printAll("Final") {
            trainPerformance.record("train")
            testPerformance.record("test")
          }
        }
        iter += 1
        if (Execution.shouldBail) lopts.numIters = iter
      }

      end_track
      Record.end
      Execution.putOutput("currIter", lopts.numIters)
    }
    
    //Should override
    def generate() : Array[List[String]] = {
      val rtn = new Array[List[String]](1)
      rtn(0) = List[String]()
      rtn
    }

	def interactive : Unit = {
		println("No interactive semantics model implemented")
	}

	def loadParams(filename:String) : Unit = {
	  fails("Not yet implemented")
	}

	def saveParams(filename:String) : Unit = {
	  fails("Not yet implemented")
	}
    
  }
  

  def newModel : ModelInterface
  
  
}
