package generation

import fig.basic.LogInfo.{logs,dbg,fail,error,track,end_track}
import fig.record.Record
import fig.basic.Indexer

import tea.Utils.{map,foreach,fails}
import cortex._

import java.io._
import scala.collection.mutable.HashMap

import org.goobs.foreign.Counter
import org.goobs.classify.FeatureExtractor
import org.goobs.distributions._
import org.goobs.choices._
import org.goobs.classify._
import org.goobs.utils.MetaClass
import org.goobs.testing.MTScorer
import AlignedDataset.AlignedSentence

import induction.Event3Problem
import induction.ProbVec

case class Generator(lm:LanguageModel, opts:induction.Options) {

	var alignments:AlignedDataset = null;
	var useAlignment:Array[Boolean] = null;
	var problem:Event3Problem = null;
	var golds:AlignedDataset = null

	var goldValid = true

	def init = {
		//(load alignments)
		def loadAlignments():Unit = {
			track("Loading Alignments")
			val path:String = opts.alignSavePath
			val objStream:ObjectInputStream = new ObjectInputStream(new FileInputStream(path))
			alignments = objStream.readObject().asInstanceOf[AlignedDataset]
			objStream.close()
			logs("Alignments read: " + alignments.aligns.length)
			for( i <- 0 until alignments.aligns.length ){
				val as:AlignedSentence = alignments.aligns(i)
				val ws:WorldState = alignments.worldStates(i)
				if(alignments.trueEvents(i) == null || alignments.trueEvents(i).length == 0){
					//uh oh... we have not true events and should
					if(opts.forceTrueEvents && opts.heuristicAlign){
						//no worries, we can recover
						val rtn = new Array[Int](ws.events.length)
						for(j <- 0 until rtn.length){
							rtn(j) = j
						}
						alignments.trueEvents(i) = rtn
					} else {
						//worries! we can't recover
						throw new IllegalStateException("True events not given for training example! (exNum" + i + ")")
					}
				}
			}
			end_track
		}
		//(load parameters)
		def loadParams(numExamples:Int):Unit = {
			track("Loading Parameters")
			problem = new Event3Problem(opts)
			val model = problem.newModel	
			//(read in the relevant data)
			//opts.maxExamples = java.lang.Integer.MAX_VALUE	//ensure correct T, F, etc values
			opts.maxExamples = numExamples
			model.readExamples
			//(load parameters from file)
			if(opts.paramSavePath == null) fail("No path given to load parameters from")
			model.loadParams(opts.paramSavePath)
			//(tunnel direct generation)
			val params = model.params
			GenUtils.emissions = (t:Int,f:Int,v:Int) => params.eventTypeParams(t).fieldParams(f).generationDist(v)
			end_track
		}
		//(load gold test data)
		def loadGolds():Unit = {
			track("Loading Golds")
			//--Overhead
			//(create temporary problem)
			val tmpProb = new Event3Problem(opts)	//seperate problem instance
			val model = tmpProb.newModel
			//(example to world state)
			def exampleToWorldState(ex:tmpProb.Example):WorldState = {
				//--Get WorldState
				var e = 0;
				val events = new Array[Event](ex.events.size)
				ex.events.foreach( (ev) => {
					val fields = new Array[Field](ev.F)
					foreach( ev.F, (f:Int) => {
						fields(f) = new Field(f, ev.values(f), model.fieldType(ex.events(e).fields(f)) )
					})
					events(e) = new Event(ev.t, fields)
					e+=1
				})
				val worldState:WorldState = new WorldState( events )
				WorldState.digest(worldState)
				worldState
			}
			//(read data)
			opts.maxExamples = java.lang.Integer.MAX_VALUE	//note: the xstr options may be different because of this
			model.readExamples
			golds = new AlignedDataset(opts.testEnd - opts.testStart)
			//--Extract Information
			if(opts.testEnd > model.examples.length) opts.testEnd = model.examples.length
			if(opts.trainEnd > opts.testStart) throw new IllegalArgumentException("training ends after test starts")
			foreach(opts.testStart, opts.testEnd, (exNum:Int) => {
				val i = exNum - opts.testStart
				val ex = model.examples(exNum)
				val ws:WorldState = exampleToWorldState(ex)
				alignments.worldStates.foreach( (worldState:WorldState) => {
					if(ws eq worldState) throw new IllegalArgumentException("Duplicate world state")
				})
				//(true sentence)
				val words:Array[String] = ex.text.map( (w:Int) => tmpProb.wstr(w)) //don't care about word indexer
				//(true widget)
				val trueEvents:Array[Int]
					= if(ex.trueWidget == null) {
						if(opts.forceTrueEvents){
							if(opts.heuristicAlign){ //hacks for sumtime
								val rtn = new Array[Int](ws.events.length)
								for(i <- 0 until rtn.length){
									rtn(i) = i
								}
								rtn
							}else{
								throw new IllegalStateException("Need true events for training example (exNum=" + exNum + ")")
							}
						} else{
							Array(GenUtils.fail_e) //throw new java.lang.IllegalStateException("No gold for exNum=" + exNum)
						}
					}
					else if(ex.trueWidget.events.size == 0) Array(GenUtils.fail_e)
					else ex.trueWidget.events(0)
				//(save)
				golds.worldStates(i) = ws
				golds.trueEvents(i) = trueEvents
				golds.aligns(i) = new AlignedDataset.AlignedSentence(null, null, null, null, null)
				golds.aligns(i).strings = words
			})
			//--Consistency check
			foreach(GenUtils.T, (t:Int) => {
				if(!GenUtils.tstr(t).equals(tmpProb.tstr(t))) {
					throw new java.lang.IllegalStateException("Event indexers are out of sync between training and test")
				}
				//note: don't care about other indexers. We don't see that part anyways
			})
			//--Save in GenUtils
			GenUtils.testProblem = tmpProb
			end_track
		}
		//shuffle an aligned dataset
		def deterministicShuffle(ads:AlignedDataset, seed:Int) : Unit = {
			//TODO minor: there's probably a better way to shuffle
			//(general swap method)
			def swap[A](array:Array[A], from:Int, to:Int):Unit = {
				val tmp:A = array(from)
				array(from) = array(to)
				array(to) = array(from)
			}
			//(move each element somewhere else, prolly somewhat random)
			val rand:scala.util.Random = new scala.util.Random(seed)
			foreach(ads.aligns.size, (fromI:Int) => {
				val toI = rand.nextInt(ads.aligns.size)
				swap(ads.aligns, fromI, toI)
				swap(ads.worldStates, fromI, toI)
				swap(ads.trueEvents, fromI, toI)
				swap(ads.trueWords, fromI, toI)	
			})
		}
		//(actually load, and fill GenUtils)
		loadAlignments
		loadParams(alignments.aligns.size)
		GenUtils.fillGenUtils(problem)
		Template.opts = opts
		loadGolds
		if(opts.shuffleAlignments){
			deterministicShuffle(alignments, opts.shuffleSeed)
			deterministicShuffle(golds, opts.shuffleSeed)	//same shuffle as alignments
		}
		useAlignment = new Array[Boolean](alignments.aligns.length)
		foreach(alignments.aligns.length, (i:Int) => useAlignment(i) = true)
		if(opts.heuristicAlign && opts.forceTrueEvents){
			var bad:Int = 0
			foreach(alignments.aligns.length, (i:Int) => {
				val as = Template.heuristicAlign(	alignments.aligns(i).words, 
													GenUtils.arrayToString(alignments.aligns(i).getWords).toLowerCase,
													alignments.worldStates(i)
												)
				if(as != null){
					alignments.aligns(i) = as
				} else {
					bad += 1
					useAlignment(i) = false
				}
			})
			logs("Bad Alignments: " + bad)
		}
		//hack for WorldState logging
		alignments.worldStates.foreach( (ws:WorldState) => WorldState.digest(ws))
		//(print info)
		logs("Training start: " + opts.trainStart)
		logs("Training end: " + opts.trainEnd)
		logs("Test start: " + opts.testStart)
		logs("Test end: " + opts.testEnd)
	}

	def trainLM = {
		//--Train Language Model
		track("Training LM", true)
		logs("Reading data")
		//(train language model)
		//>>from some defined path
		//lm.readData
		//<<from training data
		var sents:List[Array[String]] = List[Array[String]]()
		foreach(opts.trainStart, opts.trainEnd, (i:Int) => {
			sents = alignments.aligns(i).getWords :: sents 
		})
		lm.readData(sents.toArray)
		logs("Training")
		lm.train
		logs("LM Memory Usage: " + lm.estimateMemoryUsage)
		//(get lm stats)
		logs("Calculating LM Stats")
		GenUtils.lmStats = lm.getLogProbStats
		logs("  min_prob=" + GenUtils.lmStats.getMin)
		logs("  mean_prob=" + GenUtils.lmStats.getMean)
		logs("  max_prob=" + GenUtils.lmStats.getMax)
		logs("  stdev=" + GenUtils.lmStats.getStandardDeviation)
		end_track
		if(opts.dropToLM){
			lm.interactive
			System.exit(0)
		}
	}
	
	/*
	*				-----------------
	*					TRAINING
	*				-----------------
	*/

	var eventChoice  	: EventChoice = null;
	var fieldChoices 	: Array[FieldChoice] = null; //array over T
	var wordChoice   	: WordChoice  = null;
	var fieldSetChoices	: Array[FieldSetChoice] = null	//array over T+1
	var templateChoices	: Array[HashMap[FieldSet,TemplateChoice]] = null	//array over T+1

	var simpleGenerator 	: ChoiceGenerator[(EventHistory,WorldState),Int] = null;
	var templateGenerator	: ChoiceGenerator[(EventHistory,WorldState),(Int,Array[Int],Array[String])] = null;

	var templates	: HashMap[Int,Array[Template]] = null;
	
	private def isFail(i:Int):Boolean = isFail(i,alignments)
	private def isFail(i:Int, alignments:AlignedDataset):Boolean = { //whether this event is the fail event
		val events = GenUtils.toMarkov(alignments.trueEvents(i))
		val ws = alignments.worldStates(i)
		if(events.length == 1 && 
			GenUtils.e2t(events(0),ws) == GenUtils.dead_t){
			true
		} else {
			false
		}
	}
	
	def trainRangeChecker = {
		foreach(opts.trainStart, opts.trainEnd, (i:Int) => {
			val ws:WorldState = alignments.worldStates(i)
			RangeChecker.digestWorldState(ws)
		})
	}

	def getFactory(model:induction.Options.GenerationModel, sigma:Double) = {
		if(model == induction.Options.GenerationModel.maxent){
			 new MaximumEntropyClassifier.Factory(sigma, opts.maxentIterations, null)
		} else if(model == induction.Options.GenerationModel.perceptron){
			new PerceptronClassifier.Factory(opts.perceptronIterations, null)
		} else {
			throw new IllegalArgumentException("Invalid classifier type")
		}
	}

	def trainEventChoice = {
		//create
		val eventFact:ProbabilisticClassifierFactory[EventHistory,Feature,Int]
			= getFactory(opts.eventModel, opts.sigmaEvent).asInstanceOf[ProbabilisticClassifierFactory[EventHistory,Feature,Int]]
		val isOK:Int=>Boolean = templates.contains(_); // are there templates of this type?
		eventChoice = MetaClass.create(opts.eventChoice).createInstance(lm, eventFact, isOK, opts)
		if(!opts.useTemplates){	
			foreach(opts.trainStart, opts.trainEnd, (i:Int) => {
			if(!isFail(i) && useAlignment(i)){
					val sent:AlignedDataset.AlignedSentence = alignments.aligns(i)
					val ws:WorldState = alignments.worldStates(i)
					eventChoice.addSentence( (sent,ws) )
				}
			})
			//train
			eventChoice.train;
			//dump
			eventChoice.dumpParams("event")
		}
	}
	
	def trainFieldChoices = {
		//create
		fieldChoices = new Array[FieldChoice](GenUtils.T)
		foreach(GenUtils.T, (t:Int) => {
			val fieldFact:ProbabilisticClassifierFactory[FieldHistory,Feature,Int]
				= getFactory(opts.fieldModel, opts.sigmaField).asInstanceOf[ProbabilisticClassifierFactory[FieldHistory,Feature,Int]]
			//val fieldFact:ProbabilisticClassifierFactory[FieldHistory,Feature,Int]
			//	= new MaximumEntropyClassifier.Factory(opts.sigmaField, opts.maxentIterations, null)
			fieldChoices(t) = MetaClass.create(opts.fieldChoice).createInstance(new java.lang.Integer(t), fieldFact, opts);
		})
		//add data
		foreach(opts.trainStart, opts.trainEnd, (i:Int) => {
			if(!isFail(i) && useAlignment(i)){
				val sent:AlignedDataset.AlignedSentence = alignments.aligns(i)
				val ws:WorldState = alignments.worldStates(i)
				fieldChoices.foreach( (fieldChoice:FieldChoice) => fieldChoice.addSentence( (sent,ws) ))
			}
		})
		//train
		fieldChoices.foreach( (fieldChoice:FieldChoice) => {
			fieldChoice.train;
		})
		//dump
		fieldChoices.foreach( (fc:FieldChoice) => { fc.dumpParams("field(" + GenUtils.tstr(fc.t) + ")") } )
	}

	def trainWordChoice = {
		//create
		val wordFact:ProbabilisticClassifierFactory[WordHistory,Feature,Int]
			= getFactory(opts.wordModel, opts.sigmaWord).asInstanceOf[ProbabilisticClassifierFactory[WordHistory,Feature,Int]]
		//val wordFact:ProbabilisticClassifierFactory[WordHistory,Feature,Int]
		//	= new MaximumEntropyClassifier.Factory(opts.sigmaWord, opts.maxentIterations, null)
		val indexer = problem.wordIndexer
		wordChoice = MetaClass.create(opts.wordChoice).createInstance(
				(hist:Array[Int], word:Int) => {
					var tail = List[Int]();
					foreach(opts.numGrams-1, (i:Int) => {
						if(i < hist.length) tail = hist(hist.length-i-1) :: tail
					})
					if(word==wordChoice.stopTerm) lm.stopProb(tail, indexer) else lm.nextWordProb(word, tail, indexer)
				}, 
				wordFact, 
				opts
			)
		//add data
		foreach(opts.trainStart, opts.trainEnd, (i:Int) => {
			if(!isFail(i) && useAlignment(i)){
				val sent:AlignedDataset.AlignedSentence = alignments.aligns(i)
				val ws:WorldState = alignments.worldStates(i)
				wordChoice.addSentence( (sent,ws) )
			}
		})
		//train
		wordChoice.train;
		//dump
		wordChoice.dumpParams("word")
	}
	

	def trainFieldSetChoices = {
		//create
		fieldSetChoices = new Array[FieldSetChoice](GenUtils.none_t+1) //0 to none_t
		foreach(GenUtils.T+1, (t:Int) => {
			//(create choice)
			val javaT:java.lang.Integer = t //compiler complains about a scala int being cast to a java Integer
			val fsFact:ProbabilisticClassifierFactory[FieldSetHistory,Feature,FieldSet]
				= new MaximumEntropyClassifier.Factory(opts.sigmaFieldSet, opts.maxentIterations, null)
			var temps:Array[Template] = Array[Template](Template.defaultTemplate(t))
			if(templates contains t){
				temps = templates(t)
			}
			fieldSetChoices(t) = MetaClass.create(opts.fieldSetChoice).createInstance(javaT, temps, fsFact, lm, opts);
			//(register fieldsets)
			temps.foreach( (m:Template) => {
				fieldSetChoices(t).registerOut(m.fieldSet)
			})

		})
		/*
		//add data
		print("Adding data...")
		foreach(opts.trainStart, opts.trainEnd, (i:Int) => {
			if(!isFail(i) && useAlignment(i)) {
				val sent:AlignedDataset.AlignedSentence = alignments.aligns(i)
				val ws:WorldState = alignments.worldStates(i)
				fieldSetChoices.foreach( (fsChoice:FieldSetChoice) => fsChoice.addSentence( (sent,ws) ))
			}
		})
		println("done")
		//train
		fieldSetChoices.foreach( (fsChoice:FieldSetChoice) => {
			println("Training for " + GenUtils.tstr( fsChoice.t ))
			fsChoice.train;
		})
		//dump
		fieldSetChoices.foreach( (fsc:FieldSetChoice) => fsc.dumpParams("fieldChoice(" + GenUtils.tstr(fsc.t) + ")") )
		*/
	}
	
	def trainTemplateChoices(fields:Int=>Array[FieldSet]) = {
		templateChoices = new Array[HashMap[FieldSet,TemplateChoice]](GenUtils.none_t+1) //0 to none_t
		foreach(GenUtils.T+1, (t:Int) => {
			val javaT:java.lang.Integer = t //compiler complaints again about scala/java ints
			var temps:Array[Template] = Array[Template](Template.defaultTemplate(t))
			if(templates contains t){
				temps = templates(t)
				if(temps == null) throw new IllegalStateException("No templates for event type " + GenUtils.tstr(t))
			}
			templateChoices(t) = new HashMap[FieldSet,TemplateChoice]
			//fore each possible field set
			fields(t).filter( (fs:FieldSet) => fs != null).foreach( (fs:FieldSet) => {
				//(create)
				val templateFact:ProbabilisticClassifierFactory[TemplateHistory,Feature,Template]
					= new MaximumEntropyClassifier.Factory(opts.sigmaTemplate, opts.maxentIterations, null)
				val choice:TemplateChoice = MetaClass.create(opts.templateChoice).createInstance(
																	javaT, 
																	temps, 
																	fs, 
																	templateFact, 
																	lm, 
																	opts)
				templateChoices(t).put(fs, choice)
				/*
				//(add data)
				println("Adding data for " + GenUtils.tstr( choice.t ))
				foreach(opts.trainStart, opts.trainEnd, (i:Int) => {
					if(!isFail(i) && useAlignment(i)){
						val sent:AlignedDataset.AlignedSentence = alignments.aligns(i)
						val ws:WorldState = alignments.worldStates(i)
						choice.addSentence( (sent,ws) )
					}
				})
				//(train)
				println("Training Template for " + GenUtils.tstr( choice.t ) + ", field set=" + fs)
				choice.train
				//(dump)
				choice.dumpParams("template(" + GenUtils.tstr(choice.t) + ")-" + fs)
				*/
			})
		})
	}

	//parent -> child
	var parentMap:HashMap[Template,Template] = new HashMap[Template,Template]
	def internThroughParent(m:Template):Template = {
		m.parents.foreach( (parent:Template) => {
			if(parentMap contains parent) return parentMap(parent)
		})
		println("Template:")
		println(m)
		println("Parents:")
		m.parents.foreach( println(_) )
		throw new IllegalArgumentException("Template could not be intered: " + m)
	}
	
	def templateTrain = {
		
		//--Create Templates
		track("Creating Templates")
		//(get templates)
		if(opts.entityRegex != null){
			logs("Using entity regex: " + opts.entityRegex);
			Template.ENTITY_REGEX = opts.entityRegex
		}
		var templateArray:Array[Template] = Template.createTemplates(alignments, opts.trainStart, opts.trainEnd, useAlignment)
		logs("" + templateArray.length + " templates")
		//(optional: prune templates)
		if(opts.oneChildTemplPerParent){
			templateArray = templateArray.filter( (m:Template) => {
					var contains:Boolean = false
					var parent:Template = null
					if(m.parents.size > 1){
						println("TEMPL: " + m)
						m.parents.foreach( println(_) )
						throw new IllegalStateException("This procedure might fail... " + m.parents.size)
					}
					m.parents.foreach( (p:Template) => {if(parentMap contains p){ parent=p; contains = true} })
					if(contains){
						false
					}else{
						m.parents.foreach( (p:Template) => {parent=p})
						parentMap(parent) = m
						true
					}
				})
			logs("PRUNED: " + templateArray.length + " templates")
		}
		//(dump templates)
		try {
			val f:File =  new File( fig.exec.Execution.getFile("templates") );
			if(!f.exists) f.createNewFile();
			val writer:FileWriter = new FileWriter(f)
			templateArray.foreach( (m:Template) => {
				writer.append(m.dump)
			})
			writer.flush; writer.close;
		} catch {
			case (e:Exception) => logs("ERROR: Could not dump templates: " + e.getMessage);	
		}
		//(sort into event types)
		val lstHash:HashMap[Int,List[Template]] = new HashMap[Int,List[Template]]
		templateArray.foreach( (m:Template) => {
			val last:List[Template] = if(lstHash.contains(m.getT)) lstHash(m.getT) else List[Template]()
			lstHash.put(m.getT, m :: last)
		})
		if(opts.alignProhibitNone && lstHash.contains(GenUtils.none_t)){
			throw new IllegalStateException("Template aligned to none_t, but none_t prohibited")
		}
		//(make lists into arrays)
		templates = new HashMap[Int,Array[Template]]
		lstHash.foreach( pair => {
			templates += pair._1 -> pair._2.toArray
			logs("" + GenUtils.tstr(pair._1) + ": " + pair._2.length + " templates")
			println("" + GenUtils.tstr(pair._1) + ": " + pair._2.length + " templates")
		
		})
		end_track
		//(initialize choices)
		//note: 'train' is a missnomer in these functions
		logs("Making Event Choice"); trainEventChoice
		logs("Making FieldSet Choice"); trainFieldSetChoices	//must train fieldset first
		logs("Making Template Choices"); trainTemplateChoices( (t:Int) => fieldSetChoices(t).fieldSetOutputs )
		
		
		//--Create Generator
		track("Building Generator")
		import org.goobs.choices.ChoiceGenerator.ParentInfo
		import org.goobs.choices.ChoiceGenerator.ForwardInfo
		import org.goobs.choices.ChoiceGenerator.BackInfo
		import org.goobs.functional.Function
		//event choice
		def eventGenerator:ChoiceGenerator[(EventHistory,WorldState),Int] = {
			if(opts.forceTrueEvents){
				def memorizeFn(ws:WorldState):Array[Int] = {
					foreach(alignments.size, (i:Int) => {
						if(alignments.worldStates(i) eq ws){
							return GenUtils.toMarkov(alignments.trueEvents(i))
						}
					})
					foreach(golds.size, (i:Int) => {
						if(golds.worldStates(i) eq ws){
							return GenUtils.toMarkov(golds.trueEvents(i))
						}
					})
					throw new IllegalArgumentException("No such world state")
				}
				return new ChoiceGenerator[(EventHistory,WorldState),Int](new StrictEventChoice(memorizeFn(_), templates.contains(_), opts).asInstanceOf[AChoice[(EventHistory,WorldState),Object]])
			} else {	
				return new ChoiceGenerator[(EventHistory,WorldState),Int](eventChoice.asInstanceOf[AChoice[(EventHistory,WorldState),Object]])
			}
		}
		//fieldSet choice
		def fsGenerator = eventGenerator.append(
			//next choice
			new Function[ParentInfo[(EventHistory,WorldState),Int],AChoice[(FieldSetHistory,WorldState),FieldSet]]{
				override def eval(parent:ParentInfo[(EventHistory,WorldState),Int]) = { 
					val choice = fieldSetChoices(GenUtils.e2t(parent.pOut,parent.pIn._2))
					if(choice.isEmpty){
						throw new IllegalStateException("An event was generated that has no training examples (no field sets): " + GenUtils.tstr(GenUtils.e2t(parent.pOut, parent.pIn._2)))
					} else {
						choice
					}
				}
			},
			//next history
			new Function[ForwardInfo[(EventHistory,WorldState),Int,(FieldSetHistory,WorldState)],(FieldSetHistory,WorldState)]{
				override def eval(parent:ForwardInfo[(EventHistory,WorldState),Int,(FieldSetHistory,WorldState)]) = {
					val fsh:FieldSetHistory = new FieldSetHistory(	parent.pIn._1.eventHist,
																	parent.pOut, 
																	parent.pIn._1.eventHist.length == 0, 
																	parent.pIn._1.genSoFar,
																	false)
					//println("Event->FS : " + GenUtils.arrayToString(parent.pIn._1.genSoFar))
					( fsh, parent.pIn._2 )
				}
			},
			//back info
			new Function[BackInfo[(FieldSetHistory,WorldState),FieldSet,(EventHistory,WorldState)],(EventHistory,WorldState)]{
				override def eval(child:BackInfo[(FieldSetHistory,WorldState),FieldSet,(EventHistory,WorldState)]) = {
					val eventHist:Array[Int] = child.pIn._1.eventHist	//eventHist stays the same
					val genSoFar:Array[String] = child.cIn._1.genSoFar	//gen so far is from child
					val ws:WorldState = child.pIn._2
					//println("Event<-FS : " + GenUtils.arrayToString(genSoFar))
					(new EventHistory(eventHist, genSoFar), ws)
				}
			}
		)
		//template choice
		def tempGenerator = fsGenerator.append(
			//next choice
			new Function[ParentInfo[(FieldSetHistory,WorldState),FieldSet],AChoice[(TemplateHistory,WorldState),Template]]{
				override def eval(parent:ParentInfo[(FieldSetHistory,WorldState),FieldSet]) = { 
					val fs = parent.pOut
					val t = GenUtils.e2t( parent.pIn._1.parentE, parent.pIn._2 )
					if(templateChoices(t) contains fs){
						templateChoices(t)(fs)
					}else{
						throw new IllegalStateException(
							"Field set for record does not exist:  t=" + GenUtils.tstr(t) + " fs=" + fs);
					}
				}
			},
			//next history
			new Function[ForwardInfo[	(FieldSetHistory,WorldState),FieldSet,(TemplateHistory,WorldState)],
										(TemplateHistory,WorldState)]{
				override def eval(parent:ForwardInfo[(FieldSetHistory,WorldState),FieldSet,(TemplateHistory,WorldState)]) = {
					val eSoFar:Array[Int] = parent.pIn._1.eSoFar
					val parentE:Int = parent.pIn._1.parentE
					val genSoFar:Array[String] = parent.pIn._1.genSoFar
					//println("\tFS->Templ : " + GenUtils.arrayToString(genSoFar))
					val mh:TemplateHistory = new TemplateHistory(eSoFar, parentE,genSoFar,false)
					( mh, parent.pIn._2 )
				}
			},
			//back history
			new Function[BackInfo[(TemplateHistory,WorldState),Template,(FieldSetHistory,WorldState)],(FieldSetHistory,WorldState)]{
				override def eval(child:BackInfo[(TemplateHistory,WorldState),Template,(FieldSetHistory,WorldState)]) = {
					val eSoFar:Array[Int] = child.pIn._1.eSoFar
					val parentE:Int = child.pIn._1.parentE	//parentE stays the same
					val isFirst:Boolean = child.pIn._1.isFirst //isFirst stays the same
					val genSoFar:Array[String] = child.cIn._1.genSoFar	//gen so far is from child
					val generated:Boolean = child.pIn._1.generated	//whether we've generated
					val ws:WorldState = child.pIn._2
					//println("\tFS<-Templ : " + GenUtils.arrayToString(genSoFar))
					(new FieldSetHistory(eSoFar, parentE, isFirst, genSoFar, generated), ws)
				}
			}
		)

		//fill template
		//val filledTempChoice = new TemplateFillChoice(opts)
		val filledTempChoice:TemplateFillChoice = MetaClass.create(opts.templateFillChoice).createInstance(opts)
		def filledTempGenerator = tempGenerator.append(
			//next choice
			new Function[ParentInfo[(TemplateHistory,WorldState),Template],
						AChoice[(TemplateFillHistory,WorldState),(Int,Array[Int],Array[String])]]{
				override def eval(parent:ParentInfo[(TemplateHistory,WorldState),Template]) = { 
					filledTempChoice.asInstanceOf[AChoice[(TemplateFillHistory,WorldState),(Int,Array[Int],Array[String])]]
				}
			},
			//next history
			new Function[ForwardInfo[	(TemplateHistory,WorldState),Template,(TemplateFillHistory,WorldState)],
									(TemplateFillHistory,WorldState)]{
				override def eval(parent:ForwardInfo[(TemplateHistory,WorldState),Template,(TemplateFillHistory,WorldState)]) = {
					val e = parent.pIn._1.parentE
					val parentTemplate = parent.pOut
					val genSoFar = parent.pIn._1.genSoFar
					val ws = parent.pIn._2
					//println("\t\tTempl->Fill : " + GenUtils.arrayToString(genSoFar))
					(new TemplateFillHistory(e, parentTemplate, genSoFar, false), ws)
				}
			},
			//back history
			new Function[BackInfo[(TemplateFillHistory,WorldState),(Int,Array[Int],Array[String]),(TemplateHistory,WorldState)],(TemplateHistory,WorldState)]{
				override def eval(child:BackInfo[(TemplateFillHistory,WorldState),(Int,Array[Int],Array[String]),(TemplateHistory,WorldState)]) = {
					val eSoFar:Array[Int] = child.pIn._1.eSoFar //event history stays the same
					val parentE:Int = child.pIn._1.parentE	//parentE stays the same
					val generated:Boolean = child.pIn._1.generated	//whether we've generated
					val ws:WorldState = child.pIn._2
					val prevGen:Array[String] = child.pIn._1.genSoFar //generated by last template
					val newGen:Array[String] = child.cIn._1.genSoFar //generation from this history
					//println("\t\tTempl<-Fill : " + GenUtils.arrayToString(Array.concat(prevGen,newGen)))
					(new TemplateHistory(eSoFar, parentE, newGen, generated), ws)
				}
			}
			
		)
		templateGenerator = filledTempGenerator
		end_track

		//--Create Choices
		//(range checker)
		trainRangeChecker
		//(choices)
		track("Training Choices")


		//--Case: Add data the new way
		logs("Adding Data")
		var lastFS:FieldSet = null;
		var choiceSeqSave = List[java.util.LinkedList[Object]]()
		foreach(opts.trainStart, opts.trainEnd, (i:Int) => {
			if(useAlignment(i)){
				//register aligned sentences
				val as:AlignedSentence = alignments.aligns(i)
				val ws:WorldState = alignments.worldStates(i)
				eventChoice.registerSentence(as, ws)
				fieldSetChoices.foreach( (x:FieldSetChoice) => x.registerSentence(as, ws) )
				templateChoices.foreach( (x:HashMap[FieldSet,TemplateChoice]) => 
					x.keys.foreach( (fs:FieldSet) => {
						x(fs).registerSentence(as, ws)
					})
				)
				//add data
				val choices:java.util.LinkedList[Object] = getChoiceSequence(alignments.aligns(i), alignments.worldStates(i))
				choiceSeqSave = choices :: choiceSeqSave
				if(!isFail(i) && useAlignment(i)){
				templateGenerator.dryRun(
						(eventChoice.noHist,alignments.worldStates(i)),
						choices,
						new ChoiceGenerator.ChoiceCallback(){
							override def choiceMade(choice:Class[_ <: java.lang.Object], hist:Any, out:Any):Unit = {
								if(classOf[EventChoice].isAssignableFrom(choice)){
									eventChoice.addExample( hist.asInstanceOf[(EventHistory,WorldState)],
																out.asInstanceOf[Int]
														  )
								}else if(classOf[FieldSetChoice].isAssignableFrom(choice)){
									val fsHist:(FieldSetHistory,WorldState) = hist.asInstanceOf[(FieldSetHistory,WorldState)]
									val parentT = GenUtils.e2t( fsHist._1.parentE, fsHist._2 )
										val fs:FieldSet = out.asInstanceOf[FieldSet]
									lastFS = fs
									fieldSetChoices(parentT).addExample( fsHist, fs )
								}else if(classOf[TemplateChoice].isAssignableFrom(choice)){
									val mHist:(TemplateHistory,WorldState) = hist.asInstanceOf[(TemplateHistory,WorldState)]
									val parentT = GenUtils.e2t( mHist._1.parentE, mHist._2 )
										if(lastFS == null) throw new IllegalStateException("null parent field set!")
									val parentFS = lastFS
									var outTempl:Template = out.asInstanceOf[Template]
									if(outTempl == TemplateChoice.stopTerm) lastFS = null
									else if(opts.oneChildTemplPerParent) outTempl = internThroughParent(outTempl)
									templateChoices(parentT)(parentFS).addExample(mHist, outTempl)
								}
							}
						},
						ChoiceGenerator.PROHIBIT_NULL_GEN
					)
				}
			}
		})
		/*
		//(double check)
		val original = choiceSeqSave.reverse.toArray
		foreach(opts.trainStart, opts.trainEnd, (i:Int) => {
			val origChoices:java.util.LinkedList[Object] = original(i-opts.trainStart)
			val choices:java.util.LinkedList[Object] = getChoiceSequence(alignments.aligns(i), alignments.worldStates(i))
			for( j <- 0 until origChoices.size){
				if(!origChoices.get(j).isInstanceOf[Tuple3[Int,Array[Int],Array[String]]] &&
						origChoices.get(j) != choices.get(j)){
					println("First time:")
					println(origChoices.get(j))
					println("Second time:")
					println(choices.get(j))
					throw new IllegalStateException("Different choices returned!")
				}
			}
		})
		*/

		//--Train
		//event
		logs("Training Event Choice")
		eventChoice.train;
		eventChoice.dumpParams("event")
		//field set
		fieldSetChoices.foreach( (fsChoice:FieldSetChoice) => {
			logs("Training FieldSet for " + GenUtils.tstr( fsChoice.t ))
			fsChoice.train;
			fsChoice.dumpParams("fieldChoice(" + GenUtils.tstr(fsChoice.t) + ")")
		})
		//template
		templateChoices.foreach( (templates:HashMap[FieldSet,TemplateChoice]) => {
			templates.keys.foreach( (fs:FieldSet) => {
				val mChoice:TemplateChoice = templates(fs)
				logs("Training Template for " + fs)
				mChoice.train
				mChoice.dumpParams("template(" + GenUtils.tstr(mChoice.t) + ")-" + fs)
			})
		})
		end_track
	}

	def simpleTrain = {
		
		//--Create Choices
		//(train rangechecker)
		trainRangeChecker
		//(train choices)
		track("Creating Choices")
		logs("Event Choice"); trainEventChoice
		logs("Field Choices"); trainFieldChoices
		logs("Word Choice"); trainWordChoice
		end_track
		
		//--Create Generator
		track("Building Generator")
		import org.goobs.choices.ChoiceGenerator.ParentInfo
		import org.goobs.choices.ChoiceGenerator.ForwardInfo
		import org.goobs.choices.ChoiceGenerator.BackInfo
		import org.goobs.functional.Function
		//event choice
		def eventGenerator = 
			new ChoiceGenerator[(EventHistory,WorldState),Int](eventChoice.asInstanceOf[AChoice[(EventHistory,WorldState),Object]])
		//field choice
		def fieldGenerator = eventGenerator.append(
			//next choice
			new Function[ParentInfo[(EventHistory,WorldState),Int],AChoice[(FieldHistory,WorldState),Int]]{
				override def eval(parent:ParentInfo[(EventHistory,WorldState),Int]) = { 
					if(parent.pOut == GenUtils.none_e(parent.pIn._2))
						new NoneFieldChoice(opts)
					else
						fieldChoices(GenUtils.e2t(parent.pOut,parent.pIn._2))
				}
			},
			//next history
			new Function[ForwardInfo[(EventHistory,WorldState),Int,(FieldHistory,WorldState)],(FieldHistory,WorldState)]{
				override def eval(parent:ForwardInfo[(EventHistory,WorldState),Int,(FieldHistory,WorldState)]) = {
					val t:Int = GenUtils.e2t(parent.pOut,parent.pIn._2)
					val fh:FieldHistory 
						= if(parent.pOut == GenUtils.none_e(parent.pIn._2))
							new FieldHistory( new Array[Int](0), parent.pOut)						
						else
							new FieldHistory( fieldChoices(t).noHist.fieldHist, parent.pOut )
					( fh, parent.pIn._2 )
				}
			},
			//back info
			new Function[BackInfo[(FieldHistory,WorldState),Int,(EventHistory,WorldState)],(EventHistory,WorldState)]{
				override def eval(child:BackInfo[(FieldHistory,WorldState),Int,(EventHistory,WorldState)]) = {
					val eventHist:Array[Int] = child.pIn._1.eventHist	//eventHist stays the same
					val ws:WorldState = child.pIn._2
					//println("Event<-FS : " + GenUtils.arrayToString(genSoFar))
					(new EventHistory(eventHist, Array[String]()), ws) //TODO pass back genSoFar
				}
			}
		)
		//word choice
		def wordGenerator = fieldGenerator.append(
			//next choice
			new Function[ParentInfo[(FieldHistory,WorldState),Int],AChoice[(WordHistory,WorldState),Int]]{
				override def eval(parent:ParentInfo[(FieldHistory,WorldState),Int]) 
					= wordChoice
			},
			//next history
			new Function[ForwardInfo[(FieldHistory,WorldState),Int,(WordHistory,WorldState)],(WordHistory,WorldState)]{
				override def eval(parent:ForwardInfo[(FieldHistory,WorldState),Int,(WordHistory,WorldState)]) = {
					if(parent.pIn._2 == null) throw new IllegalStateException("World state is null!")
					( 	new WordHistory(parent.hist._1.wordHist, parent.pOut, parent.pIn._1.parentEvent, 0),
						parent.pIn._2	)
				}
			},
			//back history
			new Function[BackInfo[(WordHistory,WorldState),Int,(FieldHistory,WorldState)],(FieldHistory,WorldState)]{
				override def eval(child:BackInfo[(WordHistory,WorldState),Int,(FieldHistory,WorldState)]) = {
					val parentE:Int = child.pIn._1.parentEvent	//parentE stays the same
					val fHist:Array[Int] = child.pIn._1.fieldHist	//parentE stays the same
					val ws:WorldState = child.pIn._2
					(new FieldHistory(fHist, parentE), ws)
				}
			}
		)
		//set simple generator
		simpleGenerator = wordGenerator
		end_track
	}

	
	/*
	*				-----------------
	*					GENERATION
	*				------------------
	*/

	def getChoiceSequence(as:AlignedDataset.AlignedSentence,ws:WorldState):java.util.LinkedList[Object] = {
		if(as==null || ws==null) return null
		val rtn = new java.util.LinkedList[Object]()
		
		//--Get Levels
		//(event)
		val events:Array[Int] = GenUtils.toMarkov(as.observedEvents)
		//(templates)
		val templateData = Template.alignedSentence2Templates(as,ws)
		val templates:Array[Template] = templateData.map( _._1 ).toArray
		val words:Array[Array[String]] = templateData.map( _._3).toArray
		val fields:Array[Array[Int]] = new Array[Array[Int]](templates.length)
		for( i <- 0 until fields.length ){
			val m:Template = templates(i)
			val rtn:Array[Int] = m.fill(events(i), ws)._1
			if(rtn.length != words(i).length) throw new IllegalStateException("Assumption wrong here")
			fields(i) = rtn
		}
		//(field sets)
		val fieldSets:Array[FieldSet] = templates.map( (m:Template) => m.fieldSet )
		//(error check)
		if(events.length != templates.length || events.length != fieldSets.length)
			throw new java.lang.IllegalArgumentException("Lengths do not match: " 
				+ events.length + ", " 
				+ fieldSets.length + ", " 
				+ templates.length )

		//--Append outputs
		def append(i:Int):Unit = {
			if(i < events.length){
				rtn.add(events(i).asInstanceOf[Object])			//generate an event
				rtn.add(fieldSets(i))							//generate a field set
				rtn.add(templates(i))							//generate a template
				rtn.add((events(i),fields(i),words(i)));		//fill the template
				rtn.add(TemplateFillChoice.stopTerm);			//stop
				rtn.add(GenUtils.STOP_TEMPLATE)					//stop
				rtn.add(GenUtils.STOP_FS)						//stop
				append(i+1)										//append the rest
			}else{
				rtn.add(GenUtils.STOP_INT.asInstanceOf[Object])
			}
		}
		append(0)

		return rtn
	}

	def commonGenerate(fn:(AlignedDataset.AlignedSentence, WorldState,Int)=>GenResult):Unit = {
		import AlignedDataset.AlignedSentence
		//--Create save functions
		var trains = List[(AlignedSentence,Array[String],WorldState)]()
		var tests = List[(AlignedSentence,Array[String],WorldState)]()
		val trainFn = (guess:AlignedSentence, gold:Array[String], ws:WorldState) => {
			trains = (guess,gold, ws) :: trains
		}
		val testFn = (guess:AlignedSentence, gold:Array[String], ws:WorldState) => {
			tests = (guess,gold, ws) :: tests
		}
		//--Results
		if(opts.scoreResults) {
			//(generate results)
			generateResults( fn, trainFn, testFn )
			track("Saving results")
			//(save trains)
			logs("Training alignments")
			val trainDataset:AlignedDataset = new AlignedDataset(trains.length)
			var i = 0
			trains.foreach( ( a:(AlignedSentence,Array[String],WorldState) ) => {
				trainDataset.addAlignment(i, a._1, new Array[Int](0), a._2, a._3)
				i += 1
			})
			trainDataset.saveAlignment( fig.exec.Execution.getFile("alignments-train") ) 
			//(save tests)
			logs("Test alignments")
			val testDataset:AlignedDataset = new AlignedDataset(tests.length)
			i = 0
			tests.foreach( ( a:(AlignedSentence,Array[String],WorldState) ) => {
				testDataset.addAlignment(i, a._1, new Array[Int](0), a._2, a._3)
				i += 1
			})
			testDataset.saveAlignment( fig.exec.Execution.getFile("alignments-test") ) 
			end_track
		}
		//--Interactive Shell
		if(opts.interactive) generateShell( fn )
	}


	class LogState {
		import AlignedDataset.AlignedSentence
		var exNum = 0
		
		//constructor: (create directory)
		val dir:String = fig.exec.Execution.getFile("choiceLogs")
		try{
			val f:File = new File(dir); if(!f.exists) f.mkdirs()
		} catch{ case (e:Exception) => println("WARNING: Exception: " + e.getMessage) }
		
		def log(as:AlignedSentence, ws:WorldState) = {
			val file:String = dir + "/" + exNum + ".log"
			exNum += 1
			ChoiceGenerator.setChoiceLogFile(file)
			ChoiceGenerator.forceNextChoiceSequence(getChoiceSequence(as,ws))
		}
	}
	val logState = new LogState()
	
	def simpleGenerate = {
		import AlignedDataset.AlignedSentence
		val fn:(AlignedSentence, WorldState,Int)=>GenResult = (oracle:AlignedSentence, ws:WorldState,flags:Int) => {
			if(opts.logChoices) logState.log(oracle,ws)
			//(test score)
			val pair:(AlignedSentence,Double) = GenUtils.gen2sent(
				{ 
					if( opts.searchType == induction.Options.SearchType.greedy ){
						simpleGenerator.generateGreedy( 
							(eventChoice.noHist, ws), 
							if(opts.logChoices) flags | ChoiceGenerator.LOG_CHOICES else flags
						)
					} else if( opts.searchType == induction.Options.SearchType.kbest ){
						simpleGenerator.generateKBest(
							(eventChoice.noHist, ws), 
							opts.kbestK,
							if(opts.logChoices) flags | ChoiceGenerator.LOG_CHOICES else flags
						)
					} else {
						throw new IllegalArgumentException("Invalid search type: " + opts.searchType);
					}
				},
				ws
			)
			//(true score)
			val trueScore:Double = if(goldValid){
					ChoiceGenerator.forceNextChoiceSequence(getChoiceSequence(oracle,ws))
					simpleGenerator.generateGreedy((eventChoice.noHist, ws), flags).getScore
				}else{
					-1.0
				}
			new GenResult(pair._1, pair._2, trueScore)
		}
		commonGenerate(fn)
	}

	def templateGenerate = {
		import org.goobs.choices.ChoiceGenerator.Generation
		import AlignedDataset.AlignedSentence
		val fn:(AlignedSentence,WorldState,Int)=>GenResult = (oracle:AlignedSentence, ws:WorldState,flags:Int) => {
			if(opts.logChoices) logState.log(oracle,ws)
			//(test)
			var f:Int = flags
			if(opts.logChoices) f = f | ChoiceGenerator.LOG_CHOICES
			if(opts.sample) f = f | ChoiceGenerator.SAMPLE
			val gen:Generation[(Int,Array[Int],Array[String])] = 
				if( opts.searchType == induction.Options.SearchType.greedy ){
						val rtn = templateGenerator.generateGreedy( 
							(eventChoice.noHist, ws),
							if(opts.logChoices) flags | ChoiceGenerator.LOG_CHOICES else flags 
						)
						rtn
				} else if( opts.searchType == induction.Options.SearchType.kbest ){
						templateGenerator.generateKBest(
							(eventChoice.noHist, ws),
							opts.kbestK,
							if(opts.logChoices) flags | ChoiceGenerator.LOG_CHOICES else flags
						)
				} else {
					throw new IllegalArgumentException("Invalid search type: " + opts.searchType);
				}
			val pair:(AlignedSentence,Double) = Template.gen2Sent(gen,ws)
			//(true score)
			val trueScore:Double = if(goldValid){
				ChoiceGenerator.forceNextChoiceSequence(getChoiceSequence(oracle,ws))
				templateGenerator.generateGreedy((eventChoice.noHist, ws), flags).getScore
				} else { -1 }
			new GenResult(pair._1, pair._2, trueScore)
		}
		commonGenerate(fn)
	}

	case class GenResult(result:AlignedDataset.AlignedSentence, resultScore:Double, trueScore:Double);

	def generateResults( 
			gen:(AlignedDataset.AlignedSentence,WorldState,Int)=>GenResult,
			trainFn:(AlignedDataset.AlignedSentence,Array[String],WorldState)=>Unit,
			testFn:(AlignedDataset.AlignedSentence,Array[String],WorldState)=>Unit ) : Unit = {
		import AlignedDataset.AlignedSentence

		case class ResultLogger(out:org.goobs.testing.ResultLogger, name:String, shouldIndex:Boolean){
			import java.util.LinkedList
			import java.util.List

			var index:Int = 0
			val bleuScorer:BleuScorer = new BleuScorer(4) //4-grams
			var mtScorer:MTScorer.Factory = new MTScorer.Factory().setXMLPaths(
					fig.exec.Execution.getFile(name + "-ref.xml"),
					fig.exec.Execution.getFile(name + "-tst.xml")
				)
		
			val golds = new LinkedList[List[List[String]]]	//bleu golds
			val guesses = new LinkedList[List[String]]		//bleu guesses

			var guessScoreNumer:Double = 0.0
			var guessScoreDenom:Double = 0.0
			var goldScoreNumer:Double = 0.0
			var goldScoreDenom:Double = 0.0
			
			var eventPrecisionNumer:Double = 0.0
			var eventPrecisionDenom:Double = 0.0
			var eventRecallNumer:Double = 0.0
			var eventRecallDenom:Double = 0.0
			var eventExactMatchNumer:Double = 0.0
			var eventExactMatchDenom:Double = 0.0
			
			var geventPrecisionNumer:Double = 0.0
			var geventPrecisionDenom:Double = 0.0
			var geventRecallNumer:Double = 0.0
			var geventRecallDenom:Double = 0.0
			var geventExactMatchNumer:Double = 0.0
			var geventExactMatchDenom:Double = 0.0
			
			var o2geventPrecisionNumer:Double = 0.0
			var o2geventPrecisionDenom:Double = 0.0
			var o2geventRecallNumer:Double = 0.0
			var o2geventRecallDenom:Double = 0.0
			var o2geventExactMatchNumer:Double = 0.0
			var o2geventExactMatchDenom:Double = 0.0
			
			def bleu(
					test:Array[String], 
					gold:Array[String]
						):Double = {
				//converts to a java list from a scal array
				def arrayToList(in:Array[String]):java.util.LinkedList[String] = {
					val rtn = new java.util.LinkedList[String]()
					in.foreach( (str:String) => {
						rtn.addLast(str)
						//rtn.addLast( if(str equals "kicks") "passes" else str )
					})
					rtn
				}
				if(golds.size == 0){
					golds.add(new LinkedList[List[String]])
				}
				bleuScorer.setThreshold(opts.maxNumDif)
				//total score
				golds.get(0).add(arrayToList(gold))
				guesses.add(arrayToList(test))
				//immediate score
				val goldSet = new LinkedList[List[String]]
					goldSet.addLast(arrayToList(gold))
				var score:Score = bleuScorer.evaluateBleuSentence(arrayToList(test),goldSet)
				mtScorer.addSentencePair(gold,test)
				score.getScore
			}	
					
			def oracleEvents(guess:Array[Int], gold:Array[Int]):(Double,Double,Double) = {
				//(precision & recall)
				val intersect:Double = GenUtils.intersect(guess,gold).size.asInstanceOf[Double]
				if(intersect > guess.size.asInstanceOf[Double] || intersect > gold.size.asInstanceOf[Double])
					throw new IllegalStateException("Just making sure")
				eventPrecisionNumer += intersect
				eventRecallNumer += intersect
				eventPrecisionDenom += guess.size.asInstanceOf[Double]
				eventRecallDenom += gold.size.asInstanceOf[Double]
				//(exact match)
				var good:Boolean = true
				if( guess.size == gold.size){
					foreach(guess.size, (i:Int) =>{
						if(guess(i) != gold(i)) good = false
						})
					if(good) eventExactMatchNumer += 1.0
				}
				eventExactMatchDenom += 1.0
				(intersect / guess.size.asInstanceOf[Double], intersect / gold.size.asInstanceOf[Double], if(good) 1 else 0)
			}
			
			def goldEvents(guess:Array[Int], gold:Array[Int]):(Double,Double,Double) = {
				//(precision & recall)
				val intersect:Double = GenUtils.intersect(guess,gold).size.asInstanceOf[Double]
				if(intersect > guess.size.asInstanceOf[Double] || intersect > gold.size.asInstanceOf[Double])
					throw new IllegalStateException("Just making sure")
				geventPrecisionNumer += intersect
				geventRecallNumer += intersect
				geventPrecisionDenom += guess.size.asInstanceOf[Double]
				geventRecallDenom += gold.size.asInstanceOf[Double]
				//(exact match)
				var good:Boolean = true
				if( guess.size == gold.size){
					foreach(guess.size, (i:Int) =>{
						if(guess(i) != gold(i)) good = false
						})
					if(good) geventExactMatchNumer += 1.0
				}
				geventExactMatchDenom += 1.0
				(intersect / guess.size.asInstanceOf[Double], intersect / gold.size.asInstanceOf[Double], if(good) 1 else 0)
			}
			
			def oracle2GoldEvents(oracle:Array[Int], gold:Array[Int]):(Double,Double,Double) = {
				//(precision & recall)
				val intersect:Double = gold.filter( (t:Int) => oracle contains t ).size.asInstanceOf[Double]
				o2geventPrecisionNumer += intersect
				o2geventRecallNumer += intersect
				o2geventPrecisionDenom += oracle.size.asInstanceOf[Double]
				o2geventRecallDenom += gold.size.asInstanceOf[Double]
				//(exact match)
				var good:Boolean = true
				if( oracle.size == gold.size){
					foreach(oracle.size, (i:Int) =>{
						if(oracle(i) != gold(i)) good = false
						})
					if(good) o2geventExactMatchNumer += 1.0
				}
				o2geventExactMatchDenom += 1.0
				(intersect / oracle.size.asInstanceOf[Double], intersect / gold.size.asInstanceOf[Double], if(good) 1 else 0)
			}
	

			def log(guess:AlignedSentence, oracle:AlignedSentence, goldEventArray:Array[Int], ws:WorldState,
					guessScore:Double, goldScore:Double):Boolean = {
				if(GenUtils.toMarkov(goldEventArray).length == 1 && 
						GenUtils.e2t(goldEventArray(0),ws) == GenUtils.dead_t){
					println("[[skipping scoring on impossible event]]")
					index += 1
					return false
				}
				//(add the instance)
				out.add(index, GenUtils.arrayToString(guess.getWords), GenUtils.arrayToString(oracle.getWords))
				//(guessed events)
				out.addLocalString(index, "guess_events",
					GenUtils.arrayToString( 
						GenUtils.toMarkov(guess.observedEvents).map( (e:Int) => {
							GenUtils.tstr( GenUtils.e2t(e,ws) ) + "(" + e + ")"
						}) ) )
				//--Bleu
				val bleuScore:Double = bleu(
						guess.getWords.map( (s:String) => {
							try{
								val i:Int = Integer.parseInt(s);
								i.toString
							} catch {
								case (e:Exception) => { s }
							}
						}), 
						oracle.getWords.map( (s:String) => {
							try{
								val i:Int = Integer.parseInt(s);
								i.toString
							} catch {
								case (e:Exception) => { s }
						}
					}) )
				out.setLocalResult( index, "bleu", bleuScore ) 
				//--Oracle
				if(oracle.observedEvents != null){
					//(log oracle event stats)
					val (prec,recall,exact):(Double,Double,Double) 
						= oracleEvents(GenUtils.toMarkov(guess.observedEvents), GenUtils.toMarkov(oracle.observedEvents))
					out.setLocalResult(index, "oracle_event_precision", prec)
					out.setLocalResult(index, "oracle_event_recall", recall)
					out.setLocalResult(index, "oracle_event_exact", exact)
					out.setLocalResult(index, "oracle_event_F1", 2*(prec*recall)/(prec+recall))
					//(log oracle event string)
					out.addLocalString(index, "oracle_events", 
						GenUtils.arrayToString( 
							GenUtils.toMarkov(oracle.observedEvents).map( (e:Int) => {
								GenUtils.tstr(GenUtils.e2t(e,ws)) + "(" + e + ")"
							}) ) )
				}
				//--Gold
				//(log gold event stats)
				val (prec,recall,exact):(Double,Double,Double) 
					= goldEvents(GenUtils.toMarkov(guess.observedEvents), GenUtils.toMarkov(goldEventArray))
				out.setLocalResult(index, "gold_event_precision", prec)
				out.setLocalResult(index, "gold_event_recall", recall)
				out.setLocalResult(index, "gold_event_exact", exact)
				out.setLocalResult(index, "gold_event_F1", 2*(prec*recall)/(prec+recall))
				//(log gold event string)
				out.addLocalString(index, "gold_events", 
					GenUtils.arrayToString( 
						GenUtils.toMarkov(goldEventArray).map( (e:Int) => {
								GenUtils.tstr(GenUtils.e2t(e,ws)) + "(" + e + ")"
							}) ) )
				//--Alignment and WS
				out.addLocalString(index, "world_state", ws.encode)
				out.addLocalString(index, "alignment", guess.encode)
				//--Oracle to Gold
				if(oracle.observedEvents != null)
					oracle2GoldEvents(GenUtils.toMarkov(oracle.observedEvents), GenUtils.toMarkov(goldEventArray))
				//--Model Scores
				guessScoreNumer += guessScore
				guessScoreDenom += 1.0
				goldScoreNumer += goldScore
				goldScoreDenom += 1.0
				out.setLocalResult(index, "model_score_guess", guessScore)
				out.setLocalResult(index, "model_score_gold", goldScore)


				
				//--Print
				val df = new java.text.DecimalFormat("0.000")
				val sb:StringBuilder = new StringBuilder
				sb.append("[" + name + "]: ").append(index + "\t")
					.append("model_score_(guess)=" + df.format(guessScore))
					.append(" (train)=" + df.format(goldScore) + "\t")
					.append("precision=" + df.format((geventPrecisionNumer/geventPrecisionDenom)) + "\t")
					.append("recall=" + df.format((geventRecallNumer/geventRecallDenom)) + "\t")
					.append("bleu=" + df.format(bleuScore) + "\t")
				logs(sb.toString)
				//if(index % 20 == 0) print("global_bleu=" + df.format(bleuScorer.evaluateBleu(guesses, golds).getScore) )
				//(increment index)
				index += 1
				return true
			}
			
			def dump = {
				//--Set global scores
				//bleu
				val scorer:MTScorer =
					try{
						mtScorer.score
					}catch{
						case (e:Exception) => {println("WARNING: Could not evaluate BLEU score"); null}
					}
				val bleu:Double = if(scorer == null) -1.0 else scorer.getBleu
				val bleuOld:Double = bleuScorer.evaluateBleu(guesses, golds).getScore
				out.setGlobalResult("bleu", bleuOld)
				out.setGlobalResult("blue_perl", bleu)
				out.setGlobalResult("nist_perl", if(scorer != null) scorer.getNist else -1.0)
				//event
				val oraclePrec = eventPrecisionNumer / eventPrecisionDenom
				val oracleRecall = eventRecallNumer / eventRecallDenom
				val oracleExact = eventExactMatchNumer / eventExactMatchDenom
				val goldPrec = geventPrecisionNumer / geventPrecisionDenom
				val goldRecall = geventRecallNumer / geventRecallDenom
				val goldExact = geventExactMatchNumer / geventExactMatchDenom
				val o2gPrec = o2geventPrecisionNumer / o2geventPrecisionDenom
				val o2gRecall = o2geventRecallNumer / o2geventRecallDenom
				val o2gExact = o2geventExactMatchNumer / o2geventExactMatchDenom
				val modelGuess = guessScoreNumer / guessScoreDenom
				val modelGold = goldScoreNumer / goldScoreDenom
				out.setGlobalResult("oracle_event_precision", oraclePrec)
				out.setGlobalResult("oracle_event_recall", oracleRecall)
				out.setGlobalResult("oracle_event_exact", oracleExact)
				out.setGlobalResult("oracle_event_F1", 2*(oraclePrec*oracleRecall)/(oraclePrec+oracleRecall))
				out.setGlobalResult("gold_event_precision", goldPrec)
				out.setGlobalResult("gold_event_recall", goldRecall)
				out.setGlobalResult("gold_event_exact", goldExact)
				out.setGlobalResult("gold_event_F1", 2*(goldPrec*goldRecall)/(goldPrec+goldRecall))
				out.setGlobalResult("oracle2gold_event_precision", o2gPrec)
				out.setGlobalResult("oracle2gold_event_recall", o2gRecall)
				out.setGlobalResult("oracle2gold_event_exact", o2gExact)
				out.setGlobalResult("oracle2gold_event_F1", 2*(o2gPrec*o2gRecall)/(o2gPrec+o2gRecall))
				out.setGlobalResult("model_score_guess", modelGuess)
				out.setGlobalResult("model_score_gold", modelGold)
				//--Set global strings
				//
				//--Dump
				out.save(name, shouldIndex)
				
				//--Print
				println(scorer)
				println()
				println("BLEU Score (perl)   : " + bleu)
				println("BLEU Score (custom) : " + bleuOld)
				println("Event F1            : " + (2*(goldPrec*goldRecall)/(goldPrec+goldRecall)))
				println()
				println("Event Precision     : " + goldPrec)
				println("Event Recall        : " + goldRecall)
				println("Event Exact         : " + goldExact)
				println()
				println("Oracle F1           : " + (2*(oraclePrec*oracleRecall)/(oraclePrec+oracleRecall)))
				println("Oracle Precision    : " + oraclePrec)
				println("Oracle Recall       : " + oracleRecall)
				println("Oracle Exact        : " + oracleExact)
				println()
				println("Oracle to Gold F1 :" + (2*(o2gPrec*o2gRecall)/(o2gPrec+o2gRecall)))
				println()
			}
		}
		

		//--Training Data
		track("Training Data Evaluation")
		val tstart:Int = opts.trainStart
		val tend:Int = if(opts.trainEnd > alignments.size) alignments.size else opts.trainEnd
		if(tstart < 0 || tstart > tend || tend > alignments.size){
			throw new java.lang.IllegalArgumentException("Training data ill-partitioned: " + tstart + "-" + tend)
		}
		//(create lcgger)
		var logger:ResultLogger = new ResultLogger(
			new org.goobs.testing.XMLResultLogger( fig.exec.Execution.getFile("results-train.xml"), opts.testIndexFile ), 
			"training",
			false)		//shouldn't index
		//(loop over data)
		foreach(tstart, tend, (exNum:Int) => {
			if(!isFail(exNum) && useAlignment(exNum)){
				val ws:WorldState = alignments.worldStates(exNum)
				val goldEvents:Array[Int] = alignments.trueEvents(exNum)
				val oracle:AlignedSentence = alignments.aligns(exNum)
				val genResult:GenResult =  gen( 
												 oracle,
												 ws,
												 ChoiceGenerator.PROHIBIT_NULL_GEN
												)
				val guess:AlignedSentence = genResult.result
				if(guess.getWords.length == 0){
					println("[WARNING]: Null generation, inferring from language model on event: " + GenUtils.estr(goldEvents(0),ws))
					guess.strings = lm.generate(false).toArray
				}
				trainFn(guess, oracle.getWords,ws)
				logger.log(guess, oracle, goldEvents, ws, genResult.resultScore, genResult.trueScore)
			} else{
				logs("[skipping impossible example]")
			}
		})
		logger.dump
		end_track

		//--Test Data
		track("Test Data Evaluation")
		//(get test data range)
		val start:Int = opts.testStart
		val end:Int = if(opts.testEnd > (start+golds.size)) start+golds.size else opts.testEnd
		if(start < 0 || start > end || (end-start) > golds.size){
			throw new java.lang.IllegalArgumentException("Test data ill-partitioned: " + start + "-" + end)
		}
		//(create logger)
		logger = new ResultLogger( 
			new org.goobs.testing.XMLResultLogger( fig.exec.Execution.getFile("results-test.xml"), opts.testIndexFile ),
			"test",
			true)		//should index
		//(loop over data)
		foreach(start, end, (exNum:Int) => {
			if(!isFail(exNum-start,golds) && useAlignment(exNum-start)){
				val ws:WorldState = golds.worldStates(exNum-start)
				if(ws == null) throw new java.lang.IllegalStateException("No world state for exNum=" + exNum)
				val goldEvents:Array[Int] = golds.trueEvents(exNum-start)
					if(goldEvents == null) throw new java.lang.IllegalStateException("No gold events given for exNum=" + exNum)
				val genResult:GenResult = gen(
												 null,
												 ws,
												 ChoiceGenerator.PROHIBIT_NULL_GEN
												)
				val guess:AlignedSentence = genResult.result
				if(guess.getWords.length == 0){
					println("[WARNING]: Null generation, inferring from language model on event: " + GenUtils.estr(goldEvents(0),ws))
					guess.strings = lm.generate(false).toArray
				}
				val oracle:AlignedSentence = golds.aligns(exNum-start) //not really an oracle, but has true words
				testFn(guess, oracle.getWords, ws)
				logger.log(guess, oracle, goldEvents, ws, genResult.resultScore, genResult.trueScore)
			}else {
				logs("[skipping impossible example]")
			}
		})
		logger.dump
		end_track
	}


	def generateShell( gen:(AlignedDataset.AlignedSentence, WorldState,Int)=>GenResult ) : Unit = {
		//--Input Helpers
		def input(prompt:String):String = {
			print(prompt)
			val rtn = (new java.io.BufferedReader(new java.io.InputStreamReader(System.in))).readLine()
			rtn
		}
		def inputInt(prompt:String):Int = {
			var done:Boolean = false
			var rtn = -1
			while(!done){
				done = true
				val str = input(prompt)
				try{
					rtn = Integer.parseInt(str)
				}catch{
					case (e:Exception) => {
						done=false
						println("   Not an integer: " + str)
					}
				}
			}
			rtn
		}
		def str2Int(str:String) = {
			try{
				Integer.parseInt(str)
			} catch {
				case (_) => Integer.MIN_VALUE
			}
		}
		
		//--Print Helpers
		def printWorldState(ws:WorldState) = {
			def max(a:Int, b:Int) = { if(a>b) a else b }
			ws.events.foreach( (event:Event) => {
				val t:String = GenUtils.tstr(event.t)
				print(t); print("   ");
				event.fields.foreach( (field:Field) => {
					val f:String = GenUtils.fstr(event.t)(field.f)
					val v:String = GenUtils.vstr(event.t,field.f,field.value)
					print(f + "=" + v + ", ")
				})
				println()
			})
		}

		def printAlignment(sent:AlignedDataset.AlignedSentence, ws:WorldState) = {
			//(variables)
			val printer = new GenUtils.PrettyPrinter(opts)
			val events:Array[Int] = sent.observedEvents
			val eventTypes:Array[Int] = sent.eventTypes
			val fields:Array[Int] = sent.fields
			val words:Array[String] = sent.getWords
			var lastE:Int = events(0)
			var lastT:Int = eventTypes(0)
			var lastF:Int = fields(0)
			var lastW:String = words(0)
			//(first row)
			printer.logRight(GenUtils.tstr(lastT)); 
			printer.logRight(GenUtils.fstr(lastT)(lastF) + 
				{if(eventTypes(0) != GenUtils.none_t && fields(0) != GenUtils.none_f(eventTypes(0)))
					"=" + GenUtils.vstr(eventTypes(0), fields(0), ws.events(events(0)).fields(fields(0)).value) 
				else "" }
			); 
			printer.logRight(lastW);
			printer.newline
			//(print the rest)
			foreach(1, eventTypes.size, (i:Int) => {
				val e = events(i)
				val t = eventTypes(i)
				val f = fields(i)
				val w = words(i)
				if(e != lastE){
					printer.backup; printer.backup; printer.backup;
					printer.logRight(GenUtils.tstr(t)); 
					printer.logRight(GenUtils.fstr(t)(f) + 
						{if(e != GenUtils.none_e(ws) && f != GenUtils.none_f(GenUtils.e2t(e,ws))) 
							"=" + GenUtils.vstr(ws.events(e).t, ws.events(e).fields(f).f, ws.events(e).fields(f).value) 
						else "" }	); 
					printer.logRight(w);
				}else if(f != lastF){
					printer.backup; printer.backup;
					printer.logRight(GenUtils.fstr(t)(f) + 
						{if(e != GenUtils.none_e(ws) && f != GenUtils.none_f(GenUtils.e2t(e,ws))) 
							"=" + GenUtils.vstr(ws.events(e).t, ws.events(e).fields(f).f, ws.events(e).fields(f).value) 
						else "" }	); 
					printer.logRight(w);
				}else{
					printer.backup;
					printer.logRight(w);
				}
				printer.newline
				lastE = e; lastT = t; lastF = f; lastW = w;
			})
		}
		
		//--Interactive Loop
		println("\n------------------------------------\n")
		var in = input("Example> ")
		while( !in.equalsIgnoreCase("exit") ) {
			println()
			//--Get Generation Options
			val tokens = new java.util.StringTokenizer(in)
			var exNum = Integer.MIN_VALUE
			var good = true
			var sample = false; var printWS = false; var printTrue = false; var printAlign = false;
			while(tokens.hasMoreTokens){
				val token = tokens.nextToken.toLowerCase
				// search the optimal sequence rather than sampling
				if(token.equals("sample") || token.equals("sample") || token.equals("s")){
					sample = true;
				// print the world state before generating
				}else if(tokens.equals("printws") || token.equals("pws") || token.equals("w")){
					printWS = true;
				// print out the true alignment as well
				}else if(token.equals("printtruealignment") || token.equals("printtrue") || token.equals("t")) {
					printTrue = true
				}else if(token.equals("printalignment") || token.equals("printalign") || token.equals("a")){
					printAlign = true
				// example number has to be in there somewhere
				}else if(str2Int(token) > Integer.MIN_VALUE){
					if(exNum > Integer.MIN_VALUE){
						println("Cannot have two numeric terms in options")
						good = false
					}
					exNum = str2Int(token)
				}else{
					good = false
					println("Invalid generation option: " + token)
				}
			}
			//(normalize to test data)
			exNum += opts.testStart
			if(exNum < 0){
				println("Example number out of range (or not given)")
				good = false
			}
			if(exNum >= opts.testEnd){
				println("Example number is too large!")
				good = false
			}

			//--Generate
			if(good) try{
				//(get variables)
				val ws:WorldState 
					= 	if(exNum < alignments.size) alignments.worldStates(exNum)
						else golds.worldStates(exNum-opts.testStart)
				val as:AlignedDataset.AlignedSentence 
					= 	if(exNum < alignments.size) alignments.aligns(exNum)
						else golds.aligns(exNum-opts.testStart)
				//(pre-generation tasks)
				if(printWS) { println(); printWorldState(ws); println(); }
				if(printTrue) { 
						println(); println("ORACLE:"); println()
						if(exNum>=alignments.aligns.size) println("No alignment for this example") 
						else printAlignment(alignments.aligns(exNum), ws)
						println(); println("-----"); println()
				}
				//(generate)
				val genResult:GenResult
							= gen(	
									null,
									ws, 
									ChoiceGenerator.PROHIBIT_NULL_GEN | {if(sample) ChoiceGenerator.SAMPLE else 0x0}
								 )
				val generated:AlignedDataset.AlignedSentence = genResult.result
				val sent:Array[String] = generated.getWords
				//(print result)
				if(printAlign){
					println(); println("GENERATED:"); println();
					printAlignment(generated, ws)
					println(); println("-----"); println();
				}
				print("\nActual>>       "); GenUtils.printArray( as.getWords )
				print("\nGenerated>>    "); GenUtils.printArray(sent)
			} catch {
				case (e:Exception) => e.printStackTrace	
			}
			println("\n------------------------------------\n")
			in = input("Example> ")
		}
	}


}
