package generation

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import java.util.Random

import fig.basic.{IOUtils,Indexer,StatFig}
import fig.exec.Execution
import fig.exec.Execution.putLogRec
import fig.record.Record
import fig.basic.LogInfo.{logs,dbg,fail,fails,error,track,end_track}

import tea._
import tea.Utils.{map,foreach,fmt,fmt1,returnFirst,assertValid}

import org.goobs.utils.MemoryAccountable
import MemoryAccountable._
import org.goobs.utils.Tree
import org.goobs.utils.StatCalc

import induction.ProbVec



case class LanguageModel(opts:induction.Options, depth:Int, wordIndexer:Indexer[String]) extends MemoryAccountable{
	
	// An example is defined as a sentence: an array of strings
	type Example = Array[Int]
	val EMPTY_VEC:ProbVec = new ProbVec(new Array[Double](0), 0, 0)
	val rand:scala.util.Random = new scala.util.Random(8682522807148012L + System.nanoTime())
	
	//(for reading)
	val wordBreak:String = "\\s+"
	
	//(word indexer)
	val UNK:Int = wordIndexer.getIndex("*UNK*")
	val START:Int = wordIndexer.getIndex("*START*")
	val STOP:Int = wordIndexer.getIndex("*STOP*")
	val NUM:Int = wordIndexer.getIndex("*INT*")
	val numIndexer:Indexer[String] = new Indexer[String]
	def N = wordIndexer.size
	def NUM_count:Double = numIndexer.size.doubleValue
	def wstr(x:Int):String = wordIndexer.getObject(x)
	
	//(tuple indexers)
	val tupleIndexers:Array[Indexer[List[Int]]] = new Array[Indexer[List[Int]]](depth)
	def numTuples(n:Int) = tupleIndexers(n-1).size
	
	//--Major Variables
	var examples:List[Example] = List[Example]();
	val grams : Array[Array[ProbVec]] = new Array[Array[ProbVec]](depth)
	var uniqueContexts : ProbVec = new ProbVec(new Array[Double](0), 0, 0)
	val seenSequences : HashSet[List[Int]] = new HashSet[List[Int]]
	
	
	
	//-----
	// ACCESS AND HELPER METHODS
	//-----
	def estimateMemoryUsage:Int = {
		var size:Int = OBJ_OVERHEAD
		size += wordIndexer.size * (INT_SIZE + INT_SIZE + 5*CHR_SIZE) //wordIndexer
		size += numIndexer.size * (INT_SIZE + INT_SIZE + 3*CHR_SIZE) //numIndexer
		//tuple indexer
		tupleIndexers.foreach( (ind:Indexer[List[Int]]) => {
			new Range(0,ind.size,1).foreach( (i:Int) => {
				val lst:List[Int] = ind.get(i)
				size += lst.size * INT_SIZE
			})
		})
		//examples
		examples.foreach( (ex:Example) => {
			size += ex.size * INT_SIZE
		})
		//grams
		grams.foreach( (ar:Array[ProbVec]) => {
			ar.foreach( (v:ProbVec) => {
				size += v.size * DBL_SIZE
			})
		})
		//return	
		size
	}

	def dumpMemoryUsage(minUse:Int):Tree[MemoryReport] = {
		val size:Int = estimateMemoryUsage
		if(size >= minUse){
			val rtn:Tree[MemoryReport] = new Tree[MemoryReport](new MemoryReport(size, "Language Model"))
			rtn
		}else{
			new Tree[MemoryReport]
		}
	}

	def tailToInt(tail:List[Int]):Int = {
		if(tupleIndexers(tail.size).contains(tail))
			tupleIndexers(tail.size).getIndex(tail)
		else
			-1
	}
	
	def getProbVec(tail:List[Int]):ProbVec = {
		val tailInt = tailToInt(tail)
		if(tailInt < 0)
			EMPTY_VEC
		else
			grams(tail.size)(tailInt)
	}
	
	def isUnk(word:String):Boolean = {
		!wordIndexer.contains(word)
	}
	
	def isInt(word:String):Boolean = {
		val chars:Array[char] = word.toCharArray
		def isIntRec(start:Int):Boolean = {
			if(start>=chars.size) true
			else (chars(start)>='0' && chars(start)<='9' && isIntRec(start+1))
		}
		if(chars(0)=='-' && chars.size>1) isIntRec(1)
		else isIntRec(0)
	}
	
	
	//-----
	// STANDARD METHODS
	//-----

	def readData(sentences:Array[Array[String]]):Unit = {
		sentences.foreach( (sent:Array[String]) => {
			examples 
				= lineToExample( sent.foldRight("")( (rolling:String,term:String) => rolling + " " + term ) ) :: examples
		})
	}
	
	def readData(path:String, maxExamples:Int) : Int = {
		var read = 0;
		Utils.foreachLine(path, maxExamples, { line:String =>
			examples = lineToExample(line) :: examples
			read += 1
			true
		})
		read
	}
	
	//readData - read the data at the given path
	def readData : Unit = {
		//(variables)
		val maxExamples = opts.lmMaxExamples
		var numExamples = 0
		//(helper functions)
		def needMoreExamples_? = numExamples < maxExamples
		def validName(path:String) = Utils.empty_?(opts.lmFileExt) || path.endsWith(opts.lmFileExt)
		
		//--Recursively Add Files in Dir
		def addPath(path:String) : Unit = {
			import java.io.File
//			println("adding " + path);
			val file:java.io.File = (new File(path)).getCanonicalFile
			if((new File(path)).isDirectory) {
				//(add subdirectories)
				var files = (new File(path)).list
				if(files == null) {
					println("SKIPPING: " + path);
//					java.lang.Thread.sleep(2000)
					files = (new File(path)).list
				}else{
//					println("ADDING: " + path);
					Utils.sortWithEmbeddedInt_!(files).foreach( (f:String) => {
						if(needMoreExamples_?) addPath(path + "/" + f)
					})
				}
			} else if (needMoreExamples_? && validName(path)) {
				//(add file)
				numExamples += readData(path, maxExamples-numExamples)
			}
		}
		
		foreach(opts.lmInputPaths.size, (i:Int) => addPath(opts.lmInputPaths.get(i)) )
	}
	
	
	//train - train the language model
	def train : Unit = {

		def updateNGram(ex:Example, n:Int, doUpdate:Boolean) : Unit = {
			val indexer = tupleIndexers(n-1)
			val gram = grams(n-1)
			
			//--Count grams up until stop
			foreach(0, ex.size+1, (pos:Int) => {
				//(create the tuple key)
				var tuple:List[Int] = List[Int]()
				val start = pos-(n-1)
				foreach(start, pos, (i:Int) => {
					if( i < 0 ) { 
						tuple = START :: tuple
					} else {
						tuple = ex(i) :: tuple
					}
					true
				})
				tuple = tuple.reverse //inefficient
				//(increment the count for that tuple)
				val tupInt:Int = indexer.getIndex(tuple)
				if(doUpdate){
					val word:Int = if(pos < ex.size) ex(pos) else STOP
					//(update the unique contexts if applicable)
					if(n == 2){ //only care about single previous word
						val completeSequence = tuple ::: List[Int](word)
						if(!seenSequences.contains(completeSequence)){
							uniqueContexts.addCount_!(word, 1.0)
							seenSequences += completeSequence
						}
					}
					//wrong I think:  if(n == 2 && gram(tupInt).getCount(word) == 0) uniqueContexts.addCount_!(word, 1.0)
					//(update the count)
					gram(tupInt).addCount_!(word, 1.0)
					//(update NUM_count if needed)
				}
			})
		}		
		
		//--Set up the grams
		import ProbVec.{zeros,zeros2}
		foreach(depth, (n:Int) => {
			tupleIndexers(n) = new Indexer[List[Int]] 
		})
		putLogRec("total words", N)
		
		//--Prepare to Count
		//(initial count)
		examples.foreach( (ex:Example) =>
			foreach(depth, (n:Int) => updateNGram(ex, n+1, false)) )
		//(set up variables)
		foreach(depth, (n:Int) => grams(n) = zeros2(numTuples(n+1),N) )
		foreach(depth, (n:Int) => putLogRec((n+1) + "-gram tails", numTuples(n+1)) )
		if(depth >= 2) uniqueContexts = zeros(N)
		
		//--Count the Grams
		//(count some UNKs)
		grams(0)(tupleIndexers(0).getIndex(List[Int]())).addCount_!(UNK, unkNum)
		uniqueContexts.addCount_!(UNK, unkNum)
		uniqueContexts.addCount_!(NUM, 1)
		//(count training data)
		examples.foreach( (ex:Example) => {
			foreach(depth, (n:Int) => updateNGram(ex, n+1, true)) 
		})

		//--Error Check
		foreach(N, (w:Int) => {
			if(w != START && uniqueContexts.getProb(w) == 0.0) 
				throw new IllegalStateException("No counts for observed word " + w + " => " + wstr(w))
		})
		
	}
	
	
	def getNGramCount(word:Int, tail:List[Int]) = {
		val gInt:Int = tupleIndexers(tail.size).getIndex(tail)
		grams(tail.size)(gInt).getCount(word)
	}
	
	def getNGramProb(word:Int, tail:List[Int]) = {
		val gInt:Int = tupleIndexers(tail.size).getIndex(tail)
		val prob = grams(tail.size)(gInt).getProb(word)
		if(word == NUM) prob/NUM_count else prob
	}

	def getLogProbStats:StatCalc = {
		//--Base N Counter
		case class BaseNCounter(n:Int) {
			var num:Long = -1
			def tick(decimalPlaces:Int):List[Int] = {
				//(variables)
				num += 1
				var rest = num
				var lst = List[Int]()
				//(fill decimal places)
				var i = decimalPlaces; while(i > 0){ i -= 1;
					lst = (rest % n).asInstanceOf[Int] :: lst
					rest = (rest - (rest % n)) / n
				}
				//(return; null if overflow)
				if(rest != 0) null
				else lst
			}
		}
		//--StatCalc
		//(variables)
		val calc = new StatCalc
		val counter = new BaseNCounter(N)
		var tail:List[Int] = counter.tick(depth-1)
		//(enter values)
		while(tail != null){
			if(!tail.contains(STOP) && !tail.contains(UNK)){	//tail can't contain stop or unk
				foreach(N, (w:Int) => {
					if(w != START && w != UNK){ //don't allow start or unk term as word
						val prob:Double = getCondProb(w,tail)
						if(prob == 0.0) 
							throw new IllegalStateException("0 probability for sequence " + tail.map(wstr(_)) + " -> " + wstr(w))
						calc.enter(Math.log(getCondProb(w, tail)))
					}
				})
			}
			tail = counter.tick(depth-1)
		}
		//(return)
		return calc		
	}
	
	//-----
	// LIKELY TO OVERWRITE
	//-----
	
	def unkNum = 1	//assume we've seen this many unks in our training
	
	def lineToExample(line:String) : Example = {
		val tokens:Array[String] = line.trim.split(wordBreak)
		tokens.map( (t:String) => {
			val word = t.trim
			if(isInt(word)) { numIndexer.getIndex(word); NUM }
			else { wordIndexer.getIndex(word) }
		})
	}
	
	def getCondProb(word:Int, tail:List[Int]) : Double = {
		// note: don't use this
		getNGramProb(word, tail)
	}
	
	//-----
	// TOP LEVEL METHODS
	//-----
	
	
	def getSentenceProb(array:Array[Int]):Double = {
		//--Prepare the sentence
		var sent:List[Int] = array.toList
		foreach(depth-1, (i:Int) => sent = START :: sent)
		sent = sent ::: List[Int](STOP)
		
		//--Get the probability
		var prob:Double = 1.0
		foreach(array.size+1, (pos:Int) => {
			//(get the tuple)
			var tuple:List[Int] = List[Int]()
			val start = pos-(depth-1)
			foreach(start, pos, (i:Int) => {
				if( i < 0 )
					tuple = START :: tuple
				else
					tuple = array(i) :: tuple
			})
			tuple = tuple.reverse //inefficient
			//(get the prob)
			val word = if(pos >= array.size) STOP else array(pos)
			val wordProb = getCondProb(word, tuple)
			if(!opts.allowZeroProbSentence && wordProb == 0){
				fail("Word has zero probability in context: " + sent(pos) + " | " + tuple)
			}
			prob *= wordProb
		})
		
		//--Return
		prob
	}
	
	def getSentenceProb(x:Array[String]):Double = {
		getSentenceProb( x.map( (x:String) => 
			if (isUnk(x)) UNK else wordIndexer.getIndex(x) ))
	}

	def getFragmentProb(history:Array[String], fragment:Array[String], isLast:Boolean, logScale:Boolean):Double = {
		//--Prepare the sentence
		var sent:List[Int] = fragment.map( (x:String) => 	if(isInt(x)) NUM
															else if(isUnk(x)) {println("[WARNING]: unk: " + x); UNK }
															else wordIndexer.getIndex(x) ).toList
		val hist:Array[Int] = history.map( (x:String) => if(isInt(x)) NUM else if(isUnk(x)) UNK else wordIndexer.getIndex(x) )
		foreach(depth-1, (i:Int) => {
			sent = { if(hist.length-i-1 >=0 ) hist(hist.length-i-1) else START } :: sent
		})
		if(isLast) sent = sent ::: List[Int](STOP)
		val sentence = sent.toArray
		
		//--Get the probability
		var prob:Double = if(logScale) 0.0 else 1.0
		foreach(depth-1, sentence.length, (pos:Int) => {
			//(get the tuple)
			var tuple:List[Int] = List[Int]()
			foreach(pos-(depth-1), pos, (i:Int) => {
				tuple = sentence(i) :: tuple
			})
			tuple = tuple.reverse
			//(get the word)
			val word:Int = sentence(pos)
			//(get the prob)
			val wordProb = getCondProb(word, tuple)
			if(!opts.allowZeroProbSentence && wordProb == 0){
				fail("Word has zero probability in context: " 
					+ wordIndexer.get(sent(pos)) + " | " + tuple.map(wordIndexer.get(_)))
			}
			if(logScale)
				prob += Math.log(wordProb)
			else
				prob *= wordProb
		})

		//--Return
		return prob
	}

	def generate : List[String] = generate(true)
	def generate(sample:Boolean) : List[String] = {
		//--Initialize the sentence
		var sentence:List[String] = List[String]()
		var tail:List[Int] = List[Int]()
		foreach(depth-1, (i:Int) => tail = START :: tail )
		
		//--Generate words
		var nextWord:Int = -1
		while(nextWord != STOP){
			//(sample)
			val tailInt:Int = tupleIndexers(depth-1).getIndex(tail)
			val dist:ProbVec = grams(depth-1)(tailInt)
			nextWord = if(sample) dist.sample(rand) else dist.maxPair._1
			sentence = wstr(nextWord) :: sentence
			//(update the tail)
			tail = tail ::: List[Int](nextWord)
			tail = tail.tail
		}
		sentence = sentence.tail.reverse
		
		//--Return
		sentence
	}

	def toLocalTail(history:List[Int], internDict:Indexer[String]) : List[Int] = {
		toLocalTail(history, internDict, toLocalWord(_,internDict))
	}
	def toLocalTail(history:List[Int], internDict:Indexer[String], toLocalWord:((Int)=>Int)) : List[Int] = {
		var tail = history
		while(tail.size >= depth) {
			tail = tail.tail
		}
		tail = tail.map( toLocalWord(_) )
		while(tail.size < depth-1) {
			tail = START :: tail
		}
		tail
	}
	def toLocalTail(history:List[String]):List[Int] = {
		var tail:List[String] = history
		while(tail.size >= depth) {
			tail = tail.tail
		}
		var locTail:List[Int] = tail.map( (s:String) => toLocalWord(s) )
		while(locTail.size < depth-1) {
			locTail = START :: locTail
		}
		locTail
	}
	def toLocalWord(word:String):Int = {
		if(isInt(word)) NUM
		else if(isUnk(word)) UNK
		else wordIndexer.indexOf(word)
	}
	def toLocalWord(forWord:Int, internDict:Indexer[String]):Int = {
		val word:String = internDict.getObject(forWord)
		toLocalWord(word)
	}
	
	def nextWordProb(word:String, history:List[String]) = {
		val tail = toLocalTail(history)
		val locWord = toLocalWord(word)
		getCondProb(locWord, tail)
	}

	def nextWordProb(word:Int, history:List[Int], internDict:Indexer[String]) = {
		val tail = toLocalTail(history, internDict)
		val locWord = toLocalWord(word, internDict)
		getCondProb(locWord, tail)
	}
	
	def nextWordProb(word:Int, history:List[Int], internDict:Indexer[String], numTerm:Int) = {
		def loc(w:Int) : Int = {
			if(w == numTerm) NUM
			else toLocalWord(w, internDict)
		}
		val tail = toLocalTail(history, internDict, loc(_))
		val locWord = loc(word)
		getCondProb(locWord, tail)
	}

	def nextNumProb(history:List[Int], internDict:Indexer[String]) = {
		val tail = toLocalTail(history, internDict)
		val locWord = NUM
		getCondProb(locWord, tail)
	}

	def nextWordDist(history:List[Int], prior:ProbVec, internDict:Indexer[String]) : ProbVec = {
		//(set up return vector)
		import ProbVec.{zeros}
		val rtn:ProbVec = zeros(prior.size)
		//(set up tail)
		val tail = toLocalTail(history, internDict)
		//(populate the probvec)
		foreach(prior.size, (forWord:Int) => {
			val locWord = toLocalWord(forWord, internDict)
			rtn.addCount_!(forWord, (prior.getProb(forWord) * getCondProb(locWord, tail)) )
		})
		rtn
	}
	
	def nextWordDist(history:List[Int], internDict:Indexer[String]) : ProbVec = {
		val uniformPrior = ProbVec.zeros(internDict.size)
		uniformPrior.set_!(1.0)
		nextWordDist(history, uniformPrior, internDict)
	}

	def nextWordDist(history:List[Int]) : ProbVec = {
		val rtn:ProbVec = ProbVec.zeros(N)
		//(set up tail)
		val tail = toLocalTail(history, wordIndexer)
		//(populate the probvec)
		foreach(N, (w:Int) => {
			rtn.addCount_!(w, getCondProb(w, tail))
		})
		rtn	
	}
	
	def stopProb(history:List[Int], internDict:Indexer[String]):Double = {
		//(set up tail)
		val tail = toLocalTail(history, internDict)
		//(calculate prob)
		getCondProb(STOP, tail)
	}

	def stopProb(history:List[String]):Double = {
		val tail = toLocalTail(history)
		getCondProb(STOP,tail)
	}

	def interactive = {
		def readLine:String = {
			print("lm> ") 
			val rtn = (new java.io.BufferedReader(new java.io.InputStreamReader(System.in))).readLine()
			rtn
		}
		def lineToList(input:String):List[Int] = {
			def tokensToList(tokens:java.util.StringTokenizer):List[String] = {
				if(!tokens.hasMoreTokens) List[String]()
				else tokens.nextToken :: tokensToList(tokens)
			}
			tokensToList(new java.util.StringTokenizer(input)).map( (str:String) => {
				if(isInt(str)) NUM
				else if(isUnk(str)) UNK
				else wordIndexer.indexOf(str)
			})
		}

		println( "Interactive Language Model" )
		var input:String = readLine
		while(!input.equalsIgnoreCase("exit")){
			val words:List[Int] = lineToList(input)
			//(print digested sentence)
			print("   Sentence: ")
			words.foreach( (w:Int) => {print(wstr(w) + " ")} )
			println("")
			//(sentence prob)
			println("      prob:\t" + getSentenceProb( (words ::: List(STOP)).toArray ))
			//(conditional prob)
			val history:List[Int] = words.init
			val word:Int = words.last
			val dist:ProbVec = nextWordDist(history)
			var seen:Boolean = false
			var countUsed:Double = 0.0;
			val df = new java.text.DecimalFormat ("0.0000");
			print("      P( w | ")
			history.foreach( (w:Int) => {print(wstr(w) + " ")} )
			println(")")
			foreach(5, (i:Int) => {
				print("        ")
				val (argmax, m):(Int, Double) = dist.maxPair
				val max = dist.getCount(argmax) / (dist.totalCount+countUsed)
				dist.setCount_!(argmax, 0.0)
				countUsed = countUsed + max
				if(argmax == word){
					seen = true
					print(">")
				}else{
					print(" ")
				}
				println( df.format(max) + "\t" + wstr(argmax) )
			})
			if(!seen) println("        >" + df.format(dist.getCount(word)/(dist.totalCount+countUsed)) + "\t" + wstr(word) )

			input = readLine
		}


	}
	
	def bleu(golds:java.util.List[java.util.List[String]]) : Double = {
		val bleuScorer = new cortex.BleuScorer
		def bleuScore(
			test:java.util.List[String],
			gold:java.util.List[String]     ) = {
			val goldSet = new java.util.LinkedList[java.util.List[String]]()
			goldSet.addLast(gold)
			var score:cortex.Score = bleuScorer.evaluateBleuSentence(test,goldSet)
			score.getScore
		}
		def toJavaList(lst:List[String]):java.util.List[String] = {
			val jlst = new java.util.LinkedList[String]()
			lst.foreach( (s:String) => {
				jlst.addLast(s)
			})
			jlst
		}
		
		var sumBleu:Double = 0.0;
		var count:Double = 0.0;
		val iter = golds.iterator
		while( iter.hasNext ) {
			val gold = iter.next
			val gen = generate
			sumBleu += bleuScore( toJavaList(gen), gold )
			count += 1
		}

		sumBleu / count
	}
	
	def assertIsOK = {
		//--Trigram Sentence Prob Check
		var prob:Double = 0.0
//		foreach(N, (w1:Int) =>
//			foreach(N, (w2:Int) =>
//				foreach(N, (w3:Int) => {
//					val sent = new Array[Int](3)
//					sent(0) = w1; sent(1) = w2; sent(2) = w3;
//					//println(sent.map((x:Int) => wstr(x)))
//					prob += getSentenceProb(sent)
//				})))
//		if(Math.abs(1.0-prob) > 0.001) error("Sentences did not sum to 1.0: " + prob)
		
		//--Unigram check
		prob = 0
		foreach(N, (word:Int) => {
			val tail = List[Int]()
			prob += getCondProb(word, tail)
		})
		if(Math.abs(1.0-prob) > 0.001) error("Sentences did not sum to 1.0: " + prob)
		
		//--Bigram check
		foreach(N, (w1:Int) => {
			prob = 0
			foreach(N, (word:Int) => {
				val tail = List[Int](w1)
				prob += getCondProb(word, tail)
			})
			if(Math.abs(1.0-prob) > 0.001) error("Sentences did not sum to 1.0: " + prob)
		})

		
		//--Trigram check
		foreach(N, (w1:Int) => {
			foreach(N, (w2:Int) => {
				prob = 0
				foreach(N, (word:Int) => {
					val tail = List[Int](w1, w2)
					prob += getCondProb(word, tail)
				})
				if(Math.abs(1.0-prob) > 0.001) error("Sentences did not sum to 1.0: " + prob)
			})
		})
		
	}

}

