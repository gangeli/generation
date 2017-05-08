package generation

import java.util.HashMap
import java.io._

import tea.Utils.{map,foreach,fails}
import fig.basic.LogInfo.{logs,dbg,fail,error,track,end_track}
import induction.Event3Problem
import AlignedDataset.AlignedSentence
import org.goobs.internet.WebServer
import org.goobs.internet.WebServerHandler

class Evaluator(opts:EvalOptions) {
	
	object Type{
		val GOLD:Int = 0
		val GUESS:Int = 1
		val REF:Int = 2
	}

	case class Example(ws:WorldState, sent:Array[String], test:Boolean, tp:Int){
		var englishFluency:Double = 0;
		var semanticCorrectness:Double = 0;
		def toHtml:String = {
			var dat:String = ""
			try{
				dat += ws.toHtml
				if(sent == null) dat += "<p><h3> NO GENERATION </h3></p>"
				else dat += "<p><h3>" + GenUtils.arrayToString(sent.map( (s:String) => if(s.equals("[[UNK]]")) "nne" else s)) + "</h3></p>"
			} catch {
				case (e:Exception) => { e.printStackTrace(); dat = "<h3> RENDERING ERROR </h3>" }
			}
			dat
		}
		override def toString:String = {
			GenUtils.arrayToString(sent) + "\n\t" + 
			ws.toString.replaceAll("\\\n","\n\t") + 
			"(fluency=" + englishFluency + ", correctness=" + semanticCorrectness + ")\n"
		}
	}

	def evaluate:Unit = {
		def loadAlignments(path:String):AlignedDataset = {
			val objStream:ObjectInputStream = new ObjectInputStream(new FileInputStream(path))
			val rtn = objStream.readObject().asInstanceOf[AlignedDataset]
			objStream.close()
			rtn
		}
		//(load parameters)
		def createProblem:Event3Problem = {
			val opts:induction.Options = new induction.Options
			opts.inputFileExt = "events"
			opts.inputFormat = induction.Options.InputFormat.raw
			opts.inputPaths = this.opts.inputPaths
			val problem = new Event3Problem(opts)
			val model = problem.newModel	
			//(read in the relevant data)
			opts.maxExamples = java.lang.Integer.MAX_VALUE	//ensure correct T, F, etc values
			model.readExamples
			//(return)
			problem
		}
		def parseResult(ex:Example, values:HashMap[String,String]):Boolean = {
			try{
				ex.englishFluency = java.lang.Integer.parseInt(values.get("fluency"))
				ex.semanticCorrectness = java.lang.Integer.parseInt(values.get("correctness"))
			} catch {
				case (e:Exception) => return false
			}
			true
		}
		def dumpResults(examples:Array[Example]):Unit = {
			println("Dumping results...")
			try {
			
			//--Get Example types
			val trainGuess:Array[Example] = examples.filter( (ex:Example) => (!ex.test && ex.tp==Type.GUESS) )
			val trainGold:Array[Example] = examples.filter( (ex:Example) => (!ex.test && ex.tp==Type.GOLD) )
			val testGuess:Array[Example] = examples.filter( (ex:Example) => (ex.test && ex.tp==Type.GUESS) )
			val testGold:Array[Example] = examples.filter( (ex:Example) => (ex.test && ex.tp==Type.GOLD) )
			val testRef:Array[Example] = examples.filter( (ex:Example) => (ex.test && ex.tp==Type.REF) )
			//--Get output file
			val file:File = new File(fig.exec.Execution.getFile("results"))
			if(!file.exists) file.createNewFile
			val writer = new FileWriter(file)
			//--Log Results
			def log(s:String) = { writer.append(s).append("\n"); writer.flush; println(s) }
			log("---AVERAGES---")
			log("Training Data")
			log("\tGuess Fluency: " 
				+ (trainGuess.foldRight(0.0)((ex:Example,i:Double) => { i+ex.englishFluency }) 
					/ trainGuess.length.asInstanceOf[Double]) )
			log("\tGuess Correctness: " 
				+ (trainGuess.foldRight(0.0)((ex:Example,i:Double) => { i+ex.semanticCorrectness }) 
					/ trainGuess.length.asInstanceOf[Double]) )
			log("\tGold Fluency: " 
				+ (trainGold.foldRight(0.0)((ex:Example,i:Double) => { i+ex.englishFluency }) 
					/ trainGold.length.asInstanceOf[Double]) )
			log("\tGold Correctness: " 
				+ (trainGold.foldRight(0.0)((ex:Example,i:Double) => { i+ex.semanticCorrectness }) 
					/ trainGold.length.asInstanceOf[Double]) )
			log("Test Data")
			log("\tGuess Fluency: " 
				+ (testGuess.foldRight(0.0)((ex:Example,i:Double) => { i+ex.englishFluency }) 
					/ testGuess.length.asInstanceOf[Double]) )
			log("\tGuess Correctness: " 
				+ (testGuess.foldRight(0.0)((ex:Example,i:Double) => { i+ex.semanticCorrectness }) 
					/ testGuess.length.asInstanceOf[Double]) )
			log("\tGold Fluency: " 
				+ (testGold.foldRight(0.0)((ex:Example,i:Double) => { i+ex.englishFluency }) 
					/ testGold.length.asInstanceOf[Double]) )
			log("\tGold Correctness: " 
				+ (testGold.foldRight(0.0)((ex:Example,i:Double) => { i+ex.semanticCorrectness }) 
					/ testGold.length.asInstanceOf[Double]) )
			log("\tRef Fluency: " 
				+ (testRef.foldRight(0.0)((ex:Example,i:Double) => { i+ex.englishFluency }) 
					/ testRef.length.asInstanceOf[Double]) )
			log("\tRef Correctness: " 
				+ (testRef.foldRight(0.0)((ex:Example,i:Double) => { i+ex.semanticCorrectness }) 
					/ testRef.length.asInstanceOf[Double]) )
			log("")
			log("---RAW DATA---")
			log("Train Guess: \n" + GenUtils.arrayToString(trainGuess))
			log("Train Gold:  \n" + GenUtils.arrayToString(trainGold))
			log("Test Guess:  \n" + GenUtils.arrayToString(testGuess))
			log("Test Gold:   \n" + GenUtils.arrayToString(testGold))
			log("Test Ref:    \n" + GenUtils.arrayToString(testRef))
			
			} catch {
				case (e:Exception) => { e.printStackTrace; System.exit(1); }
			}
		}
		//(GenUtils)
		GenUtils.fillGenUtils(createProblem)
		
		//--Load Alignments
		track("Loading alignments")
		val train = loadAlignments(opts.trainPath)
		val test = loadAlignments(opts.testPath)
		end_track

		//--Reference
		track("Reading references")
		//(read reference)
		val refPairs:Array[(Array[String],Array[String])] = if(opts.refPath == null) null else {
			var refLst = List[(Array[String],Array[String])]()
			val reader = new BufferedReader(new FileReader(new File(opts.refPath)))
			var line:String = null
			var i = 0;
			try{
			line = reader.readLine
			while( line != null){
				val parts:Array[String] = line.toLowerCase.split("=>")
				refLst = (	parts(0).split(" ").toList.filter( (s:String) => !s.trim.equals("") ).toArray, 
							parts(1).split(" ").toList.filter( (s:String) => !s.trim.equals("") ).toArray  ) :: refLst
				line = reader.readLine
			}
			} catch {
				case (e:Exception) => {e.printStackTrace(); println("ERROR reading references: " + e.getMessage); System.exit(1); }
			}
			refLst.reverse.toArray
		}
		//(align references)
		var refs = new Array[Array[String]](test.trueWords.length)
		foreach(test.trueWords.length, (i:Int) => {
			if(opts.useRefs){
			try{
			val gold:Array[String] = test.trueWords(i)
			val refCands = refPairs.filter( (pair:(Array[String],Array[String])) => pair._1.deepEquals(gold) ) 
			if(refCands.length == 0) {
				println("warning: No translation for " + GenUtils.arrayToString(gold))
				refs(i) == null
			}
			else if(refCands.length > 1) {
				val a = refCands(0)._2
				refCands.foreach( (pair:(Array[String],Array[String])) => {
					if(!pair._2.deepEquals(a))
						throw new IllegalArgumentException("Multiple distinct generatons match: " + GenUtils.arrayToString(gold))
				})
				refs(i) = refCands(0)._2
			} else{
				refs(i) = refCands(0)._2
			}
			} catch {
				case (e:Exception) => refs(i) = null
			}
			}
		})
		end_track


		//--Make Examples
		track("Creating examples")
		var exList = List[Example]()
		if(!opts.useRefs) refs = null
		//(training examples)
		foreach(opts.trainStart, opts.trainStart+opts.trainNum, (i:Int) => {
			exList = new Example(	train.worldStates(i), 
									train.aligns(i).getWords, 
									false,
									Type.GUESS	) :: exList
			exList = new Example(	train.worldStates(i), 
									train.trueWords(i), 
									false,
									Type.GOLD	) :: exList
		})
		//(test examples)
		var i = opts.testStart
		var num = 0
		while(num < opts.testNum) {
			if(!opts.useRefs || (opts.useRefs && refs(i) != null)){
			if(test.aligns(i).getWords != null && test.trueWords(i) != null){
				/*
				println("---------------")
				println(test.worldStates(i))
				println(GenUtils.arrayToString(test.trueWords(i)))
				println(GenUtils.arrayToString(test.aligns(i).getWords))
				println(GenUtils.arrayToString(refs(i)))
				println()
				*/
				//guess
				exList = new Example(	test.worldStates(i), 
										test.aligns(i).getWords, 
										true,
											Type.GUESS	) :: exList
				//gold
				exList = new Example(	test.worldStates(i), 
										test.trueWords(i), 
										true,
										Type.GOLD	) :: exList
				//ref
				if(refs != null){
					exList = new Example(	test.worldStates(i),
											refs(i),
											true,
											Type.REF	) :: exList
				}
				num += 1
			}
			}
			i += 1
		}
		//(shuffle)
		val examples = GenUtils.shuffle(exList).toArray
		end_track

		//--Start Server
		val server:WebServer = new WebServer(opts.port);
		server.start
		//(start screen)
		server.register("/", new WebServerHandler(){
			override def handle(values:HashMap[String,String], info:WebServer.HttpInfo):String = {
				val reader:BufferedReader = new BufferedReader(new FileReader(opts.greetPath))
				var greet:String = ""
				try{
					var line:String = reader.readLine
					while(line != null){
						greet += line
						line = reader.readLine
					}
				} catch {
					case (e:Exception) => println("ERROR")
				}
				return greet
			}
		})
		//(evaluation screen)
		server.register("/run", new WebServerHandler(){
			override def handle(values:HashMap[String,String], info:WebServer.HttpInfo):String = {
				//--Process Values
				//(get id)
				var id:Int = -1
				try{
					id = java.lang.Integer.parseInt(values.get("id"))
				} catch { case (e:Exception) => id = java.lang.Integer.MAX_VALUE }
				if(id > examples.length) return """<center><h1><font color="red">Invalid Url!</font></h1></center>"""
				//(process last result)
				val good:Boolean = if(id == examples.length){
						if( parseResult(examples(id-1),values) ) {
							dumpResults(examples)
							return """<center><h1>Thank you!</h1><br />You have completed all the examples</center>"""
						}else{
							false
						}
					} else if(id > 0) {
						parseResult(examples(id-1), values)
					} else {
						true
					}
				if(!good) id = id - 1
				//--Render HTML
				val dataHtml = examples(id).toHtml
				val html = """
					<html>
					<head>
						<link href=""" + "\"" + opts.cssPath + "\"" + """ rel="stylesheet" type="text/css">
					</head>""" +
					"""<body>""" + 
						"""<center><h2>Example """ + (id+1) + """ of """ + examples.size + """</h2></center>""" + 
						//error code
						{ 	if(good) "" 
							else """<center><h3><font color="red">Error processing last result!</font></h3></center>""" } +
						//add world state and sentence
						"""<center>""" + dataHtml + """</center>""" + 
						//add final form
						getForm(id) +
					"""</body>
					</html>"""
				return html
			}
		})
		
		startConsole
	}

	def startConsole:Boolean = {
		import org.goobs.io._
		val console:Console = new TextConsole
		console.show
		while(true){
			val cmd:String = console.readLine("cmd> ")
			if(cmd.equalsIgnoreCase("exit")) return true
		}
		return false
	}


val ROBO_GREETING:String =
"""
<html>
<head></head>
<body>
	<center><h1> Evaluation Interface </h1></center>
	<p> Evaluation is split into two categories: "English Fluency" and "Semantic Correctness."
		The first judges how much the generated sentence resembles common English (within the
		particular dialect of the dataset), while the second judges how well the sentence corresponds
		to the underlying facts about the world. </p>
	<p> Note that the English fluency score should not be penalized for capitalization, or breaking
		of suffixes (e.g. "pink11 's") into multiple words. </p>
	<p> Examples of rankings for each of the categories is given below. Note that these
		are meant to give a sense for what sorts of mistakes lie in particular ranges, and not
		necessarily a strict rubric)</p>
	English Fluency: <br />
	<ul>
		<li> Flawless - pink9 passes to purple8 </li>
		<li> Good - pink9 defended purple8 shot </li>
		<li> Non-Native - pink9 pass purple8 </li>
		<li> Disfluent - pink9 kicks to a bad pass by pink10 </li>
		<li> Gibberish - pink9 pink9 to a purple8 kick </li>
	</ul>
	Semantic Correctness: <br />
	<ul>
		<li> Perfect - pink9 passes back to pink8 (if pass: pink9 purple8 present, EVEN IF we don't know that pink9 passed back, since the world state might extend far enough back)</li>
		<li> Near Perfect - pink9 passes back to pink8 (ONLY IF we know for sure that pink9 didn't get the ball from pink8) </li>
		<li> Minor Errors - pink9 passes to pink10 (but actually passed to pink8: one argument wrong) </li>
		<li> Major Errors - purple10 passes to purple3 (but actually, no such pass happened: both arguments wrong) </li>
		<li> Wrong - purple1 shoots for the goal (neither purple1 nor a kick action appear) </li>
	</ul>
	<p> <center><h3> <a href="run/?id=0"> click here </a> to start </h3> </center></p>
</body>
</html>
"""


def getForm(id:Int):String = {
"""<form>
		<input type="hidden" name="id" value=""" + "\"" + (id+1) + "\"" + """>
		<h3>English Fluency:</h3>
		<input type="radio" name="fluency" value="5" /> Flawless
		<input type="radio" name="fluency" value="4" /> Good
		<input type="radio" name="fluency" value="3" /> Non-native
		<input type="radio" name="fluency" value="2" /> Disfluent
		<input type="radio" name="fluency" value="1" /> Gibberish
		<br />
		<h3>Semantic Correctness:</h3>
		<input type="radio" name="correctness" value="5" /> Perfect
		<input type="radio" name="correctness" value="4" /> Near Perfect
		<input type="radio" name="correctness" value="3" /> Minor Errors
		<input type="radio" name="correctness" value="2" /> Major Errors
		<input type="radio" name="correctness" value="1" /> Wrong
		<br /> &nbsp <br />
		<input type="submit" name="submit" value="Submit" />
	</form>"""
}


}

