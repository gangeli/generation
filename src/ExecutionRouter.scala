

import org.goobs.utils.ConfigFile
import java.io._
import induction._
import generation._
import scala.collection.mutable.HashMap

object ExecutionRunner{
	
	def findConfig(args:Array[String]):ConfigFile = {
		//--Get the filename
		var last:String = ""
		var filename:String = null
		args.foreach( (term:String) => {
			if( (last equalsIgnoreCase "-config") || (last equalsIgnoreCase "--config") ){
				filename = term
			}
			last = term
		})
		//--Create the config file
		if(filename == null) {
			println("No configuration file provided")
			return null
		}
		val config:ConfigFile = new ConfigFile(filename)
		try{
			config.read
		} catch {
			case (e:IOException) => {
				println("ERROR: Could not load configuration file: " + filename)
				println("   caused by: " + {if(e.getMessage == null) "IOException" else e.getMessage} )
				System.exit(1);
			}
			case (e:Exception) => {
				println("UNKNOWN ERROR: " + e.getClass.getName + ": " + e.getMessage)
				System.exit(2);
			}
		}
		return config
	}

	/*
	def removeScripts(config:ConfigFile):HashMap[String,CompiledScript] = {
		//--Variables
		val store:HashMap[String,CompiledScript] = new HashMap[String,CompiledScript]
		val manager:ScriptEngineManager = new ScriptEngineManager();
		val engine:ScriptEngine = manager.getEngineByName("js");
		val compilingEngine:Compilable = engine.asInstanceOf[Compilable];
		//--Add a script if it exists
		def add(str:String):Unit = {
			if(!config.contains(str)) return;
			val value:String = config.remove(str).toString
			if( value equalsIgnoreCase "false" ){
				store += str -> null
			}else{
				val f:File = new File(value)
				//(get the script contents)
				val script:String =
					"""
					importPackage(org.goobs.foreign)
					importPackage(org.goobs.classify)
					importPackage(org.goobs.choices)
					importPackage(Packages.generation)
					""" + {
						if( f.exists ){
							//(load the script from a file)
							val br:BufferedReader = new BufferedReader(new FileReader(f))
							var line:String = br.readLine
							val b:StringBuilder = new StringBuilder()
							while( line != null ){
								b.append(line).append("\n")
								line=br.readLine
							}
							b.toString
						} else {
							//(use the value in the config file as is)
							value
						}
					} + """
						if(_type == "input") inputFeatures();
						else if(_type == "ouptut") outputFeatures();
						else if(_type == "joint") jointFeatures();
						else throw "Invalid Type";
						""" //append some routing code
				val compiled:CompiledScript = 
					{try { compilingEngine.compile(script) }  //try compiling the script
					 catch{ case (e:Exception) => {
						//something failed in compilation; abort
						println("Invalid Script for " + str + "\n\tCause: " + e.getMessage)
						println("\nSCRIPT:\n--------------")
						println(script)
						System.exit(1)
						null
					}}}
				store += str -> compiled
			}
		}
		//--Get specific scripts
		add("featEvent")
		add("featField")
		add("featWord")
		add("featFS")
		add("featTemplate")
		store
	}
	*/

	def main(rawArgs:Array[String]) = {
		val configFile:ConfigFile = findConfig(rawArgs)
		val args:Array[String] = if(configFile==null) rawArgs else Array.concat(configFile.toOptions, rawArgs)
		/*
		args.foreach( (str:String) => {
			if(str startsWith "-") println()
			print(str)
			print(" ")
		})
		println()
		*/


		val opts:induction.Options = new Options
		fig.exec.Execution.run(
			args, 
			new Runnable(){
				override def run = {	
					val start = opts.testStart; opts.testStart = opts.trainEnd
					val stop = opts.testEnd; opts.testEnd = opts.trainEnd
					//(run alignment if applicable)	
					if(!configFile.getBoolean("loadAligns")){
						val alignRunner = new AlignmentRunner
						alignRunner.opts = opts
						alignRunner.run
					}
					//(signal that we're done saving)
					opts.loadAligns = true
					opts.loadParams = true
					opts.testStart = start
					opts.testEnd= stop
					//(run generation)
					val genRunner = new GenerationRunner
					genRunner.opts = opts
					genRunner.run
				}},
			opts
			)
	}
}



