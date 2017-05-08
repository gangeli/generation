package generation

import scala.collection.mutable.HashMap
import java.util.Random

import fig.basic.{IOUtils,Indexer,StatFig}
import fig.exec.Execution
import fig.exec.Execution.putLogRec
import fig.record.Record
import fig.basic.LogInfo.{logs,dbg,fail,fails,error,track,end_track}

import tea._
import tea.Utils.{map,foreach,fmt,fmt1,returnFirst,assertValid}

class Event3LM(opts:induction.Options, depth:Int, wordIndexer:Indexer[String]) extends KneserNeyLM(opts, depth, wordIndexer){
	
	override def readData(path:String, maxExamples:Int) : Int = {
//		var read = 0;
//		var str:String = ""
//		Utils.foreachLine(path, maxExamples, { line:String =>
//			str += " " + line.toLowerCase
//		})
//		examples = lineToExample(line) :: examples
//			read += 1
//		read
		
		
		var rtn = 0;
		val in = IOUtils.openInEasy(path)
		if(in != null){
			//--Get Text from File
			var done = false
			var str:String = ""
			while(!done){
				val line = in.readLine()
				done = (line == null)
				if(!done) str += " " + line.toLowerCase	//to lower case
			}
			//--Split Text into Entities
			//val tokens = str.split("(\\s+)\\.(\\s+)")
			//tokens.foreach( (token:String) => {
			//	examples = lineToExample(token) :: examples
			//	rtn += 1
			//	true
			//})
			examples = lineToExample(str) :: examples
		}
		in.close
		rtn
	}
	
}
