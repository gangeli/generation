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

import induction.ProbVec

class KneserNeyLM(opts:induction.Options, depth:Int, wordIndexer:Indexer[String]) extends LanguageModel(opts, depth, wordIndexer){
	
	def delta(count:Double):Double = {
		if(count < 1)		0
		else if(count < 2)	0.5
		else 			0.75
		//0
	}
	
	def alpha(probs:ProbVec):Double = {
		if(probs == EMPTY_VEC) {
			//(case: completely new context: all weight on lower order model)
			1.0
		} else {
			//(case: seen context before: calculate alpha)
			var sumDelta:Double = 0.0
			foreach(N, (word:Int) => sumDelta += delta(probs.getCount(word)) )
			sumDelta / probs.totalCount
		}
	}

	val alphaCache = new java.util.HashMap[List[Int], Double]

	override def getCondProb(word:Int, tail:List[Int]) : Double = {
		if(tail.size == 0){
			//--Base Case (knesser-ney)
			uniqueContexts.getProb(word)
		} else {
			//--Recursive Case (absolute discount)
			//(get the ProbVec)
			val probs:ProbVec = getProbVec(tail)
			
			//(get Prob if exists)
			var discounted:Double = 0.0;
			if(probs != EMPTY_VEC && probs.getCount(word) > 0) {
				//(case: seen this before)
				val c:Double = probs.getCount(word)
				val d:Double = delta(c)
				discounted = (c-d) / probs.totalCount
			} else {
				//(case: never seen this before)
				discounted = 0.0
			}
			
			//(get alpha)
			val a:Double = {
				if(alphaCache.containsKey(tail)){
					alphaCache.get(tail)
				}else{
					val rtn = alpha(probs)
					alphaCache.put(tail, rtn)
					rtn
				}
			}
			
			//(drop to lower order case)
			val rtn:Double = discounted + a * getCondProb(word, tail.tail)
			if(rtn == 0) throw new IllegalStateException("0 Probability for sequence " + tail.map(wstr(_)) + " -> " + wstr(word))
			else rtn
		}
	}
	
}




