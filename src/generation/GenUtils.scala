package generation

import induction._

import tea.Utils.{map,foreach,fails}

import fig.basic.LogInfo.{logs,dbg,fail,error,track,end_track}
import fig.record.Record
import fig.basic.Indexer

import org.goobs.foreign.Counter
import org.goobs.utils.StatCalc
import java.io._
import java.util.HashMap

object GenUtils {
	case class GenerationOptions(opts:Options){
		var optimalSearch:Boolean = false
		var k = 0
		var printWorldState:Boolean = false
		var lowerBound:Boolean = false
		var quiet:Boolean = false
		var maxLength:Int = 25
		var printTrueAlignment = false;
	}

	var none_t = -1;
	var none_f:Int=>Int = null;
	var none_w = -1;
	
	var dead_t = -2;
	var dead_f:Int=>Int = null;
	var dead_w = -2;
	var dead_e:WorldState=>Int = null;

	var none_e:WorldState=>Int = null;
	var fail_e = -3;

	var start_t = -4;
	var start_f = -4;
	var start_w = -4;

	var wstr:Int=>String = null
	def wStr(w:Int) = wstr(w)
	var fstr:Int=>Int=>String = null
	def fstr(t:Int,f:Int):String = fstr(t)(f)
	def fStr(t:Int,f:Int):String = fstr(t)(f)
	var tstr:Int=>String = null
	def tStr(t:Int) = tstr(t)
	var estr:(Int,WorldState)=>String = null
	def eStr(e:Int,ws:WorldState) = estr(e,ws)
	var vstr:(Int,Int,Int)=>String = null
	def vStr(t:Int,f:Int,v:Int) = vstr(t,f,v)
	var fval:(Int,String)=>Int = null
	
	var T = -42 
	var F:Int=>Int = null
	var W = -42
	var V:(Int,Int)=>Int = null
	
	def getF(t:Int):Int = F(t)

	val STOP_INT = -123
	val STOP_STR_ARRAY = Array[String]("[[the]]","[[stop]]","[[term]]")
	val STOP_INT_ARRAY = Array(1,2,3)
	val STOP_TEMPLATE = new Template(-1337, Array(-10), null)
	val STOP_FS = new FieldSet(-1338, new scala.collection.mutable.HashSet[Int])

	var allEmissions : HashMap[(Int,Int), HashMap[Int,Counter[String]]] 
		= new HashMap[(Int,Int), HashMap[Int,Counter[String]]]()
	var emissions:(Int,Int,Int)=>Counter[String] = null //(t,f,v)=>String Distribution

	var lmStats:StatCalc = null;

	var testProblem:Event3Problem = null;
	
	def fillGenUtils(problem:Event3Problem){
		//--Set limits
		T = problem.T
		F = (t:Int) => if(t==problem.eventTypes.size) 0 else problem.eventTypes(t).F
		W = problem.W

		//--Set special values
		none_t = T
		none_f = (t:Int) => F(t)
		none_w = W
		dead_t = T+1
		dead_f = (t:Int) => F(t)+1
		dead_w = W+1
		dead_e = (ws:WorldState) => ws.size+1
		none_e = (ws:WorldState) => ws.size
		fail_e = -1
		start_t = -5
		start_f = -5
		start_w = -5
		

		//--Set Strings
		//(wstr)
		wstr = (w:Int) => { 
			if(w == dead_w) "---" 
			else if(w == none_w) "(none)" 
			else if(w>=W) throw new IllegalStateException("Not a valid word: " + w + " (W=" + W + ")")
			else if(w==start_w) "[[START]]" 
			else if(w==STOP_INT) "[[STOP]]"
			else problem.wstr(w) };
		//(fstr)
		fstr = (t:Int) => (f:Int) => { 
			if(f == start_f) "[[START]]"
			else if(f==dead_f(t)) "---" 
			else if(f==none_f(t)) "(none)"
			else if(t==none_t || t==start_t) "[[BAD_T]]" 
			else if(f==STOP_INT) "[[STOP]]"
			else {
				try{
					val rtn = problem.eventTypes(t).fstr(f) 
					if(rtn.equals("")) throw new IllegalStateException("Returning empty string")
					rtn
				} catch {
					case (e:Exception) => {
						throw new java.lang.IllegalArgumentException("Invalid field to convert to string: " + f + " (F=" + F(t) + ")")
					}
				}
			}
			};
		//(fval)
		fval = (t:Int,str:String) => {
			def rec(f:Int):Int = {
				if(f >= F(t)) dead_f(t)
				else if(str equalsIgnoreCase fstr(t,f)) f
				else rec(f+1)
			}
			rec(0)
		}
		//(tstr)
		tstr = (t:Int) => 
			{ if(t==dead_t) "---" 
				else if(t == start_t) "[[START]]"
				else if(t == none_t) "(none)"
				else if(t == dead_t) "(dead)"
				else if(t == STOP_INT) "[[STOP]]"
				else if(t > problem.T) throw new java.lang.IllegalArgumentException("t>T: t=" + t + " T=" + problem.T)
				else if(t < 0) throw new IllegalArgumentException("t<0: t=" + t)
				else problem.tstr(t) 
			};
		estr = (e:Int,ws:WorldState) => tstr(e2t(e,ws))
		//(vstr)
		vstr = (t:Int,f:Int,v:Int) => {
				if(t == none_t || t == dead_t) "---"
				else if(f == none_f(t) || f == dead_f(t)) "---"
				else {
					try{
						problem.eventTypes(t).fields(f).vstr(v)
					}catch{
						case (e:Exception) => {
							testProblem.eventTypes(t).fields(f).vstr(v)
							//println("ERROR in vstr(): " + e.getMessage); "N" 
						}
					}
				}
			}
		//(V)
		V = (t:Int,f:Int) => {
			if(t == none_t || t == dead_t) 0
			else if(f == none_f(t) || f == dead_f(t)) 0
			else problem.eventTypes(t).fields(f).V
		}

		//(all emissions)
		/*
		foreach(T, (t:Int) => {
			foreach(F(t), (f:Int) => {
				val key = (t,f)
				if(!allEmissions.containsKey(key)){
					allEmissions.put(key, new HashMap[Int,Counter[String]])
				}
				foreach(V(t,f), (v:Int) => {
					allEmissions.get(key).put(v, emissions(t,f,v))
				})
			})
		})
		*/
		
	}

	def e2t(e:Int, ws:WorldState) = {
		if(e < 0 || e==none_e(ws)) none_t else if(e==dead_e(ws)) dead_t else ws.events(e).t
	}

	val stateIndexer = new fig.basic.Indexer[WorldState]
	def uniqueIndex(ws:WorldState) = stateIndexer.getIndex(ws)
	
	case class PrettyPrinter(opts:Options) {
		var newLine:Boolean = true
		var depth:Int = 0
		
		def logRight(str:String, prob:Double) : Unit = {
			//(handle newline)
			if(newLine) foreach(depth, (i:Int) => { 
				print("       ") // adjust for prob terms
				foreach(opts.printCellWidth+ 3, (j:Int) => { print(" ") } ) 
			})
			newLine = false
			//(handle prob)
			val df:java.text.DecimalFormat = new java.text.DecimalFormat("0.000")
			print("(");	print(df.format(prob)); print(")")
			//(handle str)
			val toPrint:String = if(str.length > opts.printCellWidth-1) str.substring(0, opts.printCellWidth-1) else str
			print(toPrint)
			//(handle width normalization)
			foreach(toPrint.length, opts.printCellWidth+3, (i:Int) => {print(" ")})
			depth += 1
		}

		def logRight(str:String):Unit = logRight(str, 1.0)
		
		def backup : Unit = {
			if(depth == 0) throw new java.lang.IllegalStateException("Backing up from depth 0")
			depth -= 1
		}
		
		def newline : Unit = {
			print("\n")
			newLine = true
		}
	}

	def linearCombination[Encode<:Any](a:Counter[Encode], b:Counter[Encode], outerTerm:Int) = {
		val outer:Counter[Encode] = if(outerTerm == 0) a else b
		val inner:Counter[Encode] = if(outerTerm == 0) b else a
		var sum:Double = 0.0

		val iter:java.util.Iterator[Encode] = outer.keySet.iterator
		while(iter.hasNext){
			val key:Encode = iter.next
			sum += outer.getCount(key) * inner.getCount(key)
		}

		sum
	}
	
	def toMarkov[Type<:Any](in:Array[Type]) : Array[Type] = {
		def helper(i:Int, last:Int):List[Type] = {
			if( i >= in.size) List[Type]()
			else if(last >= 0 && in(i)==in(last)) helper(i+1, last)
			else in(i) :: helper(i+1, i)
		}
		def lst:List[Type] = helper(0, -1)
		lst.toArray
	}

	def arrayToString[Type](a:Array[Type]):String = {
		val sb:java.lang.StringBuilder = new java.lang.StringBuilder()
		def helper(i:Int) : Unit = {
			if(i < a.size) {
				sb.append(a(i)).append(" ")
				helper(i+1)
			}
		}
		helper(0)
		return sb.toString
	}
	
	def printArray[Type<:Any](a:Array[Type]) = {
		def helper(i:Int) : Unit = {
			if(i < a.size) {
				print(a(i))
				print(" ")
				helper(i+1)
			}
		}
		helper(0)
		println()
	}

	def sample[Type](counts:Counter[Type]) : Type = {
		counts.normalize
		val terms:java.util.Set[Type] = counts.keySet
		val iter:java.util.Iterator[Type] = terms.iterator
		val rand:Double = java.lang.Math.random
		//(sample loop)
		var rolling:Double = 0.0
		var arg:Type = iter.next
		rolling += counts.getCount(arg)
		while(rolling < rand) {
			arg = iter.next
			rolling += counts.getCount(arg)
		}
		arg
	}

	def sample[Type](c:List[(Type,Double)]) : (Type,Double) = {
		var counts:List[(Type,Double)] = c
		val rand:Double = java.lang.Math.random
		var rolling:Double = 0.0
		var arg:(Type,Double) = counts.head
		while(rolling < rand){
			if(counts.isEmpty){
				throw new java.lang.IllegalArgumentException("Counts sum to less than one: " + rolling)
			}
			arg = counts.head.asInstanceOf[(Type,Double)]
			counts = counts.tail
			rolling += arg._2
		}
		arg
	}

	def infer[Type](c:List[(Type,Double)]) : Type = {
		c.foldLeft((c(0)._1,-1.0)) ( (a:(Type,Double),b:(Type,Double)) => {
			if(a._2 > b._2) a
			else b
		})._1
	}

	def getWeight[Type](obj:Type, c:List[(Type,Double)]):Double = {
		val matches = c.filter( (o:(Type,Double)) => o._1.equals(obj) )
		if(matches.size > 0) matches(0)._2
		else -1.0
	}


	def gen2sent(gen:org.goobs.choices.ChoiceGenerator.Generation[Int], ws:WorldState):(AlignedDataset.AlignedSentence,Double) = {
		val tracks:Array[Int] = new Array[Int](gen.size)
		val events:Array[Int] = new Array[Int](gen.size)
		val eventTypes:Array[Int] = new Array[Int](gen.size)
		val fields:Array[Int] = new Array[Int](gen.size)
		val words:Array[Int] = new Array[Int](gen.size)

		foreach(gen.size, (i:Int) => {
			events(i) = gen.get(0,i).asInstanceOf[Int]
			fields(i) = gen.get(1,i).asInstanceOf[Int]
			words(i) = gen.get(2,i).asInstanceOf[Int]
			eventTypes(i) = e2t(events(i), ws)
		})

		(new AlignedDataset.AlignedSentence(tracks, eventTypes, fields, words, events), gen.getScore)
	}
	
	def flatten[Type](in:Array[Array[Type]]):Array[Type] = {
		var lst = List[Type]()
		in.foreach( (a:Array[Type]) => {
			a.foreach( (t:Type) => {
				lst = t :: lst
			})
		})
		lst.toArray
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

	def shuffle[Type](lst:List[Type]):List[Type] = {
		val array:Array[Type] = lst.toArray
		val rand = new scala.util.Random();
		def swap[T](xs: Array[T], i: Int, j: Int) = {
		      val t = xs(i)
		      xs(i) = xs(j)
		      xs(j) = t
	    }
		def fisherYatesShuffle[T](xs: Array[T]) = {
			for (i <- xs.indices.reverse)
				swap(xs, i, rand.nextInt(i + 1))
		}
		fisherYatesShuffle(array)
		array.toList
	}

	def df(x:Any):String = {
		val decformat:java.text.DecimalFormat = new java.text.DecimalFormat("0.000")
		decformat.format(x)
	}

	def deepIter[Type](array:Array[Array[Type]]):(Unit=>(Int,Int,Type)) = {
		var outer = 0
		var inner = -1
		val rtn:(Unit=>(Int,Int,Type)) = (Unit) => {
			//(find next element)
			inner += 1
			while(outer < array.length && inner >= array(outer).length){
				inner = 0
				outer += 1
			}
			if(outer >= array.length) null
			else (outer, inner, array(outer)(inner))
		}
		rtn
	}

	def intersect[Type](a:Array[Type], b:Array[Type]) = {
		val set = new scala.collection.mutable.HashSet[Type]
		a.foreach( (t:Type) => {
			if(b contains t)
				set += t
		})
		b.foreach( (t:Type) => {
			if(a contains t)
				set += t
		})
		var lst = List[Type]()
		set.foreach( (t:Type) => {
			lst = t :: lst
		})
		if(lst.length > a.length || lst.length > b.length) throw new IllegalStateException("Intersection is larger than parts")
		lst.toArray
	}
	
}
