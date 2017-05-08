package induction

import tea.Utils
import tea.Utils.{map,foreach}
import java.util.Random
import fig.basic.LogInfo.{fail,error}

// Augment a regular array with a sum and old sum
// Used for storing probability vectors allowing for an O(1) update by storing normalization constant
// The whole motivation for storing the sum is that we can update counts and get probability vectors for online EM
// The only reason for storing the oldSum is that for the aggressive online EM update, we need to scale a series
// of updates by the same sum, but updating it affects the sum
case class ProbVec(private val counts:Array[Double], private var sum:Double, private var oldSum:Double) {
  def getProb(i:Int) = counts(i)/sum // Get prob
  def getCount(i:Int) = counts(i)

  def addCount_!(x:Double) = { Utils.add_!(counts, x); sum += counts.length*x; this }
  def addCount_!(i:Int, x:Double) = { counts(i) += x; sum += x; this }
  def addCountKeepNonNegative_!(i:Int, x:Double) = {
    // If adding would make it < 0, just set it to 0
    // This is mostly for numerical precision errors (it shouldn't go too much below 0)
    if (counts(i)+x < 0) { sum -= counts(i); counts(i) = 0 }
    else { counts(i) += x; sum += x }
    this
  }
  // Add a feature vector phi (usually, phi is indicator at some i
  def addCount_!(phi:Array[Double], x:Double) = { Utils.add_!(counts, x, phi); sum += x; this }
  def addCount_!(vec:ProbVec, x:Double) = { Utils.add_!(counts, x, vec.counts); sum += x*vec.sum; this }

  // For the special aggressive online EM update
  def addProb_!(i:Int, x:Double) = addCount_!(i, x*oldSum)
  def addProbKeepNonNegative_!(i:Int, x:Double) = addCountKeepNonNegative_!(i, x*oldSum)
  def addProb_!(phi:Array[Double], x:Double) = addCount_!(phi, x*oldSum)
  // 02/18/09: replaced these
  //def addProb_!(i:Int, x:Double) = { counts(i) += x*oldSum; sum += x*oldSum; this }
  //def addProb_!(phi:Array[Double], x:Double) = { Utils.add_!(counts, x*oldSum, phi); sum += x*oldSum; this }
  def saveSum_! = oldSum = sum

  def setCountToObtainProb_!(i:Int, p:Double) = {
    assert (p < 1)
    val x = (sum-counts(i)) * p / (1-p) - counts(i)
    counts(i) += x
    sum += x
  }

  def getProbs = Utils.div_!(Utils.copy(counts), sum)

  // Usage: call saveSum, normalize, getOldSum
  // Useful for printing out posteriors - get an idea of posterior mass on these rules
  def getOldSum = oldSum

  // For HABA grammar induction experiment {12/02/08}
  def expDigamma_! = {
    if (sum > 0) {
      fig.prob.DirichletUtils.fastExpExpectedLogMut(counts)
      computeSum_! // Don't really need this
    }
    this
  }

  //def normalize_! = { Utils.normalizeOrFail_!(counts); sum = 1; this }
  //def normalize_! = { Utils.normalizeOrSetUniform_!(counts); sum = 1; this } // Doesn't work for arbitrary suff stats
  def normalize_! = {
    if (sum == 0) Utils.set_!(counts, 1.0/counts.length)
    else          Utils.div_!(counts, sum)
    sum = 1
    this
  }
  def normalizeIfTooBig_! = { if (sum > 1e20) normalize_!; this }
  def set_!(f:Int=>Double) = { foreach(counts.length, { i:Int => counts(i) = f(i) }); computeSum_! }
  def set_!(x:Double) = { foreach(counts.length, { i:Int => counts(i) = x }); computeSum_! }
  def div_!(x:Double) = { foreach(counts.length, { i:Int => counts(i) /= x }); computeSum_! }
  def mult_!(x:Double) = {foreach(counts.length, { i:Int => counts(i) *= x }); computeSum_! }
  def sample(random:Random) = {
    val target = random.nextDouble*sum
    var i = -1; var accum = 0.0
    while (accum < target) { i += 1; accum += counts(i) }
    i
  }
  private def computeSum_! = { sum = Utils.sum(counts); this }
  
  
  //--Additions by Gabor Angeli 
  def mult_!(i:Int, x:Double) = { val old=counts(i); counts(i) *= x; sum += counts(i)-old; }
  
  def sampleN(rand:scala.util.Random, N:Int) : Array[Int] = {
    //(initialize) : O(n*logn)
	val rtn = new Array[Int](N)
	var index:Int = 0
	var rollingSum:Double = 0
	var lst = List[Double]()
	foreach(N, (i:Int) => { lst = rand.nextDouble :: lst })
    val arr = lst.toArray
	scala.util.Sorting.quickSort(arr)
	lst = arr.toList
    //(sample) : O(n)
	foreach(counts.size, (i:Int) => {
	  rollingSum += counts(i) / sum
	  while(!lst.isEmpty && lst.first < rollingSum){
	    rtn(index) = i
		index += 1
		lst = lst.tail
	  }
	})
	//(return)
	rtn
  }
  
  def samplePair(rand:scala.util.Random) : (Int, Double) = {
    val argmax = sample(rand)
	(argmax, getProb(argmax))
  }
  def sample(random:scala.util.Random) = {
    val target = random.nextDouble*sum
    var i = -1; var accum = 0.0
    while (accum < target) { i += 1; accum += counts(i) }
    i
  }
  
  
  def sampleUniform(rand:scala.util.Random) : Int = {
  	val index:Int = (rand.nextDouble*size.doubleValue).intValue
  	index
  }

  def totalCount = sum
  
  def size = counts.size
  
  def multiply(other:ProbVec):ProbVec = {
  	if(other.counts.size != this.counts.size) {
  		throw new java.lang.IllegalArgumentException("Cannot Multiply ProbVecs: sizes don't match")
  	}
  	val rtn = ProbVec.zeros(counts.size)
  	foreach(counts.size, (n:Int) => {
  		rtn.addCount_!(counts(n) * other.counts(n))
  	})
  	rtn
  }

  def copy:ProbVec = {
    val rtn = ProbVec.zeros(counts.size)
	foreach(counts.size, (n:Int) => {
	  rtn.addCount_!(counts(n))
	})
	rtn
  }
  
  def setCount_!(key:Int, v:Double) = {
    sum += (v-counts(key))
    counts(key) = v
  }
  
  def maxPair:(Int,Double) = {
    var maxSoFar:Double = java.lang.Double.NEGATIVE_INFINITY
    var argmax:Int = -1
    foreach(counts.size, (i:Int) => {
      if(counts(i) > maxSoFar){
        maxSoFar = getProb(i)
        argmax = i
      }
    })
	if(argmax == -1) { println("WARNING: No Max: counts size: " + counts.size + " maxSoFar: " + maxSoFar) }
    (argmax, maxSoFar)
  }
  
  
  //--End Additions
  
}

object ProbVec {
  def zeros(n:Int) = new ProbVec(new Array[Double](n), 0, 0)
  def zeros2(n1:Int, n2:Int) = map(n1, zeros(n2))
  def zeros3(n1:Int, n2:Int, n3:Int) : Array[Array[ProbVec]] = map(n1, zeros2(n2, n3))
  def zeros3(n1:Int, n2:Int, n3:(Int,Int)=>Int) = map(n1, { i:Int => map(n2, { j:Int => zeros(n3(i,j)) }) })
  def zeros3(n1:Int, n2:Int=>Int, n3:(Int,Int)=>Int) = map(n1, { i:Int => map(n2(i), { j:Int => zeros(n3(i,j)) }) })
  def zeros3(n1:Int, n2:Int=>Int, n3:Int) = map(n1, { i:Int => map(n2(i), { j:Int => zeros(n3) }) })
}
