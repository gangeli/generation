package tea

import scala.collection.mutable.{ArrayBuffer,HashMap}
import java.util.Random
import fig.basic.{LogInfo,IOUtils}
import fig.exec.Execution
import fig.record.Record

class SubArray[A](private val array:Array[A], private val start:Int, private val end:Int) extends RandomAccessSeq[A] {
  override def hashCode = {
    var sum = 0
    var i = start
    while (i < end) {
      sum = (sum * 29 + array(i).hashCode)
      i += 1
    }
    sum
  }
  override def equals(_that:Any) : Boolean = {
    _that match {
      case that : SubArray[_] =>
        if (this.length == that.length) {
          var i = this.start
          var j = that.start
          while (i < this.end) {
            if (!(this.array(i) equals that.array(j)))
              return false
            i += 1
            j += 1
          }
          true
        }
        else
          false
      case _ => false
    }
  }
  override def length = end-start
  override def apply(i:Int) = {
    assume (i >= 0 && i < length)
    array(start+i)
  }
  override def toString = {
    val buf = new StringBuilder
    buf += '['
    var i = start
    while (i<end) {
      if (i > start) buf += ','
      buf.append(array(i).toString)
      i += 1
    }
    buf += ']'
    buf.toString
  }
}

/**
 * WARNING: many of these operations that iterate over sequences use length, which hopefully should be O(1) time:
 * fine for Array but bad for List, Stack.
 */
object Utils {
  // Logging
  def begin_track(format:String, args:Any*) = LogInfo.track(fmts(format, args : _*))
  def begin_track_printAll(format:String, args:Any*) = LogInfo.track(fmts(format, args : _*), true)
  def end_track = LogInfo.end_track
  def track[A](format:String, args:Any*)(body: =>A) = {
    LogInfo.track(fmts(format, args : _*))
    val result = body
    LogInfo.end_track
    result
  }
  def track_printAll[A](format:String, args:Any*)(body: =>A) = {
    LogInfo.track(fmts(format, args : _*), true)
    val result = body
    LogInfo.end_track
    result
  }
  def log(obj:Any) = LogInfo.logs(obj)
  def logs(format:String, args:Any*) = LogInfo.logs("%s", fmts(format, args : _*))
  def logss(format:String, args:Any*) = LogInfo.logss("%s", fmts(format, args : _*))
  def error(obj:Any) = LogInfo.error(obj)
  def errors(format:String, args:Any*) = LogInfo.error(fmts(format, args : _*))
  def warning(obj:Any) = LogInfo.warning(obj)
  def warnings(format:String, args:Any*) = LogInfo.warning(fmts(format, args : _*))
  def dbg(obj:Any) = LogInfo.dbg(obj)
  def dbgs(format:String, args:Any*) = LogInfo.dbg(fmts(format, args : _*))

  // Exception
  def fail(obj:Any) = new RuntimeException(obj.toString)
  def fails(format:String, args:Any*) = new RuntimeException(fmts(format, args : _*))
  def unknownCase(x:Any) = new RuntimeException("Unknown case: " + x)
  def impossible = new RuntimeException("Internal error: this shouldn't happen")
  def TODO = new RuntimeException("TODO: implement")
  def FUTURE = new RuntimeException("FUTURE: implement")

  ////////////////////////////////////////////////////////////
  // Iterators over a range: map, foreach, sum, product, max, min, argmin, argmax
  // Warning: when using map to create arrays which aren't of ints or doubles
  // or arrays of doubles, we need to do generic array creation, which is boxed and slow
  // even though Scala is supposed to minimize the overhead
  // That's why we specialize the map functions to the common data types.

  //// map over range
  def map[A](n:Int, f : => A) : Array[A] = { // Generic (SLOW)
    val result = new Array[A](n)
    var i = 0
    while (i < n) { result(i) = f; i += 1 }
    result
  }
  def map(n:Int, f : => Int) : Array[Int] = { // Int
    val result = new Array[Int](n)
    var i = 0
    while (i < n) { result(i) = f; i += 1 }
    result
  }
  def map(n:Int, f : => Double) : Array[Double] = { // Double
    val result = new Array[Double](n)
    var i = 0
    while (i < n) { result(i) = f; i += 1 }
    result
  }
  def map[A](n:Int, f : Int => A) : Array[A] = { // Generic (SLOW)
    val result = new Array[A](n)
    var i = 0
    while (i < n) { result(i) = f(i); i += 1 }
    result
  }
  def map(n:Int, f : Int => Int) : Array[Int] = { // Int
    val result = new Array[Int](n)
    var i = 0
    while (i < n) { result(i) = f(i); i += 1 }
    result
  }
  def map(n:Int, f : Int => Double) : Array[Double] = { // Double
    val result = new Array[Double](n)
    var i = 0
    while (i < n) { result(i) = f(i); i += 1 }
    result
  }
  def map(n:Int, f : Int => Array[Double]) : Array[Array[Double]] = { // Array[Double]
    val result = new Array[Array[Double]](n)
    var i = 0
    while (i < n) { result(i) = f(i); i += 1 }
    result
  }
  def map[A](a:Int, b:Int, f : Int => A) : Array[A] = map(a, b, 1, f) // Generic (SLOW)
  def map[A](a:Int, b:Int, incr:Int, f : Int => A) : Array[A] = {
    val result = new Array[A]((b-a+incr-1)/incr)
    var i = 0
    var x = a
    while (x < b) { result(i) = f(x); i += 1; x += incr }
    result
  }
  def map(a:Int, b:Int, f : Int => Int) : Array[Int] = { // Int
    val result = new Array[Int](b-a)
    var i = a
    while (i < b) { result(i-a) = f(i); i += 1 }
    result
  }
  def map(a:Int, b:Int, f : Int => Double) : Array[Double] = { // Double
    val result = new Array[Double](b-a)
    var i = a
    while (i < b) { result(i-a) = f(i); i += 1 }
    result
  }

  //// foreach over range
  def foreach(n:Int, f : Int => Any) = {
    var i = 0
    while (i < n) { f(i); i += 1 }
  }
  def foreachReverse(n:Int, f : Int => Any) = {
    var i = n-1
    while (i >= 0) { f(i); i -= 1 }
  }
  def foreach(a:Int, b:Int, f : Int => Any) = {
    var i = a
    while (i < b) { f(i); i += 1 }
  }
  def foreachReverse(b:Int, a:Int, f : Int => Any) = {
    var i = b-1
    while (i >= a) { f(i); i -= 1 }
  }

  //// min
  def min[A <% Ordered[A]](a:Int, b:Int, f : Int => A) : A = {
    assert (a < b)
    var bestv = f(a); var i = a+1
    while (i < b) { val v = f(i); if (v < bestv) bestv = v; i += 1 }
    bestv
  }
  def min[A <% Ordered[A]](n:Int, f : Int => A) : A = min(0, n, f)
  def min[A <% Ordered[A]](a:Seq[A]) : A = min(a.size, { i:Int => a(i) })
  def argmin[A <% Ordered[A]](n:Int, f : Int => A) : Int = argminmin(n, f)._1
  def argminmin[A <% Ordered[A]](n:Int, f : Int => A) = {
    var bestv = f(0)
    var besti = 0
    var i = 1
    while (i < n) { val v = f(i); if (v < bestv) { besti = i; bestv = v }; i += 1 }
    (besti, bestv)
  }

  //// max
  def max[A <% Ordered[A]](a:Int, b:Int, f : Int => A) : A = {
    assert (a < b)
    var bestv = f(a); var i = a+1
    while (i < b) { val v = f(i); if (v > bestv) bestv = v; i += 1 }
    bestv
  }
  def max[A <% Ordered[A]](n:Int, f : Int => A) : A = max(0, n, f)
  def max[A <% Ordered[A]](a:Seq[A]) : A = max(a.size, { i:Int => a(i) })
  def argmax[A <% Ordered[A]](n:Int, f : Int => A) : Int = argmaxmax(n, f)._1
  def argmaxmax[A <% Ordered[A]](n:Int, f : Int => A) = {
    var bestv = f(0)
    var besti = 0
    var i = 1
    while (i < n) { val v = f(i); if (v > bestv) { besti = i; bestv = v }; i += 1 }
    (besti, bestv)
  }

  //// sum
  def sum(a:Int, b:Int, f : Int => Int) = {
    var result = 0
    var i = a
    while (i < b) { result += f(i); i += 1 }
    result
  }
  def sum(n:Int, f : Int => Int) : Int = sum(0, n, f)
  def sum(a:Seq[Int]) : Int = sum(a.size, { i:Int => a(i) })
  def sum(a:Int, b:Int, f : Int => Double) = {
    var result = 0.0
    var i = a
    while (i < b) { result += f(i); i += 1 }
    result
  }
  def sum(n:Int, f : Int => Double) : Double = sum(0, n, f)
  def sum(a:Seq[Double]) : Double = sum(a.size, { i:Int => a(i) })

  //// product
  def product(n:Int, f : Int => Double) = {
    var result = 1.0
    var i = 0
    while (i < n) { result *= f(i); i += 1 }
    result
  }

  //// exists, forall, find
  def exists(a:Int, b:Int, f : Int => Boolean) = {
    var i = a
    while (i < b && !f(i)) i += 1
    i < b
  }
  def exists(n:Int, f : Int => Boolean) : Boolean = exists(0, n, f)
  def forall(a:Int, b:Int, f : Int => Boolean) = {
    var i = a
    while (i < b && f(i)) i += 1
    i == b
  }
  def forall(n:Int, f : Int => Boolean) : Boolean = forall(0, n, f)
  def find(a:Int, b:Int, f : Int => Boolean) = {
    var i = a
    while (i < b && !f(i)) i += 1
    i
  }
  def find(n:Int, f : Int => Boolean) : Int = find(0, n, f)

  ////////////////////////////////////////////////////////////
  // Iterators over arrays

  //// foreach
  def foreach[A](a:Seq[A], f : A => Any) : Unit = {
    var i = 0
    while (i < a.length) { f(a(i)); i += 1 }
  }
  def foreach[A](a:Seq[A], f : (Int,A) => Any) : Unit = {
    var i = 0
    while (i < a.length) { f(i, a(i)); i += 1 }
  }
  def foreachPair[A,B](a:Seq[(A,B)], f : (A,B) => Any) = {
    var i = 0
    while (i < a.length) { f(a(i)._1, a(i)._2); i += 1 }
  }

  //// sum
  def sum(a:Array[Int]) : Int = sum(a.length, { i:Int => a(i) })
  def sum(a:Array[Double]) : Double = sum(a.length, { i:Int => a(i) })
  def sum2(m:Array[Array[Double]]) : Double = sum(m.length, { i:Int => sum(m(i)) })

  //// max
  def max[A <% Ordered[A]](a:Array[A]) : A = max(a.length, { i:Int => a(i) })
  def argmax[A <% Ordered[A]](a:Array[A]) : Int = argmax(a.length, { i:Int => a(i) })
  def argmaxmax[A <% Ordered[A]](a:Array[A]) : (Int, A) = argmaxmax(a.length, { i:Int => a(i) })
  def max2[A <% Ordered[A]](m:Array[Array[A]]) = max(m.length, { i:Int => max(m(i)) })
  def argmaxmax2[A <% Ordered[A]](A:Array[Array[A]]) = {
    var bestv = A(0)(0)
    var besti = 0
    var bestj = 0
    var i = 0
    while (i < A.length) {
      var j = 0
      while (j < A(i).length) { 
        val v = A(i)(j)
        if (v > bestv) { besti = i; bestj = j; bestv = v }
        j += 1
      }
      i += 1
    }
    (besti, bestj, bestv)
  }

  //// forall
  def forall[A](a:Array[A], f : A => Boolean) : Boolean = forall(a.length, { i:Int => f(a(i)) })

  //// misc
  def slice[A](a:Array[A], i:Int, j:Int, incr:Int) = map(i, j, incr, { a(_) })

  def sample(random:Random, probs:Array[Double]) = { // Assume sum(probs) = 1
    val target = random.nextDouble
    var i = -1
    var accum = 0.0
    while (accum < target) {
      i += 1
      accum += probs(i)
    }
    i
  }
  def entropy(probs:Array[Double]) = {
    var sum = 0.0
    foreach(probs, { p:Double => if (p > 0) sum += p * Math.log(p) })
    -sum
  }

  def count[A](a:Array[A], f:(A=>Boolean)) = { var c = 0; a.foreach { x => if (f(x)) c += 1 }; c }
  def count(n:Int, f:Int => Boolean) = { var c = 0; foreach(n, { i:Int => if (f(i)) c += 1 }); c }

  def log1_!(a:Array[Double]) = foreach(a.length, { i:Int => a(i) = Math.log(a(i)) })
  def log2_!(a:Array[Array[Double]]) = a.foreach(log1_!(_))
  def log3_!(a:Array[Array[Array[Double]]]) = a.foreach(log2_!(_))

  // Two arrays
  def foreach[A,B](a:Array[A], b:Array[B], f : (A,B) => Any) : Unit = {
    var i = 0
    assertSameLength(a, b)
    while (i < a.length) { f(a(i), b(i)); i += 1 }
  }
  def foreach[A,B](a:Array[A], b:Array[B], f : (Int,A,B) => Any) : Unit = {
    var i = 0
    assertSameLength(a, b)
    while (i < a.length) { f(i, a(i), b(i)); i += 1 }
  }

  //// Hash map
  def map[A,B,C](h:HashMap[A,B], f : (A,B) => C) = {
    val array = new Array[C](h.size) // SLOW
    var i = 0
    h.foreach { kv:(A,B) => array(i) = f(kv._1, kv._2); i += 1 }
    array
  }
  def foreach[A,B](h:HashMap[A,B], f : (A,B) => Any) = {
    h.foreach { kv:(A,B) => f(kv._1, kv._2) }
  }
  def sum[A,B](h:HashMap[A,B], f : (A,B) => Double) = {
    var result = 0.0
    h.foreach { kv:(A,B) => result += f(kv._1, kv._2) }
    result
  }
  def sum[A](h:HashMap[A,Double]) = {
    var result = 0.0
    h.foreach { kv:(A,Double) => result += kv._2 }
    result
  }
  def product[A,B](h:HashMap[A,B], f : (A,B) => Double) = {
    var result = 1.0
    h.foreach { kv:(A,B) => result *= f(kv._1, kv._2) }
    result
  }

  ////////////////////////////////////////////////////////////
  // Normalization functions

  //// normalize an array
  def normalize_!(a:Array[Double]) : Boolean = { // Return whether could normalize (sum not 0)
    val z = sum(a)
    if (z != 0) { div_!(a, z); true }
    else false
  }
  def normalizeZ_!(a:Array[Double]) = { // Return normalization constant
    val z = sum(a)
    if (z != 0) foreach(a.length, { i:Int => a(i) /= z })
    z
  }
  def normalizeOrFail_!(a:Array[Double]) = { if (!normalize_!(a)) throw fail("Can't normalize"); a }
  def normalizeOrSetUniform_!(a:Array[Double]) = { if (!normalize_!(a)) set_!(a, 1.0/a.length); a }

  //// normalize hash table
  def normalize_![A](h:HashMap[A,Double]) : Boolean = {
    val z = sum(h)
    if (z != 0) { h.keys.foreach({ k:A => h(k) /= z }); true }
    else false
  }
  def normalizeOrFail_![A](h:HashMap[A,Double]) = { if (!normalize_!(h)) throw fail("Can't normalize"); h }

  //// normalize a matrix
  def normalize2_!(m:Array[Array[Double]]) : Boolean = {
    val z = sum2(m)
    if (z != 0) { div2_!(m, z); true }
    else false
  }
  def normalize2Z_!(m:Array[Array[Double]]) = { // Return normalization constant
    val z = sum2(m)
    if (z != 0) { div2_!(m, z); true }
    else false
    z
  }
  def normalize2OrFail_!(m:Array[Array[Double]]) = { if (!normalize2_!(m)) throw fail("Can't normalize"); m }
  def normalize2OrSetUniform_!(m:Array[Array[Double]]) = { if (!normalize2_!(m)) set_!(m, 1.0/(m.length*m(0).length)); m }

  def expNormalize_!(a:Array[Double]) : Boolean = {
    // Rescale first to make sure things stay in range
    val m = max(a)
    foreach(a.length, { i:Int => a(i) = Math.exp(a(i)-m) })
    normalize_!(a)
  }
  def expNormalizeOrFail_!(a:Array[Double]) = { if (!expNormalize_!(a)) throw fail("Can't normalize"); a }

  def expNormalizeLogZ_!(a:Array[Double]) = { // Return log normalization constant
    // Rescale first to make sure things stay in range
    val M = max(a)
    foreach(a.length, { i:Int => a(i) = Math.exp(a(i)-M) })
    M + Math.log(normalizeZ_!(a))
  }
  def expNormalize2LogZ_!(m:Array[Array[Double]]) = { // Return log normalization constant
    // Rescale first to make sure things stay in range
    val M = max2(m)
    foreach(m.length, { i:Int =>
      foreach(m(i).length, { j:Int => m(i)(j) = Math.exp(m(i)(j)-M) })
    })
    M + Math.log(normalize2Z_!(m))
  }

  def normalizeAndSample(random:Random, n:Int, f : Int => Double) = {
    val weights = map(n, f)
    normalizeOrFail_!(weights)
    sample(random, weights)
  }

  //// Formatting: warning: these functions can be slow

  def fmt(x:Int) : String = x+""
  def fmt(x:Double) : String = {
    if ((x - x.toInt).abs < 1e-40) // An integer (probably)
      x.toInt.toString
    else if (x.abs < 1e-3) // Scientific notation (close to 0)
      String.format("%.2e", x:java.lang.Double)
    else if (x.abs > 1e3) // Scientific notation (large)
      String.format("%.2e", x:java.lang.Double)
    else
      String.format("%.3f", x:java.lang.Double)
  }
  def fmt(x:Any) : String = x match {
    case a:Double => fmt(a)
    case a:Int => fmt(a)
    case _ => x.toString
  }

  def fmt1[A](xs:Seq[A]) : String = {
    val buf = new StringBuilder()
    for (x <- xs) {
      if (buf.length > 0) buf.append(' ')
      buf.append(fmt(x))
    }
    buf.toString
  }

  def fmt2[A](xss:Seq[Seq[A]]) : String = {
    val buf = new StringBuilder()
    for (xs <- xss) {
      if (buf.length > 0) buf.append(" |")
      for (x <- xs) {
        if (buf.length > 0) buf.append(' ')
        buf.append(fmt(x))
      }
    }
    buf.toString
  }

  // String.format but calls fmt on all the arguments; always use %s in the format string
  def fmts(format:String, args:Any*) : String = String.format(format, args.map { x => fmt(x) } : _*)

  ///// Asserting valid

  def assertSameLength[A,B](a:Array[A], b:Array[B]) = {
    if (a.length != b.length) throw fails("Lengths not the same: %s %s", ""+a.length, ""+b.length)
  }

  def assertNormalized(a:Array[Double]) = {
    val z = sum(a)
    if (Math.abs(z-1) > 1e-2) throw fails("Not normalized: sum is %s", z+"")
  }

  def assertValid(x:Double) = { if (x.isNaN || x.isInfinite) throw fail(x); x }
  def assertValid(a:Array[Double]) = { if (!forall(a, { x:Double => !(x.isNaN || x.isInfinite)})) throw fail(fmt1(a)); a }

  // Apply a function f repeatedly...
  def iter[A](x:A, n:Int, f:A=>A) = { // n times
    var y = x
    foreach(n, { _:Int => y = f(y) })
    y
  }
  def iter[A](x:A, test:A=>Boolean, f:A=>A) = { // while test = true
    var y = x
    while (test(y)) { y = f(y) }
    y
  }
  def iter[A](x:A, n:Int, test:A=>Boolean, f:A=>A) = { // while test = true and at most n times
    var i = 0
    var y = x
    while (i < n && test(y)) { y = f(y); i += 1 }
    y
  }
  // Call test on last two elements
  def iter[A](x:A, test:(A,A)=>Boolean, f:A=>A) = { // while test = true
    var y = x
    var done = false
    while (!done) {
      val z = f(y)
      if (test(y,z))
        y = z
      else
        done = true
    }
    y
  }

  // Set a(i) = x temporarily to call f
  def tempSet[A](a:Array[A], i:Int, x:A, f : => Any) = {
    val oldx = a(i)
    a(i) = x
    f
    a(i) = oldx
  }

  def square(x:Double) = x*x

  ////////////////////////////////////////////////////////////
  // IO

  // f: process the line and return whether we should
  // keep it and continue (true) or discard and stop (false)
  def foreachLine(path:String, f : String => Boolean) = {
    val in = IOUtils.openInEasy(path)
    if (in != null) {
      var done = false
      while (!done) {
        val line = in.readLine()
        done = (line == null || !f(line))
      }
      in.close
      true
    }
    else
      false
  }
  // Keep track of lines
  def foreachLine(path:String, maxLines:Int, f : String => Boolean) : Boolean = {
    var numLines = 0
    foreachLine(path, { line:String =>
      if (numLines < maxLines) { numLines += 1; f(line) } else false
    })
  }

  // Return an array of the result of f applied to each line
  def mapLine[A](path:String, f : String => A) = {
    createArray({ g:(A => Any) => // g stuffs the result into the array
      foreachLine(path, { line:String => g(f(line)); true })
    })
  }
  def mapLine[A](path:String, maxLines:Int, f : String => A) = {
    createArray({ g:(A => Any) => // g stuffs the result into the array
      foreachLine(path, maxLines, { line:String => g(f(line)); true })
    })
  }
  def readLines(path:String) = mapLine(path, { s:String => s })

  // Creates an array buffer to collect the items,
  // and then creates an array just large enough to store it
  // doAll is passed a function that sticks stuff in the buffer
  def createArray[A](doAll : (A => Any) => Any) = {
    val buf = new ArrayBuffer[A]
    doAll({ x => buf.append(x) })
    buf.toArray
    // Don't know why I had this before
    //val array = new Array[A](buf.length) // SLOW
    //buf.copyToArray(array, 0)
    //array
  }
  def toArray[A](it:Iterator[A]) = createArray({ add:(A=>Any) => it.foreach { x:A => add(x) } })
  def toArray[A](list:java.util.List[A]) : Array[A] = map(list.size, { i:Int => list.get(i) })

  // doAll is passed a function that does the actual writing
  def writeLines(path:String, doAll : (String => Any) => Any) = {
    val out = IOUtils.openOutEasy(path)
    if (out != null) {
      doAll({ line => out.println(line) })
      out.close
      true
    }
    else
      false
  }

  def readLines(path:String, doAll : (=> String) => Any) = {
    val in = IOUtils.openInEasy(path)
    if (in != null) {
      doAll(in.readLine)
      in.close
      true
    }
    else
      false
  }

  def reverse_![A](array:Array[A]) = {
    foreach(array.length, { i:Int => swap(array, i, array.length-i-1) })
  }

  // Sort a pair by the second item
  case class PairBySecond[A,B <% Ordered[B]](_1:A, _2:B) extends Ordered[PairBySecond[A,B]] {
    override def compare(that:PairBySecond[A,B]) = this._2 compare that._2
  }
  case class PairByReverseSecond[A,B <% Ordered[B]](_1:A, _2:B) extends Ordered[PairByReverseSecond[A,B]] {
    override def compare(that:PairByReverseSecond[A,B]) = that._2 compare this._2
  }

  // Sort strings so that embedded numbers are treated naturally: a1b < a2 < a10
  case class StringWithEmbeddedInt(value:String) extends Ordered[StringWithEmbeddedInt] {
    override def compare(that:StringWithEmbeddedInt) : Int = {
      import Character.isDigit
      val s1 = value; val s2 = that.value
      val n1 = s1.length; val n2 = s2.length
      var i1 = 0; var i2 = 0 
      while (i1 < n1 && i2 < n2) {
        if (isDigit(s1(i1)) && isDigit(s2(i2))) {
          var x1 = 0; while (i1 < n1 && isDigit(s1(i1))) { x1 = x1*10+(s1(i1)-'0'); i1 += 1 }
          var x2 = 0; while (i2 < n2 && isDigit(s2(i2))) { x2 = x2*10+(s2(i2)-'0'); i2 += 1 }
          if (x1 != x2) return x1-x2
        }
        else {
          if (s1(i1) != s2(i2)) return s1(i1)-s2(i2)
          i1 += 1
          i2 += 1
        }
      }
      n1-n2
    }
  }
  def sortWithEmbeddedInt_!(a:Array[String]) = sort_!(a.map(new StringWithEmbeddedInt(_))).map(_.value)

  // Assume the array is already sorted, just like the Unix command
  def uniq[A](a:Array[A]) = createArray({ add:(A => Any) =>
    foreach(a.length, { i:Int =>
      if (i == 0 || a(i) != a(i-1)) add(a(i))
    })
  })

  def foreachSorted[A <% Ordered[A]](array:Array[A], numTake:Int, reverse:Boolean, f : (Int,A) => Any) : Unit = {
    if (reverse) {
      val rawPairs = map(array.length, { i:Int => new PairByReverseSecond(i, array(i)) })
      val pairs = partialSort_!(rawPairs, numTake).take(numTake)
      pairs.foreach { pair => f(pair._1, pair._2) }
    }
    else {
      val rawPairs = map(array.length, { i:Int => new PairBySecond(i, array(i)) })
      val pairs = partialSort_!(rawPairs, numTake).take(numTake)
      pairs.foreach { pair => f(pair._1, pair._2) }
    }
  }
  def foreachSorted[A <% Ordered[A]](array:Array[A], reverse:Boolean, f : (Int,A) => Any) : Unit = foreachSorted(array, array.length, reverse, f)
  def mapSorted[A <% Ordered[A],B](array:Array[A], reverse:Boolean, f : (Int,A) => B) : Array[B] = {
    createArray({ add:(B=>Any) =>
      foreachSorted(array, array.length, reverse, { (i:Int,x:A) => add(f(i, x)) })
    })
  }
  def sortedIndices[A <% Ordered[A]](array:Array[A], reverse:Boolean) : Array[Int] = {
    createArray({ add:(Int=>Any) =>
      foreachSorted(array, array.length, reverse, { (i:Int,x:A) => add(i) })
    })
  }

  def foreachSortedByValue[A, B <% Ordered[B]](h:HashMap[A,B], numTake:Int, reverse:Boolean, f : (A,B) => Any) : Unit = {
    if (reverse) {
      val rawPairs = map(h, { (k:A,v:B) => new PairByReverseSecond(k, v) })
      val pairs = partialSort_!(rawPairs, numTake).take(numTake)
      pairs.foreach { pair => f(pair._1, pair._2) }
    }
    else {
      val rawPairs = map(h, { (k:A,v:B) => new PairBySecond(k, v) })
      val pairs = partialSort_!(rawPairs, numTake).take(numTake)
      pairs.foreach { pair => f(pair._1, pair._2) }
    }
  }
  def foreachSortedByValue[A, B <% Ordered[B]](h:HashMap[A,B], reverse:Boolean, f : (A,B) => Any) : Unit = foreachSortedByValue(h, h.size, reverse, f)

  // Return the first numTake elements in an array
  // Mutates original array
  def partialSort_![A <% Ordered[A]](array:Array[A], numTake:Int) = {
    val k = Math.min(numTake, array.length) - 1
    def sort(left:Int, right:Int) : Unit = {
      if (left >= right) return
      val mid = randSplit(array, left, right)
      sort(left, mid-1)
      if (mid < k) sort(mid+1, right)
    }
    sort(0, array.length-1)
    array
  }
  def sort_![A <% Ordered[A]](array:Array[A]) = partialSort_!(array, array.length)

  // Return any index k such that
  // array(j) <= array(k) for j < k 
  // array(j) >= array(k) for j > k 
  private val randSplitRandom = new scala.util.Random(12345)
  def randSplit[A <% Ordered[A]](array:Array[A], left:Int, right:Int) : Int = {
    if (left == right) return left
    val pivot = randSplitRandom.nextInt(right-left+1) + left
    val x = array(pivot)
    def cmpx(i:Int) = {
      val z = array(i) compare x
      if (z != 0) z
      else i compare pivot
    }
    //println("pivot value = " + x)
    swap(array, pivot, left) // Put x on the left
    var i = left+1
    var j = right
    // Break ties based on position; otherwise don't get even split
    while (i<j) {
      while (i<j && cmpx(i) <= 0) i += 1
      while (i<j && cmpx(j) >= 0) j -= 1
      if (i<j) swap(array, i, j)
      // Invariant: i == j or (i < j and array(i) <= x <= array(j))
    }
    val k = if (array(i) <= x) i else i-1
    swap(array, k, left)
    k
  }

  // Expected running time: O(\log n)
  // move elements in the array around so that
  // array(j) <= array(k) for j < k
  // array(j) >= array(k) for j > k
  def select_![A <% Ordered[A]](array:Array[A], k:Int) : A = {
    val n = array.length
    if (k < 0 || k >= n)
      throw fails("Out of range: %s not in [0,%s)", fmt(k), fmt(n))
    var left = 0
    var right = n-1
    while (left < right) {
      // Invariant: left <= k <= right
      val mid = randSplit(array, left, right)
      if (k < mid)
        right = mid-1
      else if (k > mid)
        left = mid+1
      else {
        left = k; right = k
      }
    }
    array(k)
  }

  def sortTest = {
    def splits_?(array:Array[Int], k:Int) =
      forall(0, k, { j:Int => array(j) <= array(k) }) &&
      forall(k+1, array.length, { j:Int => array(j) >= array(k) })
    def sorted_?(array:Array[Int], left:Int, right:Int) =
      forall(left, right, { j:Int => array(j) <= array(j+1) })
    val random = new scala.util.Random(23)
    Array(1, 2, 10, 100, 1000).foreach { n:Int =>
      println("Checking "+n)
      // Split
      foreach(n, { k:Int =>
        val array = map(n, { _:Int => random.nextInt(n) })
        select_!(array, k)
        if (!splits_?(array, k))
          throw fails("Not split: k=%s %s", fmt(k), fmt1(array))
      })
      // Sort
      foreach(n, { k:Int =>
        val array = map(n, { _:Int => random.nextInt(n) })
        partialSort_!(array, k+1)
        if (!sorted_?(array, 0, k) || !splits_?(array, k))
          throw fails("Not partially sorted: k=%s %s", fmt(k), fmt1(array))
      })
    }
  }

  // Assume array's goodness is ... false false false true true true ...
  // Return the first item that's good
  def bsearchFindFirst[A](array:Array[A], good:(A => Boolean)) = {
    var l = 0
    var u = array.length
    while (l < u) {
      val m = (l+u)/2
      if (good(array(m)))
        u = m
      else
        l = m+1
    }
    l
  }

  def swap[A](array:Array[A], i:Int, j:Int) = {
    val tmp = array(i)
    array(i) = array(j)
    array(j) = tmp
  }

  def returnFirst[A](value:A, junk: => Any) = { junk; value }

  def applyIf[A](b:Boolean, f:A=>A, x:A) = if (b) f(x) else x

  def same[A](x:A, y:A) = {
    if (x != y) throw fails("Different: %s %s", x.toString, y.toString)
    x
  }

  ////////////////////////////////////////////////////////////
  // Vector, matrix operations

  // Distance
  def distL1(a:Seq[Double], b:Seq[Double]) = {
    sum(same(a.length, b.length), { i:Int => Math.abs(a(i)-b(i)) })
  }
  def distL2(a:Seq[Double], b:Seq[Double]) = {
    Math.sqrt(distL2Squared(a, b))
  }
  def distL2Squared(a:Seq[Double], b:Seq[Double]) = {
    sum(same(a.length, b.length), { i:Int => square(a(i)-b(i)) })
  }
  def normL2Squared(a:Seq[Double]) = {
    sum(a.length, { i:Int => square(a(i)) })
  }

  def div_!(a:Array[Double], x:Double) = { foreach(a.length, { i:Int => a(i) /= x }); a }
  def div2_!(m:Array[Array[Double]], x:Double) = { foreach(m.length, { i:Int => div_!(m(i), x) }); m }
  def mult_!(a:Array[Double], x:Double) = { foreach(a.length, { i:Int => a(i) *= x }); a }
  def mult(A:Array[Array[Double]], x:Array[Double]) = A.map(dot(_, x))
  // FUTURE: choose order to minimize number of operations
  def mult(A:Array[Array[Double]], B:Array[Array[Double]], C:Array[Array[Double]]) : Array[Array[Double]] = mult(mult(A, B), C)
  def mult(A:Array[Array[Double]], B:Array[Array[Double]]) = {
    map(A.length, { i:Int =>
      map(B(0).length, { j:Int =>
        sum(same(A(i).length, B.length), { k:Int => A(i)(k)*B(k)(j) })
      })
    })
  }
  def add_!(a:Array[Double], b:Array[Double]) = { foreach(same(a.length, b.length), { i:Int => a(i) += b(i) }); a }
  def set_!(a:Array[Double], b:Array[Double]) = { foreach(same(a.length, b.length), { i:Int => a(i) = b(i) }); a }
  def add_!(a:Array[Double], scale:Double, b:Array[Double]) = { foreach(a.length, { i:Int => a(i) += scale*b(i) }); a }
  def add_!(a:Array[Double], b:Double) = { foreach(a.length, { i:Int => a(i) += b }); a }
  def set_!(a:Array[Double], b:Double) = { foreach(a.length, { i:Int => a(i) = b }); a }
  def set_!(a:Array[Int], b:Int) = { foreach(a.length, { i:Int => a(i) = b }); a }
  def set_!(a:Array[Int], b:Array[Int]) = { foreach(a.length, { i:Int => a(i) = b(i) }); a }
  def set_!(a:Array[Char], b:Char) = { foreach(a.length, { i:Int => a(i) = b }); a }
  def set_!(A:Array[Array[Double]], b:Double) : Array[Array[Double]] = { A.foreach(set_!(_, b)); A }
  def set_!(A:Array[Array[Int]], b:Int) : Array[Array[Int]] = { A.foreach(set_!(_, b)); A }

  def dot(x:Array[Double], y:Array[Double]) = sum(same(x.length, y.length), { i:Int => x(i)*y(i) })
  def copy(a:Array[Double]) : Array[Double] = { val b = new Array[Double](a.length); set_!(b, a); b }
  def copy(a:Array[Array[Double]]) : Array[Array[Double]] = {
    val b = new Array[Array[Double]](a.length)
    foreach(a.length, { i:Int => b(i) = copy(a(i)) })
    b
  }

  ////////////////////////////////////////////////////////////
  // Simple readable serialization/deserialization (not optimized for efficiency)
  def serializeArray[A](puts:(String=>Any), a:Array[A]) = puts(a.mkString(" "))
  def deserializeDoubleArray(gets: =>String) = gets.split(" ").map(_.toDouble)
  def serializeArray2[A](puts:(String=>Any), A:Array[Array[A]]) = {
    puts(fmt(A.length))
    A.foreach(serializeArray(puts, _))
  }
  def deserializeDoubleArray2(gets: =>String) = {
    val n = gets.toInt
    map(n, { _:Int => deserializeDoubleArray(gets) })
  }
  def serializeArray3[A](puts:(String=>Any), A:Array[Array[Array[A]]]) = {
    puts(fmt(A.length))
    A.foreach(serializeArray2(puts, _))
  }
  def deserializeDoubleArray3(gets: =>String) = {
    val n = gets.toInt
    map(n, { _:Int => deserializeDoubleArray2(gets) })
  }

  def parallel_map[A,B](numThreads:Int, a:Seq[A], f:(Int,A,Boolean)=>B) = {
    val result = new Array[B](a.length)
    parallel_foreach(numThreads, a, { (i:Int,x:A,log:Boolean) =>
      val y = f(i,x,log)
      result.synchronized { result(i) = y }
    })
    result
  }

  def parallel_foreach[A](numThreads:Int, a:Seq[A], f:(Int,A,Boolean)=>Any) = {
    if (a.size == 1) f(0, a.first, true) // Special case: no need to start threads
    else {
      import java.util.concurrent.{ExecutorService,Executors,TimeUnit}
      //val executor = Executors.newCachedThreadPool
      val executor = Executors.newFixedThreadPool(numThreads)
      var exception : Option[Throwable] = None
      var primaryThread : Option[Thread] = None
      foreach(a, { (i:Int,x:A) =>
        executor.execute(new Runnable() {
          def run = {
            if (!Execution.shouldBail) {
              try {
                if (exception == None) {
                  executor.synchronized {
                    if (primaryThread == None)
                      primaryThread = Some(Thread.currentThread)
                  }
                  f(i, x, primaryThread == Some(Thread.currentThread))
                }
              } catch { case t : Throwable => 
                exception = Some(t); // Save exception
              }
            }
          }
        })
      })
      executor.shutdown
      try {
        while (!executor.awaitTermination(1, TimeUnit.SECONDS)) { }
      } catch { case e:InterruptedException => throw fail("Interrupted") }
      exception match { 
		case Some(t) => {
			t.printStackTrace
			throw new RuntimeException(t); 
		}
		case None => 
	  }
    }
  }

  // Loop through an array of things and print out status using track/record
  def track_foreach(name:String, n:Int, f:Int => Boolean) : Unit = {
    var i = 0
    var done = false
    val currName = "curr"+name.capitalize
    while (!done && i < n) {
      begin_track("%s %s/%s", name.capitalize, i, n)
      Execution.putOutput(currName, i)
      done = Execution.shouldBail || !f(i)
      i += 1
      end_track
    }
    Execution.putOutput(currName, i)
  }
  def track_foreach[A](name:String, a:Seq[A], f:A => Boolean) : Unit = {
    track_foreach(name, a.size, { i:Int => f(a(i)) })
  }
  def track_parallel_foreach[A](numThreads:Int, name:String, a:Seq[A], f:A => Any) : Unit = {
    val currName = "curr"+name.capitalize
    parallel_foreach(numThreads, a, { (i:Int,x:A,log:Boolean) =>
      if (log) begin_track("%s %s/%s", name.capitalize, i, a.size)
      if (log) Execution.putOutput(currName, i)
      f(x)
      if (log) end_track
    })
    Execution.putOutput(currName, a.size)
  }
  def track_record_foreach(name:String, n:Int, f:Int => Boolean) : Unit = {
    track_foreach(name, n, { i:Int => Record.begin(name, i); returnFirst(f(i), Record.end) })
  }
  def track_record_foreach[A](name:String, a:Seq[A], f:A => Boolean) : Unit = {
    track_record_foreach(name, a.size, { i:Int => f(a(i)) })
  }

  // justify(r, c) = -1 (left), 0 (center), 1 (right)
  def formatTable(table:Array[Array[String]], justify:((Int,Int)=>Int)) : Array[String] = {
    def spaces(n:Int) = set_!(new Array[Char](n), ' ').mkString
    val nr = table.length
    val nc = max(table.map(_.length))
    def get(r:Int, c:Int) = if (c < table(r).length) table(r)(c) else ""
    val widths = map(nc, { c:Int => max(nr, { r:Int => get(r, c).length }) })
    map(nr, { r:Int =>
      map(nc, { c:Int =>
        val padding = (widths(c)-get(r,c).length) max 0
        justify(r, c) match {
          case -1 => get(r, c) + spaces(padding)
          case 0 => spaces(padding/2) + get(r, c) + spaces((padding+1)/2)
          case 1 => spaces(padding) + get(r, c)
          case _ => throw fail("Bad"); ""
        }
      }).mkString(" ")
    })
  }

  def empty_?(s:String) = s == null || s.length == 0

  // Detect overflow, then set to cap (MAX_VALUE or MIN_VALUE)
  def safeAdd(x:Int, y:Int) = {
    if (x > 0 && y > 0) {
      val z = x+y
      if (z > 0) z else Integer.MAX_VALUE
    }
    else if (x < 0 && y < 0) {
      val z = x+y
      if (z < 0) z else Integer.MIN_VALUE
    }
    else { // No chance of overflow if both have different sign
      x+y
    }
  }
}
