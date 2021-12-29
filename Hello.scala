import scala.collection.mutable.ListBuffer
import scala.math.BigInt._
import scala.util.Random


object Hello{

  def main(args: Array[String]): Unit = {
    println(task31(List((7, "e"), (17, "e66"), (75, "e6"), (79, "e8"), (71, "e2"))))
  }

  def task4(): Unit = {
    println(BigInt(2).pow(1024))
  }

  def task5(): Unit = {
    println(probablePrime(100, Random))
  }

  def task6(): Unit = {
    println(probablePrime(100, Random).toString(36))
  }

  def task7(): Unit = {
    val s2: String = "Hello"
    println(s2(0))
    println(s2(s2.length - 1))
  }

  def task8(): Unit = {
    //def take(n: Int): String // Выбирает первые n элементов.
    // def drop(n: Int): String // Выбирает все элементы, кроме первых n.
    // def takeRight(n: Int): String // Выбирает последние n элементов.
    // def dropRight(n: Int): String // Выбирает все элементы, кроме последних n.
  }

  def tast9(int: Int): Int = {
    var signum: Int = 0
    if (int > 0) {
      signum = 1
    }
    else if (int < 0) {
      signum = -1
    }
    signum
  }

  def task11(): Unit = {
    for (w <- 0 to 10) {
      println(10 - w)
    }
  }

  def task12(n: Int): Unit = {
    for (w <- 0 to n) {
      println(n - w)
    }
  }

  def task13(): Unit = {
    val s2: String = "Hello"
    var a: Long = 1
    for (w <- 0 until s2.length) {
      a = a * s2(w)
    }
    println(a)
  }

  def task14(s2: String): Long = {
    s2.foldLeft(1L)((m, n) => m * n)
  }
  def task15(s2: String): Long = {
    var x: Long = 1
    for (c <- s2) x *= c.toInt
    x
  }

  def task16(s2: String): Long = {
    if (s2.nonEmpty) {
      s2(0) * task14(s2.drop(1))
    } else 1L
  }

//   def tast17(n: Long): Long ={
//    if(n>0){
//      if(n % 2 == 0){
//
//      } else {
//
//      }
//    } else {
//      if(n==0){
//        1L
//      } else {
//        tast17(n)
//      }
//    }
//   }

  def task18(m: Int, n: Int): Int = {
    var a: Int = 0
    for (w <- m to n; if (valid(w.toString))) {
      a += w;
    }
    a
  }
  def valid(toString: String): Boolean = {
    var a = ListBuffer[Int]()
    for (w <- 0 until toString.length) {
      if (!a.contains(toString(w))) {
        a += toString(w)
      } else {
        return false
      }
    }
    true
  }

  def task19(m: List[Any]) : List[Int]={
    var s = 1::Nil
    s = s.filter(_<2)
    val s2 = m.toString()
    for(v<-0 until s2.length){
      if(s2(v).toInt>47&&s2(v).toInt<58){
        s = s2(v).toString.toInt::s
      }
    }
    s.reverse
  }

  def tak20(n: Int): Int = {
    var a:Int = 1
    for (i <- 2 until n) {
      if (n % i == 0) {
        a=i;
      }
    }
    var c = 0
    for (w <- 0 until a.toString.length) {
      c+=a.toString.charAt(w).toString.toInt
    }
    c
  }

  def task21(k:Int, m: List[Any]) : List[Any]={
    var c = List[Any]()
    for(i<- m.indices){
      for(j<- 0 until k){ c = m(i)::c
      }
    }
    c
  }

  def task22(n: Int): Int = {
    var a:Int = 1
    for (i <- 2 until n) {
      if (n % i == 0) {
        a=i;
      }
    }
    var c = 0
    for (w <- 0 until a.toString.length) {
      c+=a.toString.charAt(w).toString.toInt
    }
    c
  }

  def task23(k:Int, m: List[Any]) : List[Any]={
    var c = List[Any]()
    for(i<- m.indices){
      for(j<- 0 until k){
        c = m(i)::c
      }
    }
    c
  }

  def task24(n: Int, m: Int): Int ={
    n / gcd(n, m) * m
  }

  def gcd(a: Int, b: Int): Int ={
    if(b == 0){
      a
    } else {
      gcd(b,a % b);
    }
  }

  def task25(k:Int, m: List[Any]) : List[Any]={
    var c = List[Any]()
    for(i<- m.indices){
      if((i+1)%k != 0){ c = m(i)::c
      }
    }
    c.reverse
  }

  def faktorial(n:Int):Int={
    if(n<2){
      1
    } else {
      n*faktorial(n-1)
    }
  }
  def task26(n: Int, k: Int): Int ={
    faktorial(n)/faktorial(n-k)
  }

  def task27(k:Int, m: List[Any]) : List[Any]={
    var c = List[Any]()
    var i = k
    if(i>0){
      for(i<- 1 until m.length){
        c = m(i)::c
      }
      c = c.reverse
      val b = m.head
      c = c:+b
      i -= 1
    }
    else {
      for(i<- 0 until m.length-1){
        c = m(i)::c
      }
      c = c.reverse
      val a = m.last
      c = a::c
      i += 1
    }
    if(i!=0){
      return task27(i, c)
    }
    c
  }
  def summdel(n: Int): Int ={
    var c = 1
    for(v <- 2 until n){
      if(n%v==0)
        c+=v
    }
    c
  }
  def task28(n: Int): Int ={
    for(v <- 0 until n){
      if(n-v==summdel(n-v))
        return n-v
    }
    0
  }

  def task29(m: List[Any]) : Unit={
    var c1 = List[Any]()
    var c2 = List[Any]()
    for(i<- m.indices){
      if(i%2 == 0){
        c1 = m(i)::c1
      }
      else {
        c2 = m(i)::c2
      }
    }
    println(c1.reverse)
    println(c2.reverse)
  }

  def task30(n: Int): Int ={
    var count = 1
    var sum = 0
    for(i<-n to 1 by -1){
      var a = i
      while (a>0){
        sum+=a%10
        a/=10
      }
      if(sum>1){
        val b = sum
        while (sum<i){
          sum*= b
          count+=1
        }
      }
      if(sum == i && count !=1 )return sum
      sum=0
    }
    0
  }

  def task31(m: List[(Int, String)]) : Unit={
    var c1 = List[Int]()
    var c2 = List[String]()
    for(i<- m.indices){
      c1 = m(i)._1::c1
      c2 = m(i)._2::c2
    }
    println(c1.reverse)
    println(c2.reverse)
  }
}