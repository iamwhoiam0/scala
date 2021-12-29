package lab3


import scala.annotation.tailrec

/** Напишите свои решения в тестовых функциях.
  * 
  * Seq(1, 2) match {
  *   case head +: tail => ???
  *   case Nil          => ???
  *   case s            => ???
  * }
  * 
  * https://www.scala-lang.org/api/2.12.0/scala/collection/Seq.html
  */
// Примечание: напишите функции с хвостовой рекурсией

object Sequence {

  /* a) Найдите последний элемент Seq.
   *    
   */
  def testLastElement[A](seq: Seq[A]): Option[A] = {
    @tailrec
    def loop(seq: Seq[A]): Option[A] = seq match {
      case Nil => None
      case Seq(a) => Some(a)
      case _ :: tail => loop(tail)
    }

    loop(seq)
  }

  /* b) Объедините две Seqs (то есть Seq(1, 2) и Seq(3, 4) образуют Seq((1, 3), (2, 4))) - если Seq длиннее игнорируйте оставшиеся элементы.
   *
   */
  def testZip[A](a: Seq[A], b: Seq[A]): Seq[(A, A)] = a.zip(b)

  /* c) Проверьте, выполняется ли условие для всех элементов в Seq.
   *
   */
  def testForAll[A](seq: Seq[A])(cond: A => Boolean): Boolean = seq.forall(cond)

  /* d) Проверьте, является ли Seq палиндромом
   *
   */
  def testPalindrom[A](seq: Seq[A]): Boolean = seq.reverse.equals(seq)


  def main(args: Array[String]): Unit = {
    print("testLastElement(Seq(1, 2, 3, 4, 5)) = " + testLastElement(Seq(1, 2, 3, 4, 5)))
    print("\ntestLastElement(Nil) = " + testLastElement(Nil))

    print("\n\ntestZip(Seq(1, 2), Seq(3, 4)) = " + testZip(Seq(1, 2), Seq(3, 4)))
    print("\ntestZip(Seq(1, 2), Seq(3, 4, 5)) = " + testZip(Seq(1, 2), Seq(3, 4, 5)))
    print("\ntestZip(Seq(3, 4, 5), Nil) = " + testZip(Seq(3, 4, 5), Nil))

    print("\n\ntestForAll(Seq(3, 4, 5))((x: Int) => x < 6) = " + testForAll(Seq(3, 4, 5))((x: Int) => x < 6))
    print("\ntestForAll(Seq(3, 4, 9))((x: Int) => x < 6) = " + testForAll(Seq(3, 4, 9))((x: Int) => x < 6))
    print("\ntestForAll(Nil)((x: Int) => x < 6) = " + testForAll(Nil)((x: Int) => x < 6))

    print("\n\ntestPalindrom(Seq(1, 2, 3, 2, 1)) = " + testPalindrom(Seq(1, 2, 3, 2, 1)))
    print("\ntestPalindrom(Seq(1, 2, 3, 2, 1, 0)) = " + testPalindrom(Seq(1, 2, 3, 2, 1, 0)))

  }
}
