sealed trait Maybe[A] {
  def fold[B](empty: B, f: A => B) =
    this match {
      case Full(v) => f(v)
      case Empty() => empty
    }
  def flatMap[B](fn: A => Maybe[B]): Maybe[B] =
    this match {
      case Full(v) => fn(v)
      case Empty() => Empty[B]()
    }
//   def map[B](fn: A => B): Maybe[B] =
//     this match {
//       case Full(v) => Full(fn(v))
//       case Empty() => Empty[B]()
//     }
  def map[B](fn: A => B): Maybe[B] =
    flatMap[B](v => Full(fn(v)))
}

final case class Full[A](value: A) extends Maybe[A]
final case class Empty[A]() extends Maybe[A]

val perhaps1: Maybe[Int] = Empty[Int]
val perhaps2: Maybe[Int] = Full(1)


def mightFail1: Maybe[Int] = Full(1)
def mightFail2: Maybe[Int] = Full(2)
def mightFail3: Maybe[Int] = Empty()

mightFail1 flatMap { x =>
  mightFail2 flatMap { y =>
    mightFail3 flatMap { z =>
      Full(x + y + z)
    }
  }
}

// 5.5.4.3
val list = List(Full(3), Full(2), Full(1))
val result = list.map(maybe => maybe flatMap[Int]{x => if(x % 2 == 0) Full(x) else Empty()})

