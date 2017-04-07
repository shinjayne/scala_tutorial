
sealed abstract class IList
case class INil() extends IList
case class ICons(hd: Int, tl: IList) extends IList

sealed abstract class Exp
case class EInt(i: Int) extends Exp
case class EAdd(lhs: Exp, rhs: Exp) extends Exp
case class ESub(lhs: Exp, rhs: Exp) extends Exp
case class EMul(lhs: Exp, rhs: Exp) extends Exp



//Main.scala---------------------------------------------------------


def map(xs: IList)(f: Int => Int): IList = {
  xs match {
    case ICons(x, tl) => ICons(f(x), map(tl)(f))
    case INil() => INil()
  }

}

/*
 Exercise 2: IList Reverse
 Write a reverse function that reverses the order of the given IList.
 */
def goback(xs: IList): IList = {
  xs match {
    case ICons(x, ICons(y, tl)) =>  ICons(y, goback(ICons(x,tl)))
    case ICons(x,INil()) =>  ICons(x, INil())
  }
}

def connect(xs : IList , x : Int ) : IList = {
  xs match {
    case INil() => ICons(x, INil())
    case _ => goback(ICons(x, xs))
  }
}

def reverse(xs : IList) : IList = {
  xs match {
    case ICons(x, tl) => connect(reverse(tl), x)
    case INil()=> INil()
  }
}

/*
 Exercise 3: Exp Calculator
 Given Exp, calculate the result of Int value.
 For each case class Add/Sub/Mul, you may interpret them as
 normal integer operators: +, -, *.
 */
def calculate(x: Exp): Int = {
  x match {
    case EInt(i) => i
    case EAdd(lhs, rhs) => calculate(lhs) + calculate(rhs)
    case ESub(lhs, rhs) => calculate(lhs) - calculate(rhs)
    case EMul(lhs, rhs) => calculate(lhs) * calculate(rhs)
  }
}



//Test.scala----------------------------------------------------


def print_result(b:Boolean) : Unit =
  if (b) println("O") else println("X")

def listIntToIList(xs: List[Int]): IList =
  xs match {
    case (h :: t) => ICons(h, listIntToIList(t))
    case Nil => INil()
  }
// Problem 1

val a1 = listIntToIList(List(1, 2, 3, 4))
val b1 = listIntToIList(List(2, 4, 6, 8))
  print_result(map(a1)(_ * 2) == b1)

// Problem 2

  val a2 = listIntToIList(List(1, 2, 3, 4))
  val b2 = listIntToIList(List(4, 3, 2, 1))
  print_result(reverse(a2) == b2)


// Problem 3

  val three = EInt(3)
  val two = EInt(2)
  print_result(calculate(EAdd(two, three)) == 5)












/*
// q1--------------------------------------------------------

def map(xs: IList)(f: Int => Int): IList = {
  xs match {
    case ICons(x, tl) => ICons(f(x), map(tl)(f))
    case INil() => INil()
  }
}

val x : IList  = ICons(2, ICons(1, INil()))

map(x)(y=>y+10)

val a = listIntToIList(List(1, 2, 3, 4))
val b = listIntToIList(List(2, 4, 6, 8))

print_result(map(a)(_ * 2) == b)

// q2--------------------------------------------


def goback(xs: IList): IList = {
  xs match {
    case ICons(x, ICons(y, tl)) =>  ICons(y, goback(ICons(x,tl)))
    case ICons(x,INil()) =>  ICons(x, INil())
  }
}

def connect(xs : IList , x : Int ) : IList = {
  xs match {
    case INil() => ICons(x, INil())
    case _ => goback(ICons(x, xs))
  }
}

def reverse(xs : IList) : IList = {
  xs match {
    case ICons(x, tl) => connect(reverse(tl), x)
    case INil()=> INil()
  }
}

val a1 = listIntToIList(List(1, 2, 3, 4))
val b1 = listIntToIList(List(4, 3, 2, 1))

reverse(a1)


print_result(reverse(a1) == b1)
// q3--------------------------------------------


def calculate(x: Exp): Int = {
  x match {
    case EInt(i) => i
    case EAdd(lhs, rhs) => calculate(lhs) + calculate(rhs)
    case ESub(lhs, rhs) => calculate(lhs) - calculate(rhs)
    case EMul(lhs, rhs) => calculate(lhs) * calculate(rhs)
  }
}

val three = EInt(3)
val two = EInt(2)
print_result(calculate(EAdd(two, three)) == 5)


*/