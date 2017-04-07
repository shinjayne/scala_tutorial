import java.lang.InheritableThreadLocal
//Datatypes.sc

//tuples
(1,2,3) : (Int, Int, Int)
var t  = (1, "a")

t._1  // Int = 1
t._2  // String = a


/*

<< Structural Types (record types) >>


 */
// -- Example !
val foo = new {
  val a = 3
  def b = a + 1
  def f(x:Int) = b+x
  def f(x:String) = "hello" + x
}

foo.b
foo.f(3)
foo.f("gil")

//above thing is same as this

object foo2{
  val a = 3
  def b = a + 1
  def f(x:Int) = b+x
  def f(x:String) = "hello" + x
}

foo2.b
foo2.f(3)
foo2.f("gil")

def g( x : {val a: Int; def b : Int ; def f(x:Int):Int ; def f(x:String):String}) = x.f(2)

g(foo2) // foo2.f(2)

// -- Scope and Type Alias
val gn  = 0
object foo3{
  val a = 3
  def b = a+1
  def f(x:Int) = b+x+gn
}

type Foo = {val a : Int ; def b:Int ; def f(x:Int):Int}

def g2(x:Foo):Int = {
  val gn = 10
  x.f(3)
}

g2(foo3)



/*

<< Algebraic Types >>
-----------------------------------------
E.g.1
Attr = Name of String
     | Age of Int
     | DOB of Int*Int*Int
     | Height of Double

Intro1:
Name(“Chulsoo Kim”), Name(“Younghee Lee”), Age(16),
DOB(2000,3,10), Height(171.5), ...

-----------------------------------------

E.g.2
IList = INil
      | ICons of Int * IList

Intro2:
INil(), ICons(3,INil(), ICons(2,ICons(1,INil()), ...

-----------------------------------------
 */

//Attr
sealed abstract class Attr
case class Name(name:String) extends Attr
case class Age(age : Int) extends Attr
case class DOB(year : Int , month : Int, day : Int) extends Attr
case class Height(height : Double) extends Attr

val a : Attr = Name("Chulsoo Kim")
val b : Attr =  DOB(2003, 3, 10)

a
b

//IList
sealed abstract class IList
case class INil() extends IList
case class ICons(hd : Int, tl : IList) extends IList

val x : IList = ICons(2, ICons(1, INil()))
def gen(n:Int) : IList = {
  if (n<= 0) INil()
  else ICons(n, gen(n))
}