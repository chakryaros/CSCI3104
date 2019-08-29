
// Nats

sealed trait Nat
case object Zero extends Nat
case class Succ(pred : Nat) extends Nat

val one = Succ(Zero)
val two = Succ(one)
val three = Succ(two)
val four = Succ(three)
val five = Succ(four)
val six = Succ(five)
val seven = Succ(six)
val eight = Succ(seven)
val nine = Succ(eight)
val ten = Succ(nine)

def plus(x : Nat, y : Nat) : Nat = x match {
    case Zero    => y
    case Succ(x) => Succ(plus(x, y))
}

def nat_to_int(x : Nat) : Int = x match {
    case Zero => 0
    case Succ( x ) => 1 + nat_to_int(x)
}

def print_nat(x : Nat) : String = nat_to_int(x).toString

def mult(x : Nat, y : Nat) : Nat = x match {
    case Zero    => Zero
    case Succ(x) => plus(mult(x,y), y)
}


def minus(x : Nat, y : Nat) : Nat = (x, y) match {
    case (Zero, _)          => Zero
    case (x, Zero)          => x
    case (Succ(x), Succ(y)) => minus(x, y)
}

def pow(x : Nat, y : Nat) : Nat = x match {
    case Zero       => one
    case Succ(Zero) => y 
    case Succ(x)    => mult(y, pow(x, y))
}

def lte(x : Nat, y : Nat) : Bool = (x, y) match {
    case (Zero, y)          => True
    case (x, Zero)          => False
    case (Succ(x), Succ(y)) => lte(x, y)
}

// Booleans

sealed trait Bool
case object True extends Bool
case object False extends Bool

def t = True
def f = False

def id(x : Bool) : Bool = x

def not(x : Bool) : Bool = x match {
    case True => False
    case False => True
}

def and(x : Bool, y : Bool) : Bool = (x,y) match {
    case (True, True) => True
    case _ => False
}

def or(x : Bool, y : Bool) : Bool = (x, y) match {
    case (False, False) => False
    case _              => True
}

def xor(x : Bool, y : Bool) : Bool = (x, y) match{
    case (True, False) => True
    case (False, True) => True
    case _ => False
}

def nand(x : Bool, y : Bool) : Bool = not(and(x,y))


// Lists

sealed trait List[+A]
case object Empty extends List[Nothing]
case class Cons[A](x : A, xs : List[A]) extends List[A]

def map[A, B](f : (A => B), xs : List[A]) : List[B] = xs match {
    case Empty       => Empty
    case Cons(x, xs) => Cons(f(x), map(f, xs))
}

def fold[A, B](f : ((A, B) => B), acc : B, xs : List[A]) : B = xs match {
    case Empty       => acc
    case Cons(x, xs) => fold(f, f(x, acc), xs)
}

// Products

sealed trait Pair[+A, +B]
case class MkPair[A, B](fst : A, snd : B) extends Pair[A, B]

// Sums

sealed trait Either[+A, +B]
case class Left[A, B](left : A) extends Either[A, B]
case class Right[A, B](right : B) extends Either[A, B]