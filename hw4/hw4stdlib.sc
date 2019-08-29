
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

def nat_plus(x : Nat, y : Nat) : Nat = x match {
    case Zero    => y
    case Succ(x) => Succ(nat_plus(x, y))
}

def nat_to_int(x : Nat) : Int = x match {
    case Zero => 0
    case Succ( x ) => 1 + nat_to_int(x)
}

def nat_to_str(x : Nat) = nat_to_int(x).toString()

def print_nat(x : Nat) = println(nat_to_str(x))

def nat_mult(x : Nat, y : Nat) : Nat = x match {
    case Zero    => Zero
    case Succ(x) => nat_plus(nat_mult(x,y), y)
}


def nat_minus(x : Nat, y : Nat) : Nat = (x, y) match {
    case (Zero, _)          => Zero
    case (x, Zero)          => x
    case (Succ(x), Succ(y)) => nat_minus(x, y)
}

def nat_pow(x : Nat, y : Nat) : Nat = y match {
    case Zero       => one
    case Succ(Zero) => x 
    case Succ(y)    => nat_mult(x, nat_pow(x, y))
}

def nat_lte(x : Nat, y : Nat) : Bool = (x, y) match {
    case (Zero, y)          => True
    case (x, Zero)          => False
    case (Succ(x), Succ(y)) => nat_lte(x, y)
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

def filter[A](p : (A => Bool), xs : List[A]) : List[A] = xs match {
    case Empty       => Empty
    case Cons(x, xs) => p(x) match {
        case True  => Cons(x, filter(p, xs))
        case False => filter(p, xs)
    }
}

// Maybe

sealed trait Maybe[+A]
case object Nothing extends Maybe[Nothing]
case class Just[A](fromJust : A) extends Maybe[A]

def map_maybe[A, B](f : (A => B), m : Maybe[A]) : Maybe[B] = m match{
    case Nothing => Nothing
    case Just(x) => Just(f(x))
}

// Products

sealed trait Pair[+A, +B]
case class MkPair[A, B](fst : A, snd : B) extends Pair[A, B]

// Sums

sealed trait Either[+A, +B]
case class Left[A, B](left : A) extends Either[A, B]
case class Right[A, B](right : B) extends Either[A, B]
