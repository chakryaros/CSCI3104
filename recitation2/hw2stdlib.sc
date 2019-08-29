
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


