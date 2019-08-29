
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

def nat_pow(x : Nat, y : Nat) : Nat = x match {
    case Zero       => one
    case Succ(Zero) => y 
    case Succ(x)    => nat_mult(y, nat_pow(x, y))
}

def nat_lte(x : Nat, y : Nat) : Bool = (x, y) match {
    case (Zero, y)          => True
    case (x, Zero)          => False
    case (Succ(x), Succ(y)) => nat_lte(x, y)
}

def nat_eq(x : Nat, y : Nat) : Bool = (x, y) match {
    case (Zero, Zero)       => True
    case (Succ(x), Succ(y)) => nat_eq(x, y)
    case _                  => False
}

// Integers

sealed trait Integer
case class Positive(x : Nat) extends Integer
case class Negative(x : Nat) extends Integer

def int_to_str(x : Integer) : String = x match {
    case Positive(x) => nat_to_str(x)
    case Negative(x) => "-" + nat_to_str(x)
}

def print_integer(x : Integer) = println(int_to_str(x))

def abs(x : Integer) : Nat = x match {
    case Positive(x) => x
    case Negative(x) => x
}

def negate(x : Integer) : Integer = x match {
    case Positive(x) => Negative(x)
    case Negative(x) => Positive(x)
}

def plus(n : Integer, m : Integer) : Integer = (n, m) match {
    case (Positive(x), Positive(y)) => Positive(nat_plus(x, y))
    case (Negative(x), Negative(y)) => Negative(nat_plus(x,y))
    case (Negative(x), Positive(y)) => nat_lte(x, y) match {
        case True  => Positive(nat_minus(y, x))
        case False => Negative(nat_minus(x, y))
    }
    case (Positive(x), Negative(y)) => nat_lte(x, y) match {
        case True  => Negative(nat_minus(y, x))
        case False => Positive(nat_minus(x, y))
    }    
}

def minus(x : Integer, y : Integer) : Integer = plus(x, negate(y))

def mult(x : Integer, y : Integer) : Integer = (x,y) match {
    case (Positive(x), Positive(y)) => Positive(nat_mult(x, y))   
    case (Negative(x), Negative(y)) => Positive(nat_mult(x, y))
    case (Negative(x), Positive(y)) => Negative(nat_mult(x, y))
    case (Positive(x), Negative(y)) => Negative(nat_mult(x, y))
}

def pow(x : Integer, y : Nat) : Integer = y match {
    case Zero => Positive(Succ(Zero))
    case Succ(y) => mult(x, pow(x, y))
}

def int_eq(x : Integer, y : Integer) : Bool = (x, y) match {
    case (Positive(x), Positive(y)) => nat_eq(x, y)
    case (Negative(x), Negative(y)) => nat_eq(x, y)
    case _                          => False
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

def bool_eq(x : Bool, y : Bool) : Bool = not(xor(x,y))

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

// Maps

sealed trait Dictionary[+A, +B] 
case object EmptyDict extends Dictionary[Nothing, Nothing]
case class Entry[A,B](key : A, value : B, xs : Dictionary[A, B]) extends Dictionary[A, B]

def isIn[A,B](eq : ((A,A) => Bool), dict : Dictionary[A,B], k : A ) : Bool = dict match {
    case EmptyDict        => False
    case Entry(k1, v, xs) => eq(k,k1) match {
        case True  => True
        case False => isIn(eq, dict, k)
    }
}

def lookup[A,B](eq : (A,A) => Bool, dict : Dictionary[A,B], k : A) : Maybe[B] = dict match {
    case EmptyDict        => Nothing
    case Entry(k1, v, xs) => eq(k,k1) match {
        case True  => Just(v)
        case False => lookup(eq, dict, k)
    }
}

// Strings

def string_eq(s1 : String, s2 : String) : Bool = if (s1 == s2) True else False


// Products

sealed trait Pair[+A, +B]
case class MkPair[A, B](fst : A, snd : B) extends Pair[A, B]

// Sums

sealed trait Either[+A, +B]
case class Left[A, B](left : A) extends Either[A, B]
case class Right[A, B](right : B) extends Either[A, B]



sealed trait Expr
case class Const(x : Integer) extends Expr
case class Bin(x : Bool) extends Expr
case class Ident(x : String) extends Expr
case class Plus(x : Expr, y : Expr) extends Expr
case class Minus(x : Expr, y : Expr) extends Expr
case class Mult(x : Expr, y : Expr) extends Expr
case class Pow(x : Expr, y : Expr) extends Expr
case class Neg(x : Expr) extends Expr
case class Eq(x : Expr, y : Expr) extends Expr
case class And(x : Expr, y : Expr) extends Expr
case class Or(x : Expr, y : Expr) extends Expr
case class IfThenElse(p : Expr, t : Expr, f : Expr) extends Expr
case class Let(id : String, x : Expr, y : Expr) extends Expr
case class FunDef(id : String, x : Expr) extends Expr
case class FunCall(x : Expr, y: Expr) extends Expr
case class LetRec(id1 : String, id2 : String, x : Expr, y : Expr ) extends Expr




sealed trait Environment
case object EmptyEnv extends Environment 
case class Extend(k : String, v : Value, env : Environment) extends Environment
case class ExtendRec(f: String, x: String, e: Expr, sigma: Environment ) extends Environment

sealed trait Value
case class NumVal(x : Integer) extends Value
case class BinVal(x : Bool) extends Value
case object Error extends Value
case class Closure(id: String, x: Expr, y: Environment) extends Value


def lookupEnv(sigma: Environment, x: String): Maybe[Value] = sigma match {
    case EmptyEnv => Nothing
    case Extend(y, v, pi) => string_eq(y,x) match {
        case True => Just(v)
        case False => lookupEnv(pi, x)
    } 
    case ExtendRec(f, y, e, pi) => string_eq(x,f) match {
        case True => Just(Closure(y, e, sigma))
        case False => lookupEnv(pi, x)
    } 
}   



def eval(env : Environment, expr : Expr) : Value = expr match {
    case Const(n)  => NumVal(n)
    case Bin(p)    => BinVal(p)
    case Ident(id) => lookupEnv(env, id) match {
        case Just(v) => v
        case Nothing => Error
    }
    case Plus(e1, e2) => eval_bin_arith(plus, env, e1, e2)
    case Minus(e1, e2) => eval_bin_arith(minus, env, e1, e2)
    case Mult(e1, e2) => eval_bin_arith(mult, env, e1, e2)
    case Pow(e1, e2)  => (eval(env, e1), eval(env,e2)) match {
        case (NumVal(x), NumVal(Positive(n))) => NumVal(pow(x, n))
        case _ => Error
    }
    case Neg(e) => eval(env, e) match {
        case NumVal(x) => NumVal(negate(x))
        case BinVal(p) => BinVal(not(p))
        case Error     => Error
    }
    case Eq(e1, e2) => (eval(env, e1), eval(env, e2)) match {
        case (NumVal(x), NumVal(y)) => BinVal(int_eq(x,y))
        case (BinVal(p), BinVal(q)) => BinVal(bool_eq(p,q))
        case _                      => Error
    }
    case And(e1, e2) => eval_bin_bool(and, env, e1, e2)
    case Or(e1, e2) => eval_bin_bool(or, env, e1, e2)
    case IfThenElse(p, e_t, e_f) => eval(env, p) match{
        case BinVal(True)  => eval(env, e_t)
        case BinVal(False) => eval(env, e_f)
        case _             => Error
    }
    case Let(id, df, body) => eval(env, df) match{
        case Error => Error
        case x     => {
            val new_env = Extend(id, x, env)
            eval(new_env , body)
        }
    }
    // FunDef, FunCall, and LetRec cases go here
    case FunDef(id, x) => Closure(id, x, env)
    case FunCall(e1, e2) => eval(env, e1) match {
        case Closure(id, e_b, pi) => {
            val new_env = Extend(id, eval(env, e2), pi)
            eval(new_env, e_b)
        }
        case Error => Error
    }
    case LetRec(name, arg, defFun, body) => {
        val new_env = ExtendRec(name, arg, defFun, env)
        eval(new_env, body)
    }
    
}

def eval_bin_arith( op : (Integer, Integer) => Integer
                  , env : Environment
                  , e1 : Expr
                  , e2 : Expr) : Value 
    = (eval(env, e1), eval(env, e2)) match{
        case (NumVal(x), NumVal(y)) => NumVal(op(x,y))
        case _ => Error
    }

def eval_bin_bool( op : (Bool, Bool) => Bool
                 , env : Environment
                 , e1 : Expr
                 , e2 : Expr) : Value 
    = (eval(env, e1), eval(env, e2)) match{
        case (BinVal(x), BinVal(y)) => BinVal(op(x,y))
        case _ => Error
    }
    
    
sealed trait Type 
case object NumType extends Type
case object BoolType extends Type
case class FunType(x : Type, y : Type) extends Type