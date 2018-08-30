package eval

abstract class Exp {
}

object Exp {

	def condOf(first: SExp): SExp = {
		first match {
			case SList(cond, _) => cond
			case _ => throw new Exception()
		}
	}


	def consequenceOf(first: SExp): SExp = {
		first match {
			case SList(_, consequence) => consequence
			case _ => throw new Exception()
		}
	}

	def clausesToIfExp(clauses: SExp): Exp = clauses match {
		case SNil() => NullExp()
		case SCons(SList(SSymbol("else"), sexp), SNil()) => from(sexp)
		case SCons(first, other) => IfExp(from(condOf(first)), from(consequenceOf(first)), clausesToIfExp(other))
	}

	def from(sexp : SExp) : Exp = {
    sexp match {

      // References:
      case SSymbol(id) => RefExp(id)

      // Lambda terms:
      case SList(SSymbol("lambda"),params,body) => {
        val vars = params.toList map { case SSymbol(id) => id }
        LambdaExp(vars,from(body))
      }
       

      // Conditionals:
      case STrue() => BoolExp(true)

      case SFalse() => BoolExp(false)

      case SList(SSymbol("if"),cond,ifTrue,ifFalse) =>
       IfExp(from(cond), from(ifTrue), from(ifFalse))

			case SCons(SSymbol("cond"), clauses) =>
				clausesToIfExp(clauses)

      case SList(SSymbol("and"),a,b) =>
       AndExp(from(a),from(b))

      case SList(SSymbol("or"),a,b) =>
       OrExp(from(a),from(b))


      // Numerics:
      case SInt(value) => IntExp(value) 

      case SList(SSymbol("zero?"), arg) =>
       ZeroPExp(from(arg))

      case SList(SSymbol("-"),a,b) =>
       SubExp(from(a),from(b))

      case SList(SSymbol("+"),a,b) =>
       PlusExp(from(a), from(b))

      case SList(SSymbol("*"),a,b) =>
       TimesExp(from(a),from(b))

      case SList(SSymbol("="),a,b) =>
       EqExp(from(a), from(b))

      
     // Lists:
     case SList(SSymbol("cons"),car,cdr) =>
      ConsExp(from(car),from(cdr))

     case SList(SSymbol("car"),arg) =>
      CarExp(from(arg))

     case SList(SSymbol("cdr"),arg) =>
      CdrExp(from(arg))

     case SList(SSymbol("quote"),SList()) => 
      NullExp()

     case SList(SSymbol("pair?"),arg) => 
      PairPExp(from(arg))

     case SList(SSymbol("null?"),arg) => 
      NullPExp(from(arg))

		case SList(SSymbol("define"), arg, body) => {
			val vars = arg match { case SSymbol(id) => id }
			DefineExp(vars, from(body))
		}

     // Binding forms:
     case SList(SSymbol("let"),
                bindings,
                body) => {
        val varexps = 
         bindings.toList map {
           case SList(SSymbol(id),exp) =>
             (id,from(exp))
         }
        val (vars,exps) = varexps.unzip
        LetExp(vars,exps,from(body))
      }
    

     case SList(SSymbol("letrec"),
                SList(SList(SSymbol(fun),lambda)),
                body) =>
       LetRecExp(fun,from(lambda),from(body))
    

     // Application
     case SCons(fun,args) => 
       AppExp(from(fun), args.toList map from)


    }

  }

	def listToCons(args: List[Exp]): Exp = {
		args match {
			case Nil => NullExp()
			case ::(h, t) => ConsExp(h, listToCons(t))
		}
	}
}


/* Core lambda calculus forms. */
case class RefExp(val id : String) extends Exp {
  override def toString = id
}

case class LambdaExp(val params : List[String], val body : Exp) extends Exp {
  override def toString = 
    "(lambda (" +params.mkString(" ")+ ") " +body+ ")"
}

case class AppExp(val fun : Exp, args : List[Exp]) extends Exp {
  override def toString = 
    "(" +(fun :: args).mkString(" ")+ ")"
}


/* Scheme forms. */

// Conditionals:
case class BoolExp(val value : Boolean) extends Exp {
	override def toString: String = if (value) "#t" else "#f"
}

case class IfExp(val cond : Exp, 
                 val ifTrue : Exp, 
                 val ifFalse : Exp) extends Exp

case class AndExp(val cond1 : Exp, val cond2 : Exp) extends Exp

case class OrExp(val cond1 : Exp, val cond2 : Exp) extends Exp


// Numerics:
case class IntExp(val value : Int) extends Exp {
	override def toString: String = value.toString
}

case class ZeroPExp(val test : Exp) extends Exp

case class SubExp(val exp1 : Exp, val exp2 : Exp) extends Exp

case class EqExp(val exp1 : Exp, val exp2 : Exp) extends Exp

case class PlusExp(val exp1 : Exp, val exp2 : Exp) extends Exp

case class TimesExp(val exp1 : Exp, val exp2 : Exp) extends Exp

case class ClosureExp(params: List[String], body: Exp, env: collection.mutable.Map[String, Exp]) extends Exp


// Binding and recursion:
case class LetExp(val vars : List[String],
                  val exps : List[Exp],
                  val body : Exp) extends Exp

case class LetRecExp(val fun : String,
                     val lambda : Exp,
                     val body : Exp) extends Exp



// Lists:
case class ConsExp(val car : Exp, val cdr : Exp) extends Exp{
	override def toString: String = "(cons " + car + " " + cdr.toString() + ")"
}

case class CarExp(val arg : Exp) extends Exp
 
case class CdrExp(val arg : Exp) extends Exp

case class PairPExp(val arg : Exp) extends Exp

case class NullPExp(val arg : Exp) extends Exp

case class NullExp() extends Exp {
	override def toString: String = "'()"
}

case class DefineExp(vars: String, exp: Exp) extends Exp


