package eval

object Interpreter {
	def main(args: Array[String]): Unit = {
		while(true) {
			try{
				val content = Console.readLine()
				test(content)
			} catch {
				case e: Exception => println("wow! wrong input!")
			}
		}
	}

	val emptyEnv = collection.mutable.Map[String, Exp]()

	def test(str: String) = {
		val sexp = SExp.from(str)
		val exp = Exp.from(sexp)
		println(eval(exp, emptyEnv))
	}

	case class NoValueException() extends RuntimeException
	def find(id: String, env: collection.mutable.Map[String, Exp]): Exp = {
		env.get(id) match {
			case Some(value) => value
			case None => throw NoValueException()
		}
	}

	def listOfValues(args: List[Exp], env: collection.mutable.Map[String, Exp]): List[Exp] = {
		args map (x => eval(x, env))
	}


	def pairp(arg: Exp, env: collection.mutable.Map[String, Exp]): Exp = {
		eval(arg, env) match {
			case ConsExp(_, _) => BoolExp(true)
				case _ => BoolExp(false)
		}
	}


	def nullp(arg: Exp, env: collection.mutable.Map[String, Exp]): Exp = eval(arg,env) match {
		case NullExp() => BoolExp(true)
		case _ => BoolExp(false)
	}


	def plus(exp1: Exp, exp2: Exp, env: collection.mutable.Map[String, Exp]): Exp = (eval(exp1, env), eval(exp2, env)) match {
		case (IntExp(v1), IntExp(v2)) => IntExp(v1+v2)
		case _ => throw new Exception("wrong type , expected int")
	}

	def cons(car: Exp, cdr: Exp, env: collection.mutable.Map[String, Exp]): Exp = {
		ConsExp(eval(car, env), eval(cdr, env))
	}

	def zerop(arg: Exp, env: collection.mutable.Map[String, Exp]): Exp = {
		eval(arg, env) match {
			case IntExp(0) => BoolExp(true)
			case _ => BoolExp(false)
		}
	}

	def times(exp1: Exp, exp2: Exp, env: collection.mutable.Map[String, Exp]): Exp = {
		(eval(exp1, env), eval(exp2, env)) match {
			case (IntExp(v1), IntExp(v2)) => IntExp(v1*v2)
			case _ => throw new Exception()
		}
	}


	def sub(exp1: Exp, exp2: Exp, env: collection.mutable.Map[String, Exp]): Exp = {
		(eval(exp1, env), eval(exp2, env)) match {
			case (IntExp(v1), IntExp(v2)) => IntExp(v1-v2)
				case _ => throw new Exception()
		}
	}

	def eqp(exp1: Exp, exp2: Exp, env: collection.mutable.Map[String, Exp]): Exp = {
		(eval(exp1, env), eval(exp2, env)) match {
			case (IntExp(v1), IntExp(v2)) => BoolExp(v1 == v2)
			case _ => throw new Exception()
		}
	}

	def evalDefinition(vars: String, body: Exp, env: collection.mutable.Map[String, Exp]) = {
		emptyEnv += (vars -> eval(body, env))
		NullExp()
	}

	def eval(e: Exp, env: collection.mutable.Map[String, Exp]): Exp = {
		e match {
			case RefExp(id) => find(id, env)
			case DefineExp(vars, body) => evalDefinition(vars, body, env)
			case LambdaExp(params, body) => ClosureExp(params, body, env)
			case BoolExp(value) => BoolExp(value)
			case IfExp(cond, ifTrue, ifFalse) => evalIf(cond, ifTrue, ifFalse, env)
			case IntExp(value) => IntExp(value)
			case ConsExp(car, cdr) => cons(car, cdr, env)
			case CarExp(arg) => car(arg, env)
			case CdrExp(arg) => cdr(arg, env)
			case NullExp() => NullExp()
			case PairPExp(arg) => pairp(arg, env)
			case NullPExp(arg) => nullp(arg, env)
			case PlusExp(exp1, exp2) => plus(exp1, exp2, env)
			case EqExp(exp1, exp2) => eqp(exp1, exp2, env)
			case SubExp(exp1, exp2) => sub(exp1, exp2, env)
			case TimesExp(exp1, exp2) => times(exp1, exp2, env)
			case ZeroPExp(arg) => zerop(arg, env)
			case AppExp(fun, args) => apply(eval(fun, env), listOfValues(args, env))
		}
	}

	def car(arg: Exp, env: collection.mutable.Map[String, Exp]) = eval(arg,env) match {
		case ConsExp(car, _) => car
			case _ => throw new Exception("cannot get car from a no-cons")
	}

	def cdr(arg: Exp, env: collection.mutable.Map[String, Exp]) = eval(arg,env) match {
		case ConsExp(_, cdr) => cdr
			case _ => throw new Exception("cannot get cdr from a no-cons")
	}

	def extend(params: List[String], args: List[Exp], env: collection.mutable.Map[String, Exp]) = {
		env.++(params.zip(args))
	}

	def evalSequence(body: Exp, env: collection.mutable.Map[String, Exp]): Exp = {
		eval(body, env)
	}

	def apply(procedure: Exp, args: List[Exp]): Exp = procedure match {
		case ClosureExp(params, body, env) => evalSequence(body, extend(params, args, env))
	}

	def trueP(exp: Exp): Boolean = {
		exp match {
			case BoolExp(true) => true
			case BoolExp(false) => false
			case _ => throw new Exception()
		}
	}

	def evalIf(cond: Exp, ifTrue: Exp, ifFalse: Exp, env: collection.mutable.Map[String, Exp]): Exp = {
		if (trueP(eval(cond, env))) {
			eval(ifTrue, env)
		}else {
			eval(ifFalse, env)
		}
	}
}
