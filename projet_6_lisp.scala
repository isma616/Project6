package project6

object lisp {

// Types -------------------------------------------------------

  type Data = Any

  case class Lambda(f: List[Data] => Data)

// Parsing and Prettyprinting -----------------------------------

  class LispTokenizer(s: String) extends Iterator[String] {

    private var i = 0

    private def isDelimiter(ch: Char) = ch <= ' ' || ch == '(' || ch == ')'

    def hasNext: Boolean = {
      while (i < s.length && s.charAt(i) <= ' ') i += 1
      i < s.length
    }
    def next: String =
      if (hasNext) {
        val start = i
        if (isDelimiter(s.charAt(i))) i += 1
        else
          do i += 1
          while (i < s.length && !isDelimiter(s.charAt(i)))
        s.substring(start, i)
      } else error("premature end of input")
  }

  def string2lisp(s: String): Data = {

    val it = new LispTokenizer(s)

    def parseExpr(token: String): Data = {
      if (token == "(") parseList
      else if (token == ")") error("unbalanced parentheses")
      else if (Character.isDigit(token.charAt(0))) Integer.parseInt(token)
      else if (token.charAt(0) == '\"' &&
               token.charAt(token.length-1) == '\"')
        token.substring(1, token.length - 1)
      else Symbol(token)
    }

    def parseList: List[Data] = {
      val token = it.next
      if (token == ")") List() else parseExpr(token) :: parseList
    }
    parseExpr(it.next)
  }

  def lisp2string(x: Data): String = x match {
    case Symbol(name) =>
      name
    case xs: List[_] =>
      (xs map lisp2string).mkString("(", " ", ")")
    case _ =>
      x.toString
  }

// Diagnostics---------------------------------------------------

  var curexp: Data = null
  var trace: Boolean = false
  var indent: Int = 0

  val indentString =
    "                                                              "

  def evaluate(x: Data): Data = eval(x, globalEnv)

  def evaluate(s: String): Data = evaluate(string2lisp(s))

  def eval(x: Data, env: Environment): Data = {
    val prevexp = curexp
    curexp = x
    if (trace) {
      println(indentString.substring(0, indent) + "===> " + x)
      indent += 1
    }
    val result = eval1(x, env)
    if (trace) {
      indent -= 1
      println(indentString.substring(0, indent)+"<=== "+result)
    }
    curexp = prevexp
    result
  }

// Checked conversions ----------- -----------------------------------

  def asList(x: Data): List[Data] = x match {
    case xs: List[_] => xs
    case _ => error("malformed list: " + x)
  }

  def paramName(x: Data): String = x match {
    case Symbol(name) => name
    case _ => error("malformed parameter")
  }

// Environments -------------------------------------------------------

  abstract class Environment {
    def lookup(n: String): Data
    def extend(name: String, v: Data) = new Environment {
      def lookup(n: String): Data =
        if (n == name) v else Environment.this.lookup(n)
    }
    def extendMulti(ps: List[String], vs: List[Data]): Environment = (ps, vs) match {
      case (List(), List()) => this
      case (p :: ps1, arg :: args1) => extend(p, arg).extendMulti(ps1, args1)
      case _ => error("wrong number of arguments")
    }
    def extendRec(name: String, expr: Environment => Data) = new Environment {
      def lookup(n: String): Data =
        if (n == name) expr(this)
        else Environment.this.lookup(n)
    }
  }

  object EmptyEnvironment extends Environment {
    def lookup(n: String): Data =  error("undefined: " + n)
  }

  var globalEnv = EmptyEnvironment
    .extend("=", Lambda{
      case List(arg1, arg2) => if(arg1 == arg2) 1 else 0})
    .extend("+", Lambda{
      case List(arg1: Int, arg2: Int) => arg1 + arg2
      case List(arg1: String, arg2: String) => arg1 + arg2})
    .extend("-", Lambda{
      case List(arg1: Int, arg2: Int) => arg1 - arg2})
    .extend("*", Lambda{
      case List(arg1: Int, arg2: Int) => arg1 * arg2})
    .extend("/", Lambda{
      case List(arg1: Int, arg2: Int) => arg1 / arg2})
    .extend("nil", Nil)
    .extend("cons", Lambda{
      case List(arg1, arg2) => arg1 :: asList(arg2)})
    .extend("car", Lambda{
      case List(x :: xs) => x})
    .extend("cdr", Lambda{
      case List(x :: xs) => xs})
    .extend("null?", Lambda{
      case List(Nil) => 1
      case _ => 0})

// Parsing and PrettyprInting -----------------------------------

  def eval1(x: Data, env: Environment): Data = x match {
    case _: String =>
      x
    case _: Int =>
      x
    case Lambda(_) =>
      x
    case Symbol(name) =>
      env lookup name
    case 'val :: Symbol(name) :: expr :: rest :: Nil =>
      eval(rest, env.extend(name, eval(expr, env)))
    case 'if :: cond :: thenpart :: elsepart :: Nil =>
      if (eval(cond, env) != 0) eval(thenpart, env)
      else eval(elsepart, env)

    // sucre syntaxique

    case 'and :: x :: y :: Nil =>
      eval('if :: x :: y :: 0 :: Nil, env)
    case 'or :: x :: y :: Nil =>
      eval('if :: x :: 1 :: y :: Nil, env)
    // INSERER CODE POUR TRAITER AUTRES FORMES SYNTAXIQUES %%%

    // def, quote, lambda et application

    case 'def :: Symbol(name) :: body :: Nil => // definition GLOBAL
      if(env == globalEnv) {
        globalEnv = env.extendRec(name, env1 => eval(body, env1))
        "def "+name // just confirm we got the def
      } else
        error("trying to add global definition in some inner scope")
    case 'def :: Symbol(name) :: body :: rest :: Nil => // GLOBAL or LOCAL
      if(env == globalEnv)
        globalEnv = env.extendRec(name, env1 => eval(body, env1))
      eval(rest, env.extendRec(name, env1 => eval(body, env1))) // evaluate
    case 'def :: (Symbol(name) :: args) :: body :: rest :: Nil => // improved def: check
      val newArgs = args map(x => x match {
	      case Symbol(name)	=> name
	      case Nil		=> error("Expected argument name instead of " + x)
      })
      val newEnv = env.extendRec(name, env1 => eval(mkLambda(newArgs, body, env), env1))
      if(env == globalEnv)
        globalEnv = newEnv
      eval(rest, newEnv)
    case 'cond :: args =>
      def doCond(args: Data): Data = args match {
        case x :: Nil => x match {
            case else1 :: exprElse :: Nil =>
                eval(exprElse, env)
            case _ =>
                error("Expected (else exprElse)")
        }
        case x :: xs => x match {
            case test :: expr :: Nil =>
                if(eval(test, env) != 0) eval(expr, env)
                else doCond(xs)
            case _ =>
                error("Expected (test expr)")
        }
      }
      doCond(args)
    case 'quote :: y :: Nil =>
      y
    case 'lambda :: params :: body :: Nil =>
      mkLambda(asList(params) map paramName, body, env)
    case operator :: operands =>
      try {
        apply(eval(operator, env), operands map (x => eval(x, env)))
      } catch {
        case ex: MatchError => error("bad arguments for function "+operator)
      }
  }

  def mkLambda(ps: List[String], body: Data, env: Environment) =
    Lambda { args => eval(body, env.extendMulti(ps, args)) }

  def apply(f: Data, args: List[Data]) = f match {
    case Lambda(f) =>
      f(args)
    case _ =>
      error("application of non-function "+f+" to arguments "+args)
  }

}

object main {
  def main(args: Array[String]) {
    println("# Welcome to 'Yet Another Lisp Interpreter' (YALI)")
    println("# ")
    println("# Here's a list of builtins: ")
    println("#   (= x y)              equality test")
    println("#   (+ x y)              addition")
    println("#   (- x y)              subtraction")
    println("#   (* x y)              multiplication")
    println("#   (/ x y)              division")
    println("#   nil                  nil/null/empty list")
    println("#   (cons elem list)     prepend elem to list")
    println("#   (null? x)            nullity test")
    println("#   (def (name arg1 arg2) body test)     define a new function")
    println("#   (if condition thenexpr elseexpr)     if statement")
    println("#   (cond (cond1 expr1) (cond2 exprn) (condn exprn) (else exprelse))")
    
    // define concat
    lisp evaluate "(def (concat self xs ys) (cond ((null? xs) ys) (else (cons (car xs) (self self (cdr xs) ys)))) (concat concat (cons 1 (cons 2 nil)) (cons 3 (cons 4 nil))))"
    
    // define reverse
    lisp evaluate "(def (reverse self xs) (cond ((null? xs) nil) (else (concat concat (self self (cdr xs)) (cons (car xs) nil)))) (reverse reverse (cons 1 (cons 2 (cons 3 (cons 4 nil))))))"
    
    println("# ")
    println("# And for convenience, the following functions are available to you:")
    println("#   (concat list1 list2)     concatenate two lists")
    println("#   (reverse list1)          reverse a list")
    println("# ")
    println("# That's it! To exit, hit Ctrl+C. Have fun!")
    println()
    
    def balanced(str: String): Boolean = {
        def balanced0(chars: List[Char], count: Int): Boolean = chars match {
            case x :: xs =>
                val inc = x match {
                    case '(' =>  1
                    case ')' => -1
                    case _   =>  0
                }
                // for some reason if we replace 'inc' with the
                // match directly it.. doesn't work
                balanced0(xs, count + inc)
            case Nil =>
                (count == 0)
        }
        balanced0(str.toList, 0)
    }
    
    while(true) {
        print("yali > ")
        var line = readLine
        while(!balanced(line)) {
            print("       ")
            line = line + " " + readLine
        }
        try {
            println(lisp evaluate line)
        } catch {
            case ex: RuntimeException => println("error: " + ex.getMessage)
        }
    }
  }

}

// LispTest
