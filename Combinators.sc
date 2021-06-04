/*
Author: Evren Keskin
Date: 02/24/2021
 */
import scala.annotation.tailrec

//Problem 1: Generic compose combinator
//Implement a generic compose combinator.
def compose[T](f: T=>T, g: T=>T): T => T = {
  def r(x: T): T = f(g(x))
  r
}

//Problem 2: Self-composition iterator combinator
//Use recursion and your solution to problem 1 to implement the self-composition iterator combinator
def selfIter[T](f: T=>T, n: Int): T => T = { //f composed with itself n times.
  if(n == 1) f
  else compose(f, selfIter(f, n - 1))
}

def inc(x: Double) = x + 1
def double(x: Double) = 2 * x

selfIter(inc, 5)(6) //start with 6, iterate incrementing 5 times, to get 11s

//Problem 3: Array Elements Tester
//Write a function called countPass that counts the number of elements
//in an array of elements of type T that pass a test of type T=>Boolean

def countPass[T]( array: Array[T], test: T => Boolean): Int = {
  if(array.isEmpty) 0
  else if(test(array.head)) 1 + countPass(array.drop(1),test)
  else countPass(array.drop(1),test)
}
//test with a less than 5 comparison
def countTest(x: Int): Boolean = x <= 5
countPass(Array(1,2,3,4,5,6,7,8,9,10), countTest) // 5 values pass this test

//Problem 4: Recursion Combinator
def recur(baseVal: Int, combiner: (Int, Int)=>Int): Int=>Int = {
  def f(n : Int) : Int = {
    if(n == 0) baseVal
    else combiner(n, f(n - 1))
  }
  f
}

def factorial(n : Int, m :Int) : Int = n * m
recur(1, factorial)(5) // gives 5! = 120


//Problem 5: DeOptionize Error Handling
//Some programmers like to handle errors by throwing exceptions,
// others like to return the optional value None.
// Implement a combinator called deOptionize that takes a unary, option-returning function,
// f, as input and converts it into a non-option returning function g that handles errors by throwing exceptions.

def deOptionize[T,D](optionFunction: D=>Option[T]) : D=>T = {
  def clearFunction(parameter: D): T = {
      optionFunction(parameter) match {
        case None => throw new Exception("DeOptionized None")
        case Some(x) => x
      }
  }
  clearFunction
}

def parseDigits(digits: String): Option[Int] =
  if (digits.matches("[0-9]*")) Some(digits.toInt) else None

//doesn't throw an error
deOptionize(parseDigits)("012345") // gives Int 12345
//will throw an error
//deOptionize(parseDigits)("012---345") // causes an expection to be thrown

//Problem 6: Iterate Parameter Function
//Write a generic combinator that will generate functions like iterSquare, but in which square is replaced by a parameter.
def iterGeneric[T](init: T, n: Int, iterated : T=>T): T = {
  var result = init
  for(_ <- 0 to n) result = iterated(result)
  result
}

def square(x: Double) = x * x
iterGeneric(2.0, 2, square) // 2.0 squared 3 times = 256.0


//Problem 7: Unit Tester
//A unit tester tests an arbitrary function f: T => S
// by applying it to a list of inputs, one at a time,
// comparing each output to the expected output,
// then returns the number of errors detected.

def unitTest[T,S](testFunction: T => S, array: Array[(T,S)]): Int = {
  if(array.isEmpty) 0 //tests finished
  else if(array.head._2 == testFunction(array.head._1)) unitTest(testFunction, array.drop(1)) //works as expected
  else 1 + unitTest(testFunction, array.drop(1)) // an error case
}

def cube(n: Int): Int = n * n * n
unitTest(cube, Array((1, 1), (2, 8), (3, 9), (4, 64), (5, 124))) //2 errors to detect

//Problem 8.1: Control Loop
//Find a tail recursive implementation of controlLoop

@tailrec
def controlLoop[S](state: S, cycle: Int, halt: (S, Int)=> Boolean, update: (S, Int)=>S): S = {
  if(halt(state,cycle)) state
  else controlLoop(update(state, cycle+1), cycle+1, halt, update)
}

//Problem 8.2: Population Growth
//A pond of amoebas reproduces asexually until the pond's carrying capacity is reached.
// More specifically, the initial population of one doubles every week until it exceeds 105.
// Use your controlLoop function to compute the size of the final population.
def amoebaUpdate(population: Double, unused: Int): Double = {
  population * 2
}
def amoebaHalt(population: Double, unused: Int): Boolean = {
  population >= 105
}

controlLoop[Double](1,0,amoebaHalt,amoebaUpdate) // reaches 128.0 before halting

//Problem 8.3: Finding Roots of Functions
//Use Newton's method and your controlLoop to complete
//if solve(f) = a, then f(a) = 0 (approximately)
def solve(f: Double=>Double): Double = { //r where |f(r)| <= delta
  val delta = 1e-8
  def approximate(guess: Double, unused:Int):Boolean = math.abs(f(guess)) <= delta
  def derivativeF(guess: Double): Double = (f(guess + delta) - f(guess))/delta
  def improve(currentGuess: Double, unused:Int) = currentGuess - f(currentGuess)/derivativeF(currentGuess)

  controlLoop(1.0, 0, approximate,improve)
}

def foo(x:Double ): Double = 2 * x * x - 10
solve(foo) //solves the x value of the given function foo near 0

//Problem 8.4: Approximating Square Roots
//Use your solve function to complete
def squareFunc(sqr: Double): Double => Double = {
  def squared(x: Double): Double = x * x - sqr
  squared
}
def squareRoot(x: Double): Double = solve(squareFunc(x) )


//test square root finder
squareRoot(25)
squareRoot(5)
squareRoot(4)
squareRoot(2)

//Problem 8.5: Approximating Cube Roots
//Use your solve function to complete
def cubeFunc(cube: Double) : Double => Double = {
  def cubed(x: Double): Double = x * x * x - cube
  cubed
}
def cubeRoot(x: Double): Double = solve(cubeFunc(x))

//test cube root finder
cubeRoot(27)
cubeRoot(3)
cubeRoot(125)
cubeRoot(5)

//Problem 8.6: Approximating Nth Roots
//Use your solve function to complete
def nFunc(number: Double, root: Int): Double => Double = {
  def nPowerValue(x: Double, n : Double): Double = {//assumes n as positive power
    if(n == 0) 1
    else x * nPowerValue(x, n - 1)
  }
  def nRaised(x: Double): Double = nPowerValue(x,root) - number
  nRaised
}

def nthRoot(x: Double, n: Int) = solve(nFunc(x,n))

//test nth root finder
nthRoot(125,3)
nthRoot(81,4)
nthRoot(81,2)
nthRoot(256,8)


//Problem 8.7:
//Assume $1.00 is invested for 1 year at 100% interest compounded monthly.
//
def compoundValue(periods: Int): Double = {
  def end(currVal: Double, cycle: Int): Boolean = cycle == periods
  val rate= 1.0 / periods
  def compound(currVal: Double, cycle : Int): Double = currVal + rate * currVal
  controlLoop(1.0,0,end,compound) // $1.00 invested
}

//test compound values with given number of compounds over a year
compoundValue(12) //compounded once a month
compoundValue(365)//compounded every day
compoundValue(365 * 24)//compounded every hour
compoundValue(365 * 24 * 60 *60)//compounded every second