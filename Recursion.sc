/*
Author: Evren Keskin
Date: 02/16/2021
*/
import scala.annotation.tailrec

def inc(n: Int) = n + 1
def dec(n: Int) = n - 1
def isZero(n: Int) = n == 0

//Problem 1: addition
//Your definition should only use classical recursion(not tail recursion), inc, dec, and isZero.
def add(n: Int, m: Int): Int = {
  if(isZero(n)) m
  else
    {
      inc(add(dec(n), m))
    }
}


//Problem 2: multiplication
//Your definition should only use classical recursion, add, inc, dec, and isZero.
def mul(n: Int, m: Int): Int  = {
  if(isZero(n)) 0
  else
    {
      m + mul(dec(n), m)
    }
}

//Problem 3: exponential
//Your definition should only use classical recursion, add, mul, inc, dec, and isZero.
def exp(m: Int): Int  = { // = pow(2, m) // = 2^m
  //2^m
  if(isZero(m)) 1
  else
    {
      mul(2, exp(dec(m)))
    }
}

//Problem 4: Hyper-exponentiation function
//Your definition should only use classical recursion, exp, add, mul, inc, dec, and isZero.
def hyperExp(n: Int): Int = {  // exp(exp(... (exp(0)) ...)) // n-times

  if(isZero(n)) 0
  else if(isZero(dec(n))) 1 // exp(1)
  else
    {
      exp(hyperExp(dec(n)))
    }
}


//Problem 5: Problems 1-4 with tail recursion
@tailrec
def add2(n : Int, m : Int) : Int = {
  if(isZero(n)) m
  else add2(dec(n), inc(m))
}
def mul2(n: Int, m: Int): Int  = {
  @tailrec
  def mulHelper(n : Int, m : Int, r: Int): Int =
    {
      if(isZero(n)) r
      else mulHelper(dec(n), m, add2(m,r))
    }
    mulHelper(n,m, 0)
}
def exp2(m: Int): Int  = { // = pow(2, m) // = 2^m
  //2^m
  @tailrec
  def expHelper(m: Int, r : Int): Int = {
    if(isZero(m)) r
    else expHelper(dec(m), mul2(2,r))
  }
  expHelper(m,1)
}
def hyperExp2(n: Int): Int = {  // exp(exp(... (exp(0)) ...)) // n-times
  @tailrec
  def hyperHelper(n: Int, r: Int): Int = {
    if(isZero(n)) 0
    else if(isZero(dec(n))) r // exp(1)
    else
    {
      hyperHelper(dec(n), exp(r))
    }
  }
  hyperHelper(n,1)
}

//Problem 9: Classic and tail recursive Fibonacci
def fib(n : Int): Int = {
  if(isZero(n)) 0 //0 returns itself
  else if(isZero(dec(n))) 1 // 1 returns itself
  else fib(dec(n)) + fib(dec(dec(n)))
}

def fib2(n : Int): Int = {

  @tailrec
  def fibHelper(n : Int, r1 : Int, r2 : Int) : Int = {
    if(isZero(dec(n))) r1 // num 1
    else if(isZero(dec(dec(n)))) r2 //num 2
    else fibHelper(dec(n),r2,r1 + r2)
  }
  fibHelper(n,1,1)
}


//Problem 10
//choose n, m
//base case, m m > n means 0, m == n means 1
def choose(n: Int, m : Int): Int = { // # of ways to choose m things from n
  if(n == m) 1
  else if(m > n) 0
  else if(isZero(m)) 1
  else add2(choose(dec(n), m), choose(dec(n),dec(m)))
}

//Test cases

add(2,3)
mul(5,6)
exp(5)
hyperExp(5)

add2(2,3)
mul2(5,6)
exp2(5)
hyperExp2(4)

fib(7)
fib2(7)

choose(10,5)
choose(5,5)
choose(5,10)