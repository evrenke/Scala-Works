import scala.collection.immutable
import scala.util.Random
//Author: Evren Keskin
//Date: 02/09/2021

//Problem 1: "Number Kingdom"
//In numerology a number (i.e., an integer) belongs to one of several kingdoms.
// If it is an even integer bigger than 10, it belongs to kingdom 1,
// unless it is also divisible by 100, then it belongs to kingdom 2.
// Any number less than or equal to 10 belongs to the third kingdom.
// Otherwise, it is condemned to the lowly fourth kingdom.
def kingdom(n: Int): Int =
  if(n > 10)
    if(n % 100== 0) 2
    else 1
  else if(n <= 10) 3
  else 4


//Problem 2: "Number Orders"
//In numerology a positive integer belongs to one of several orders.
// (Non-positive integers are considered to belong to order 0.)
// Otherwise the order is computed using the following sacred formula:
//family(n) * class(n) + genus(n)
//·       The family of n is 1 if n is a multiple of 3, otherwise it's 2.
//·       The class of 50 is 3, the class of all other numbers is 4.
//·       The genus of a number is 5 if it is a multiple of 7, otherwise it's 6.
def order(n: Int): Int =
  if(n < 0) // non-positive number
    0
  else if(n % 3 == 0) //family of n
    if(n == 50) //class of n
      if(n % 7 == 0) // genus of n
        1 * 3 + 5
      else 1 * 3 + 6
    else if(n % 7 == 0) // genus of n
      1 * 4 + 5
    else 1 * 4 + 6
  else if(n == 50) //class of n
    if(n % 7 == 0) // genus of n
      2 * 3 + 5
    else 2 * 3 + 6
  else if(n % 7 == 0) // genus of n
    2 * 4 + 5
  else 2 * 4 + 6


//Problem 3: "Number Species"
//In numerology the species of a number is 1 if it is positive and even,
// otherwise it's 2.
// Say what's wrong with the following implementation and fix it:

//WRONG IMPLEMENTATION AND EXPLANATION
//This original implementation missed noting negative elements as species 2
//It also missed setting the return value type as Int
//def species(n: Int) =
//  if (0 < n) if (n % 2 == 0) 1 else 2

def species(n: Int): Int =
  if (0 < n)
    if (n % 2 == 0) 1
    else 2
  else 2

//Problem 4: "Elbonian Tax Calculator"
def tax(income: Double): Double = {
  if(income < 0)
    throw Exception
  if(income < 20000.0)
    0
  else if(income < 30000.0)
    0.05 * income
  else if(income < 40000.0)
    0.11 * income
  else if(income < 60000.0)
    0.23 * income
  else if(income < 100000.0)
    0.32 * income
  else 0.50 * income
}

//Problem 5: Rectangle Drawing Procedure
def drawRectangle(height : Int, width : Int)
{
  for (i <- 0 to height) {
    for (j <- 0 to width)
      print("*")
    println()
  }
}

//Problem 6: Sum Printer
def printSums(num1 : Int, num2 : Int)
{
  for(i <- 0 to num1)
    for(j <- 0 to num2)
      println(i + " + " + j + " = " + (i+j))
}

//Problem 8
//In numerology a positive integer belongs to one of 3 realms.
// All odd positive numbers belong to realm 1.
// Even positives not divisible by 3 belong to realm 2.
// Positive multiples of both 6 and 7 belong to realm 3.
// All other numbers belong to realm 0.
//Implement the following functions based on the realms

def realm1(n: Int): Int = // 1 if n belongs to realm 1, throws an exception otherwise
if(n % 2 != 0) 1
else throw Exception

def realm2(n: Int): Int = // 2 if n belongs to realm 2, throws an exception otherwise
if(n % 2 == 0)
  if(n % 3 != 0) 2
  else throw Exception
else throw Exception

def realm3(n: Int): Int = // 3 if n belongs to realm 3, throws an exception otherwise
if(n % 2 == 0)
  if(n % 6 == 0)
    if(n % 7 == 0)
      3
    else throw Exception
  else throw Exception
else throw Exception

def realm(n: Int): Int  = // the realm of n
if(n < 0) //negatives automatically in realm 0
  0
else if(n % 2 == 0) // not in realm 1
{
  if(n % 3 == 0) //not in realm 2
  {
    if(n % 6 == 0)
      if(n % 7 == 0)
        throw Exception // in realm 3
      else 0  // realm 0
    else 0 // realm 0
  }
  else throw Exception // in realm 2
}
else throw Exception // in realm 3


//Problem 9 Sqrtlog with Monatic Binding

def log(x: Double) = if (x <= 0) None else Some(math.log(x))

def sqrt(x: Double) = if (x < 0)None else Some(math.sqrt(x))

def sqrtLog(x: Double) = {
  val log = log(x)
  log match
  {
    case log: Double => sqrt(log)
    case _ => None
  }
}


//PART 2: STRING PROCESSING
//I was unable to finish this part of the assignment

//def isPal(string: String) : Boolean =
//  {
//    val str = string
//    //str = str.dropWhile( _ == ' ' || _ == '\t' || _ == '\n')
//  }



// PART 3: MATHEMATICS

def rollDice(): (Int, Int) =
  {
    val longs =(math.round(math.random() * 6 + 1), math.round(math.random() * 6 + 1)) // two 6-sided dice rolls
    (longs._1.toInt, longs._2.toInt)
  }

