/*
Author: Evren Keskin
Date: 03/02/2021
 */

import scala.annotation.tailrec

//Problem 1: Cube Sums List Processor
//Write a function that computes the sum of cubes of all odd numbers occurring in a list of integers.
//implement four versions:
//·       Iterative version
//·       Recursive version
//·       Tail-recursive version (this should be different from the previous version)
//·       pipeline version
//1: Iterative version
def sumOfCubesIterative(intList: List[Int]): Int = {
  var sum = 0
  for(num <- intList if num % 2 != 0 ) {
    sum += num * num * num
  }
  sum
}

//2: Recursive version
def sumOfCubesRecursive(intList: List[Int]): Int = {
  if(intList.isEmpty) 0
  else if(intList.head % 2 == 0) sumOfCubesRecursive(intList.drop(1))
  else intList.head * intList.head * intList.head + sumOfCubesRecursive(intList.drop(1))
}

//3: Tail-Recursive version
def sumOfCubesTailRecursive(intList: List[Int]): Int = {
  @tailrec
  def sumOfCubesHelper(intList: List[Int], sum : Int): Int = {
    if(intList.isEmpty) sum
    else if(intList.head % 2 == 0) sumOfCubesHelper(intList.drop(1), sum)
    else sumOfCubesHelper(intList.drop(1), intList.head * intList.head * intList.head + sum)
  }
  sumOfCubesHelper(intList,0)
}

//4: Pipeline Version
def odd(num : Int): Boolean = {num % 2 != 0}
def cube(num: Int): Int = {num * num * num}
def sumOfCubesPipeline(intList: List[Int]): Int = {
  //filter out evens, and map to cube values
  intList.filter(odd).map(cube).sum
}
//test all 4 versions
var testList = List(1, 2, 3, 4, 5, 6) // should be 153
sumOfCubesIterative(testList)
sumOfCubesRecursive(testList)
sumOfCubesTailRecursive(testList)
sumOfCubesPipeline(testList)


//Problem 2: List Sum of Lists
//Write a function that computes the sum of numbers in a list of lists of numbers
//implement four versions:
//·       Iterative version
//·       Recursive version
//·       Tail-recursive version (this should be different from the previous version)
//·       pipeline version
//1: Iterative version
def sumOfSumsIterative(metaList: List[List[Int]]): Int = {
  var sum = 0
  for(list <- metaList) {
    for(num <- list)
      sum += num
  }
  sum
}
//2: Recursive version
def sumOfSumsRecursive(metaList: List[List[Int]]): Int =  {
  if(metaList.isEmpty) 0
  else if(metaList.head.isEmpty) sumOfSumsRecursive(metaList.drop(1))
  else {//head list has an item to sum
    metaList.head.head + sumOfSumsRecursive(metaList.drop(1) :+ metaList.head.drop(1))
  }
}

//3: Tail-Recursive version
def sumOfSumsTailRecursive(metaList: List[List[Int]]): Int = {
  @tailrec
  def sumOfSumsHelper(metaList: List[List[Int]], sum: Int): Int =  {
    if(metaList.isEmpty) sum
    else if(metaList.head.isEmpty) sumOfSumsHelper(metaList.drop(1), sum)
    else {//head list has an item to sum
      val newNumber = metaList.head.head
      sumOfSumsHelper(metaList.drop(1) :+ metaList.head.drop(1), sum + newNumber)
    }
  }
  sumOfSumsHelper(metaList,0)
}

//4: Pipeline Version
def listSum(list : List[Int]): Int = {list.sum}
def sumOfSumsPipeline(metaList: List[List[Int]]): Int = {
  //map to list sum values, and sum the sums
  metaList.map(listSum).sum //reduce((a,b) =>  a + b)
}

var testList2 = List(List(1,2), List(3, 4), List(5, 6)) // should be 21
sumOfSumsIterative(testList2)
sumOfSumsRecursive(testList2)
sumOfSumsTailRecursive(testList2)
sumOfSumsPipeline(testList2)


//Problem 6: List Predicate Count
// Write a function that returns the number of elements in a list that satisfy a given predicate.
// (The predicate is a parameter of type T=>Boolean.)
//implement four versions:
//·       Iterative version
//·       Recursive version
//·       Tail-recursive version (this should be different from the previous version)
//·       pipeline version
//1: Iterative version
def predicateIterative[T](full: List[T], predicate: T => Boolean): Int = {
  var satisfyCount = 0
  for(item <- full if predicate(item))
  {
    satisfyCount += 1
  }
  satisfyCount
}

//2: Recursive Version
def predicateRecursive[T](full: List[T], predicate: T => Boolean): Int =
{
  if(full.isEmpty)
  {
    0
  }
  else if(predicate(full.head))
  {
    1 + predicateRecursive(full.drop(1), predicate)
  }
  else predicateRecursive(full.drop(1), predicate)
}

//3: Tail Recursive Version
def predicateTailRecursive[T](full: List[T], predicate: T => Boolean): Int = {
  @tailrec
  def predicateHelper(full: List[T], predicate: T => Boolean, count : Int): Int = {
    if(full.isEmpty)
    {
      count
    }
    else if(predicate(full.head))
    {
      predicateHelper(full.drop(1), predicate, count + 1)
    }
    else predicateHelper(full.drop(1), predicate, count)
  }
  predicateHelper(full,predicate,0)
}

//4: Pipeline Version
def predicatePipeline[T](full: List[T], predicate: T => Boolean): Int =
{
  full.count(predicate) //.filter(predicate).length
}

var testList3 = List(1, 2, 3, 4, 5, 6, 7, 8)
def lessThanFour(num: Int): Boolean = num < 4
predicateIterative(testList3, lessThanFour)
predicateRecursive(testList3, lessThanFour)
predicateTailRecursive(testList3, lessThanFour)
predicatePipeline(testList3, lessThanFour)

//Problem 7: List Predicate Satisfaction
// Write a function that returns true if all elements in a list satisfy a given predicate.
//implement four versions:
//·       Iterative version
//·       Recursive version
//·       Tail-recursive version (this should be different from the previous version)
//·       pipeline version
//1: Iterative version
def isPredicateIterative[T](list: List[T], predicate: T => Boolean): Boolean =
{
  for(item <- list if !predicate(item)) {
    return false
  }
  true
}

//2: Recursive Version
def isPredicateRecursive[T](full: List[T], predicate: T => Boolean): Boolean =
{
  if(full.isEmpty)
    true
  else if(predicate(full.head) && isPredicateRecursive(full.drop(1), predicate))
    true
  else false
}

//3: Tail Recursive Version
@tailrec
def isPredicateTailRecursive[T](full: List[T], predicate: T => Boolean): Boolean =
{
  if(full.isEmpty)
    true
  else if(predicate(full.head))
    isPredicateTailRecursive(full.drop(1), predicate)
  else false
}

//4: Pipeline Version
def isPredicatePipeline[T](full: List[T], predicate: T => Boolean): Boolean =
{
  full.length == full.count(predicate) //all elements are counted with predicate
}


var testListAboveFive = List(5, 6, 7, 8)
var testListBelowFive = List(4, 3, 2)
def lessThanFive(num: Int): Boolean = num < 5
isPredicateIterative(testListAboveFive, lessThanFive)
isPredicateIterative(testListBelowFive, lessThanFive)
isPredicateRecursive(testListAboveFive, lessThanFive)
isPredicateRecursive(testListBelowFive, lessThanFive)
isPredicateTailRecursive(testListAboveFive, lessThanFive)
isPredicateTailRecursive(testListBelowFive, lessThanFive)
isPredicatePipeline(testListAboveFive, lessThanFive)
isPredicatePipeline(testListBelowFive, lessThanFive)


//Problem 13: Streams
//Create the following streams
//·       An infinitely long stream of 1's
//·       The stream of all non-negative integers
//·       The stream of all non-negative even integers
//·       The stream of all squares of integers

def allOnesStream(): LazyList[Int] ={
  1 #:: allOnesStream()
}

def nonNegativeStream(): LazyList[Int] = {
  //helper method for increasing integers, does not have 0 as start
  def increasingIntStream(num: Int): LazyList[Int] =
    num #:: increasingIntStream(num+1)
  increasingIntStream(0) //forces start to be 0
}

def nonNegativeEvenStream(): LazyList[Int] = {
  //helper method for increasing by two, does not have 0 as start
  def increasingTwoStream(num: Int): LazyList[Int] =
    num #:: increasingTwoStream(num+2)
  increasingTwoStream(0) //forces start to be 0
}

def squareStream(num: Int): LazyList[Int] =
{
  //always increasing squares stream
  (num * num) #:: squareStream(math.abs(num) + 1)
}


allOnesStream().take(5)
nonNegativeStream().take(5)
nonNegativeEvenStream().take(5)
squareStream(0).take(5)


//Problem 15: Spell Check Pipeline
//Re-implement spellCheck without using recursion or iteration. I.e., use map, filter, reduce instead.
def spellCheck(doc: List[String], dictionary: List[String]): List[String] = {

  def isNotInDictionary(word: String): Boolean = !dictionary.contains(word)
  doc.filter(isNotInDictionary)
  //all words in doc not in dictionary
}

val dictionary = List("This", "Is", "The", "Dictionary")
val doc1 = List("This", "Is", "The")
val doc2 = List("This", "Is", "Wrong")
spellCheck(doc1, dictionary)
spellCheck(doc2, dictionary)

//Problem 16: Polynomials and Monomials
//A polynomial can be represented as a list of monomials.
// A monomial is a pair of the form (coefficient, exponent)
//Implement: evalMono and evalPoly
def evalMono(mono: (Double, Double), x: Double): Double =
{
  //result of substituting x in mono
  mono._1 * math.pow(x,mono._2)
}
def evalPoly(poly: List[(Double, Double)], x: Double): Double = {
  //result of substituting x in poly
  def mapMono(mono: (Double, Double)): Double = {
    evalMono(mono,x)
  }
  poly.map(mapMono).sum
}

val polynomial = List((3.0, 2.0), (-5.0, 0.0)) // 3x^2 - 5

evalPoly(polynomial, 0)
evalPoly(polynomial, math.sqrt(5.0/3.0))
evalPoly(polynomial, 3)