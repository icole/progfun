def sumInt(a: Int, b: Int): Int =
  if (a > b) 0 else a + sumInt(a + 1, b)

def sumCubes(a: Int, b: Int): Int =
  if (a > b) 0 else cube(a) + sumCubes(a + 1, b)

def sumFactorials(a: Int, b: Int): Int =
  if (a > b) 0 else fact(a) + sumFactorials(a + 1, b)

// Create a more generic sum function that reuses the summing logic
def sum(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0
  else f(a) + sum(f, a + 1, b)

// Assuming the following auxillary functions
def id(x: Int): Int = x
def cube(x: Int): Int = x * x * x
def fact(x: Int): Int = if (x == 0) 1 else x + fact(x - 1)

// This allows the named functions to be rewritten as follows
sum(id, 1, 2)
sum(cube, 1, 2)
sum(fact, 1, 2)

// To prevent the need for the auxillary functions we can pass it in
def newSum(f: Int => Int): (Int, Int) => Int = {
  def sumF(a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sumF(a + 1, b)
  sumF
}

// Now we can shorten the sum functions to
newSum(x => x)(1, 2)
newSum(x => x * x * x)(1, 2)

// A tail recursive version of the sum function
def tailSum(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, f(a) + acc)
  }
  loop(a, 0)
}

// sum can be shortened
def shortSum(f: Int => Int): (Int, Int) => Int =
  if (a > b) 0 else f(a) + shortSum(f)(a + 1, b)
