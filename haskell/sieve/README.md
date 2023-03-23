# Sieve

Welcome to Sieve on Exercism's Haskell Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

<<<<<<< HEAD
## Introduction

You bought a big box of random computer parts at a garage sale.
You've started putting the parts together to build custom computers.

You want to test the performance of different combinations of parts, and decide to create your own benchmarking program to see how your computers compare.
You choose the famous "Sieve of Eratosthenes" algorithm, an ancient algorithm, but one that should push your computers to the limits.

## Instructions

Your task is to create a program that implements the Sieve of Eratosthenes algorithm to find prime numbers.

A prime number is a number that is only divisible by 1 and itself.
For example, 2, 3, 5, 7, 11, and 13 are prime numbers.

The Sieve of Eratosthenes is an ancient algorithm that works by taking a list of numbers and crossing out all the numbers that aren't prime.

A number that is **not** prime is called a "composite number".

To use the Sieve of Eratosthenes, you first create a list of all the numbers between 2 and your given number.
Then you repeat the following steps:

1. Find the next unmarked number in your list. This is a prime number.
2. Mark all the multiples of that prime number as composite (not prime).

You keep repeating these steps until you've gone through every number in your list.
At the end, all the unmarked numbers are prime.

```exercism/note
[Wikipedia's Sieve of Eratosthenes article][eratosthenes] has a useful graphic that explains the algorithm.

The tests don't check that you've implemented the algorithm, only that you've come up with the correct list of primes.
A good first test is to check that you do not use division or remainder operations.

[eratosthenes]: https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
```
=======
## Instructions

Use the Sieve of Eratosthenes to find all the primes from 2 up to a given
number.

The Sieve of Eratosthenes is a simple, ancient algorithm for finding all
prime numbers up to any given limit. It does so by iteratively marking as
composite (i.e. not prime) the multiples of each prime, starting with the
multiples of 2. It does not use any division or remainder operation.

Create your range, starting at two and continuing up to and including the given limit. (i.e. [2, limit])

The algorithm consists of repeating the following over and over:

- take the next available unmarked number in your list (it is prime)
- mark all the multiples of that number (they are not prime)

Repeat until you have processed each number in your range.

When the algorithm terminates, all the numbers in the list that have not
been marked are prime.

The wikipedia article has a useful graphic that explains the algorithm:
https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes

Notice that this is a very specific algorithm, and the tests don't check
that you've implemented the algorithm, only that you've come up with the
correct list of primes. A good first test is to check that you do not use
division or remainder operations (div, /, mod or % depending on the
language).
>>>>>>> e84524456529c3b098a2a285dfa7d76620e771c3

## Source

### Created by

- @etrepum

### Contributed to by

- @bartavelle
- @iHiD
- @kytrinyx
- @lpalma
- @petertseng
- @ppartarr
- @rbasso
- @sshine
- @tejasbubane

### Based on

Sieve of Eratosthenes at Wikipedia - https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes