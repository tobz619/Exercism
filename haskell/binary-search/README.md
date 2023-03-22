# Binary Search

Welcome to Binary Search on Exercism's Haskell Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

<<<<<<< HEAD
## Introduction

You have stumbled upon a group of mathematicians who are also singer-songwriters.
They have written a song for each of their favorite numbers, and, as you can imagine, they have a lot of favorite numbers.

You are curious to hear the song for your favorite number, but with so many songs to wade through, finding the right song could take a while.
Fortunately, they have organized their songs in a playlist sorted by the title — which is simply the number that the song is about.

You realize that you can use a binary search algorithm to quickly find a song given the title.

## Instructions

Your task is to implement a binary search algorithm.

A binary search algorithm finds an item in a list by repeatedly splitting it in half, only keeping the half which contains the item we're looking for.
It allows us to quickly narrow down the possible locations of our item until we find it, or until we've eliminated all possible locations.

```exercism/caution
Binary search only works when a list has been sorted.
```

The algorithm looks like this:

- Divide the sorted list in half and compare the middle element with the item we're looking for.
- If the middle element is our item, then we're done.
- If the middle element is greater than our item, we can eliminate that number and all the numbers **after** it.
- If the middle element is less than our item, we can eliminate that number and all the numbers **before** it.
- Repeat the process on the part of the list that we kept.

Here's an example:

Let's say we're looking for the number 23 in the following sorted list: `[4, 8, 12, 16, 23, 28, 32]`.

- We start by comparing 23 with the middle element, 16.
- Since 23 is greater than 16, we can eliminate the left half of the list, leaving us with `[23, 28, 32]`.
- We then compare 23 with the new middle element, 28.
- Since 23 is less than 28, we can eliminate the right half of the list: `[23]`.
- We've found our item.
=======
## Instructions

Implement a binary search algorithm.

Searching a sorted collection is a common task. A dictionary is a sorted
list of word definitions. Given a word, one can find its definition. A
telephone book is a sorted list of people's names, addresses, and
telephone numbers. Knowing someone's name allows one to quickly find
their telephone number and address.

If the list to be searched contains more than a few items (a dozen, say)
a binary search will require far fewer comparisons than a linear search,
but it imposes the requirement that the list be sorted.

In computer science, a binary search or half-interval search algorithm
finds the position of a specified input value (the search "key") within
an array sorted by key value.

In each step, the algorithm compares the search key value with the key
value of the middle element of the array.

If the keys match, then a matching element has been found and its index,
or position, is returned.

Otherwise, if the search key is less than the middle element's key, then
the algorithm repeats its action on the sub-array to the left of the
middle element or, if the search key is greater, on the sub-array to the
right.

If the remaining array to be searched is empty, then the key cannot be
found in the array and a special "not found" indication is returned.

A binary search halves the number of items to check with each iteration,
so locating an item (or determining its absence) takes logarithmic time.
A binary search is a dichotomic divide and conquer search algorithm.
>>>>>>> e84524456529c3b098a2a285dfa7d76620e771c3

Haskell has support for many types of arrays. This exercise uses immutable,
boxed, non-strict arrays from `Data.Array`. You can read more about the use of
these arrays on:

- [A Gentle Introduction to Haskell: Arrays][1]
- The documentation for [`Data.Array`][2]

As an optional extension to this exercise, try and make the `find` function
work for arrays with arbitrary bounds, e.g. arrays for which the first index is
not necessarily 0.

[1]: https://www.haskell.org/tutorial/arrays.html
[2]: https://hackage.haskell.org/package/array/docs/Data-Array.html

## Source

### Created by

- @sshine

### Contributed to by

- @iHiD
- @petertseng
- @ppartarr
- @tejasbubane

### Based on

Wikipedia - https://en.wikipedia.org/wiki/Binary_search_algorithm