import Distribution.Simple.Setup (trueArg)
{-

 Name: Zachary Coeur
 Uni: zjc2106

 Collaborators:

 References:

 ------------------------------

 COMS 4995 002 Parallel Function Programming

 Homework 2

 Due at 11:59 PM Sunday, October 9, 2022

 Modify this file with your solutions and submit it on Courseworks

 You may use functions, etc. from the Standard Prelude, but no
 other libraries

 Do not modify the type signatures for any of the provided functions.

 Above, include your name, UNI, list of people with whom your spoke about the
 assignment, and online references your consulted.

 Write your code alone.  You many consult the instructor, TAs, and other
 students, but do not copy/modify other's code.

 Please do not delete or modify any of the block comments below (i.e.,
 {- -} comments) as we use them to identify where your solutions begin and end.

 Feel free to add and delete single-line comments (i.e., --)

 Put any helper functions you add for a particular problem between
 the comment for the problem and the comment for the next problem

 -----

 Grading: 70% correctness: first and foremost, it needs to be correct
          30% style: is it readable, functional, concise?

 Since this homework is a bit short, its overall class weight will be
 lower than others.

 Use lts-19.23 as the "resolver" for the Haskell Tool Stack.
 E.g., stack --resolver lts-19.23 ghci  

 Your code should load under GHCi 9.0.2 with no warnings under -Wall, e.g.
 :set -Wall
 :l hw2

-}

{- For this homework, you will write a series of function that, together,
   will verify that one list is a sorted version of the other.

   The first three problems are helper functions; the fourth integrates them
   to answer the "sorted" question.

-}

{- 1) Write a function "nondecreasing" that returns true if the given list of
      ordered elements is in nondecreasing order.  Hint: use "and" and
      "zipWith" for concise code.  Your solution should reject infinite
      lists that are not nondecreasing.

      *Main> nondecreasing "abcde"
      True
      *Main> nondecreasing "abcdea"
      False
      *Main> nondecreasing "aaabccccde"
      True
      *Main> nondecreasing "aaabaccccde"
      False
      *Main> nondecreasing "a"
      True
      *Main> nondecreasing ""
      True
      *Main> nondecreasing $ "abc" ++ cycle "abc"
      False                                            
-}

nondecreasing :: Ord a => [a] -> Bool
nondecreasing x = and (zipWith (>=) (drop 1 x) x)

{- 2) Write a function "runLengths" that for a nondecreasing list,
      returns a list of (element, length) pairs, where length is the number
      of times that element appears in the list, in the order they appear in
      the list.  You may assume you are never given a list that is not
      nondecreasing.  Hint: the standard prelude function "span"

      *Main> runLengths "abcde"
      [('a',1),('b',1),('c',1),('d',1),('e',1)]
      *Main> runLengths "aaabcccde"
      [('a',3),('b',1),('c',3),('d',1),('e',1)]
      *Main> runLengths "a"
      [('a',1)]
      *Main> runLengths ""
      []
      *Main> runLengths "aaaaabcccccddddddd"
      [('a',5),('b',1),('c',5),('d',7)]
-}

runLengths :: Ord a => [a] -> [(a,Int)]
runLengths _ = [] -- Change this

{- 3) Write a function "incCount" that takes an element and an
      association list of (element, count) pairs and updates the list
      by incrementing the count for the given element.  Leave the
      order of elements in the list unchanged.  If an element x doesn't
      appear in the association list, add the pair (x,1) to the end.
      You may assume the elements in the list are unique.  Yes, this
      is a linear-time update.

      *Main> incCount 'a' []
      [('a',1)]
      *Main> incCount 'a' [('a', 3)]
      [('a',4)]
      *Main> incCount 'b' [('a', 3),('b',1),('c',10), ('f',1)]
      [('a',3),('b',2),('c',10),('f',1)]
      *Main> incCount 'z' [('a', 3),('b',1),('c',10), ('f',1)]
      [('a',3),('b',1),('c',10),('f',1),('z',1)]
      
-}

incCount :: Eq a => a -> [(a,Int)] -> [(a,Int)]
incCount _ _ = []

{- 4) Write a function "sorts" that verifies the first list is a sorted
      version of the second.  Use your "nondecreasing" function to verify
      the first list is nondecreasing, count the number of times each element
      appears in the first list using your runLengths function, then
      use "foldr" and your "incCount" function to count the number of
      appearances of each element in the second list.  Hint: use the
      list of elements you found in the first list to initialize the
      association list fed to incCount.

      *Main> "" `sorts` ""
      True
      *Main> "aaabb" `sorts` "abaab"
      True
      *Main> "aaabbc" `sorts` "abaab"
      False
      *Main> "aaabbc" `sorts` "abcaab"
      True
      *Main> "aaacbb" `sorts` "abcaab"
      False
      *Main> "abcdef" `sorts` "fedcba"
      True
      *Main> "abcdez" `sorts` "fedcba"
      False
      *Main> "abcdez" `sorts` "yedcba"
      False
      *Main> "abcdez" `sorts` "edcbay"
      False
      *Main> "abcdez" `sorts` "edcbaz"
      True
      *Main> "abcdef" `sorts` "abcdef"
      True
      *Main> "abcdefz" `sorts` "abcdef"
      False
      *Main> "abcdef" `sorts` "abcdefz"
      False
      *Main> "abcdefy" `sorts` "abcdefz"
      False      
-}

sorts :: Ord a => [a] -> [a] -> Bool
_ `sorts` _ = False -- Change this