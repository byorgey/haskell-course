% -*- LaTeX -*-
\documentclass{tufte-handout}
%include lhs2TeX.fmt

\usepackage{../hshw}

\title{CIS 194: Homework 4}
\date{}
\author{Due Monday, February 11}

\begin{document}

\maketitle

\textbf{What to turn in}: you should turn a single |.hs| (or |.lhs|)
file, which \textbf{must type check}.


\exerciset{Wholemeal programming}

Reimplement each of the following functions in a more idiomatic
Haskell style.  Use \emph{wholemeal programming} practices, breaking
each function into a pipeline of incremental transformations to an
entire data structure. Name your functions |fun1'| and |fun2'|
respectively.

\begin{enumerate}
\item
\begin{code}
fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs
\end{code}

\item
\begin{code}
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)
\end{code}

Hint: For this problem you may wish to use the functions |iterate| and
|takeWhile|.  Look them up in the Prelude documentation to see what
they do.
\end{enumerate}


\exerciset{Folding with trees}

Recall the definition of a \term{binary tree} data
structure\marginnote{\url{http://en.wikipedia.org/wiki/Binary\_tree}}. The
\term{height} of a binary tree is the length of a path from the root
to the deepest node.  For example, the height of a tree with a single
node is $0$; the height of a tree with three nodes, whose root has two
children, is $1$; and so on.  A binary tree is \term{balanced} if the
height of its left and right subtrees differ by no more than $1$, and
its left and right subtrees are also balanced.

You should use the following data structure to represent binary trees.
Note that each node stores an extra |Integer| representing the height
at that node.

\begin{code}
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)
\end{code}

For this exercise, write a function
\begin{spec}
foldTree :: [a] -> Tree a
foldTree = ...
\end{spec}
which generates a balanced binary tree from a list of values using
|foldr|.

For example, one sample output might be the following, also visualized
at right:
\begin{marginfigure}[1in]
  \begin{diagram}[width=150]
import Data.Tree
import Diagrams.TwoD.Layout.Tree

data HTree a = Leaf
             || HNode Integer (HTree a) a (HTree a)
  deriving (Show, Eq)

t :: HTree Char
t = HNode 3
    (HNode 2
      (HNode 0 Leaf 'F' Leaf) 
      'I'
      (HNode 1 (HNode 0 Leaf 'B' Leaf) 'C' Leaf))
    'J'
    (HNode 2
      (HNode 1 (HNode 0 Leaf 'A' Leaf) 'G' Leaf)
      'H'
      (HNode 1 (HNode 0 Leaf 'D' Leaf) 'E' Leaf))

htree2tree :: HTree a -> Tree (Maybe (Integer,a))
htree2tree Leaf = Node Nothing []
htree2tree (HNode h l a r) = Node (Just (h,a)) (map htree2tree [l,r])

drawHTree 
  = renderTree' drawNode drawEdge
  . symmLayout' with { slHSep = 4, slVSep = 4 }
  . htree2tree

drawNode Nothing = mempty
drawNode (Just (h,a)) = text [a] <> circle 1 # fc white

drawEdge _ (Nothing,_) = mempty
drawEdge (_,p1) (_,p2) = p1 ~~ p2

dia = drawHTree t # centerXY # pad 1.1
  \end{diagram}
\end{marginfigure}
\begin{code}
foldTree "ABCDEFGHIJ" ==
  Node 3
    (Node 2
      (Node 0 Leaf 'F' Leaf) 
      'I'
      (Node 1 (Node 0 Leaf 'B' Leaf) 'C' Leaf))
    'J'
    (Node 2
      (Node 1 (Node 0 Leaf 'A' Leaf) 'G' Leaf)
      'H'
      (Node 1 (Node 0 Leaf 'D' Leaf) 'E' Leaf))
\end{code}

Your solution might not place the nodes in the same exact order, but
it should result in balanced trees, with each subtree having a correct
computed height.

\exerciset{More folds!}

\begin{enumerate}
\item Implement a function
\begin{code}
xor :: [Bool] -> Bool
\end{code}
which returns |True| if and only if there are an odd number of |True|
values contained in the input list.  It does not matter how many
|False| values the input list contains.  For example,

|xor [False, True, False] == True|

|xor [False, True, False, False, True] == False|

Your solution must be implemented using a fold.

\item Implement |map| as a fold.  That is, complete the definition

\begin{code}
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ...
\end{code}

  in such a way that |map'| behaves identically to the standard |map|
  function.

\item \opt Implement |foldl| using |foldr|. That is, complete the definition

\begin{code}
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr ...
\end{code}

  in such a way that |myFoldl| behaves identically to the standard |foldl|
  function.

  Hint: Study how the application of |foldr| and |foldl| work out:
\begin{code}
foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)
foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
\end{code}

\end{enumerate}

\exerciset{Finding primes}

Read about the \term{Sieve of
  Sundaram}\marginnote{\url{http://en.wikipedia.org/wiki/Sieve_of_Sundaram}}.
Implement the algorithm using function composition.  Given an integer $n$, your
function should generate all the odd prime numbers up to $2n + 2$.

\begin{spec}
sieveSundaram :: Integer -> [Integer]
sieveSundaram = ...
\end{spec}

To give you some help, below is a function to compute the
\term{Cartesian product} of two lists. This is similar to |zip|, but
it produces all possible pairs instead of matching up the list
elements. For example, 
\begin{spec}
cartProd [1,2] ['a','b'] == [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
\end{spec}
It's written using a \term{list comprehension}, which we haven't
talked about in class (but feel free to research them).
\begin{code}
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

\end{code}

\end{document}
