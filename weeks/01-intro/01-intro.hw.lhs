% -*- LaTeX -*-
\documentclass{tufte-handout}
%include lhs2TeX.fmt

\title{CIS 194: Homework 1}
\date{}
\author{Due Monday, January 14}

\usepackage[stable]{footmisc} % for putting footnotes in section headings
\usepackage{../hshw}

\begin{document}

\maketitle

When solving the homework, strive to create not just code that works,
but code that is stylish and concise.  See the style guide on the
website for some general guidelines.  Try to write small functions
which perform just a single task, and then combine those smaller
pieces to create more complex functions.  Don't repeat yourself: write
one function for each logical task, and reuse functions as necessary.

Be sure to write functions with exactly the specified name and type
signature for each exercise (to help us test your code).  You may
create additional helper functions with whatever names and type
signatures you wish.

\section{Validating Credit Card Numbers\footnote{Adapted from the
    first practicum assigned in the University of Utrecht functional
    programming course taught by Doaitse Swierstra, 2008-2009.}}

Have you ever wondered how websites validate your credit card number
when you shop online? They don't check a massive database of numbers,
and they don't use magic. In fact, most credit providers rely on a
checksum formula for distinguishing valid numbers from random
collections of digits (or typing mistakes).

\begin{center}
\includegraphics[width=3in]{images/credit-card.png}  
\end{center}


In this section, you will implement the validation algorithm for
credit cards. It follows these steps:

\begin{itemize}
  \item Double the value of every second digit beginning from the
    right.  That is, the last digit is unchanged; the
    second-to-last digit is doubled; the third-to-last digit
    is unchanged; and so on.  For example, |[1,3,8,6]| becomes |[2,3,16,6]|.
  \item Add the digits of the doubled values and the undoubled digits
    from the original number.  For example, |[2,3,16,6]| becomes
    |2+3+1+6+6 = 18|.
  \item Calculate the remainder when the sum is divided by 10.  For
    the above example, the remainder would be |8|.
\end{itemize}

If the result equals 0, then the number is valid.

\exercise
We need to first find the digits of a number. Define the functions
\begin{code}
toDigits    :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
\end{code}
|toDigits| should convert positive |Integer|s to a list of
digits. (For |0| or negative inputs, |toDigits| should return the
empty list.)  |toDigitsRev| should do the same, but with the digits
reversed.

\begin{example}
  |toDigits 1234 == [1,2,3,4]|
\end{example}

\begin{example}
  |toDigitsRev 1234 == [4,3,2,1]|
\end{example}

\begin{example}
  |toDigits 0 == []|
\end{example}

\begin{example}
  |toDigits (-17) == []|
\end{example}

\exercise
Once we have the digits in the proper order, we need to double every
other one. Define a function
\begin{code}
doubleEveryOther :: [Integer] -> [Integer]
\end{code}
Remember that |doubleEveryOther| should double every other number
\emph{beginning from the right}, that is, the second-to-last,
fourth-to-last, \dots numbers are doubled.

\begin{example}
  |doubleEveryOther [8,7,6,5] == [16,7,12,5]|  
\end{example}

\begin{example}
  |doubleEveryOther [1,2,3] == [1,4,3]|
\end{example}

\exercise
The output of |doubleEveryOther| has a mix of one-digit and two-digit
numbers. Define the function
\begin{code}
sumDigits :: [Integer] -> Integer
\end{code}
to calculate the sum of all digits.

\begin{example}
  |sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22|
\end{example}

\exercise
Define the function
\begin{code}
validate :: Integer -> Bool
\end{code}
that indicates whether an |Integer| could be a valid credit card
number. This will use all functions defined in the previous exercises.

\begin{example}
  |validate 4012888888881881 = True|
\end{example}

\begin{example}
  |validate 4012888888881882 = False|
\end{example}

\section{The Towers of Hanoi\footnote{Adapted from an assignment given
    in UPenn CIS 552, taught by Benjamin Pierce}}

\begin{center}
\includegraphics[width=5in]{images/Tower_of_Hanoi.jpeg}
\end{center}

\exercise
The \term{Towers of Hanoi} is a classic puzzle with a solution that
can be described recursively.  Disks of different sizes are stacked on
three pegs; the goal is to get from a starting configuration with all
disks stacked on the first peg to an ending configuration with all
disks stacked on the last peg, as shown in \pref{fig:hanoi}.
\begin{marginfigure}[-8em]
\begin{center}
\begin{diagram}[width=150]
import Hanoi
dia = renderHanoi [[0..4], [], []] # pad 1.1
\end{diagram}

\vspace{1em}
{\LARGE $\Downarrow$}
\vspace{0.5em}

\begin{diagram}[width=150]
import Hanoi 
dia = renderHanoi [[], [], [0..4]] # pad 1.1
\end{diagram}
\end{center}
\caption{The Towers of Hanoi} \label{fig:hanoi}
\end{marginfigure}

The only rules are
\begin{itemize}
\item you may only move one disk at a time, and
\item a larger disk may never be stacked on top of a smaller one.
\end{itemize}
For example, as the first move all you can do is move the topmost,
smallest disk onto a different peg, since only one disk may be moved
at a time.
\begin{marginfigure}[-5em]
  \begin{center}
  \begin{diagram}[width=150]
import Hanoi 
dia = renderHanoi [[1..4], [0], []] # pad 1.1
  \end{diagram}
  \end{center}
  \caption{A valid first move.}
\end{marginfigure}
From this point, it is \emph{illegal} to move to the configuration
shown in \pref{fig:illegal}, because you are not allowed to put the
green disk on top of the smaller blue one.
\begin{marginfigure}[-3em]
  \begin{center}
  \begin{diagram}[width=150]
import Hanoi
dia = renderHanoi [[2..4], [1,0], []] # pad 1.1
  \end{diagram}
  \end{center}
\caption{An illegal configuration.} \label{fig:illegal}
\end{marginfigure}

To move $n$ discs (stacked in increasing size) from peg $a$ to peg $b$
using peg $c$ as temporary storage,
\begin{enumerate}
  \item move $n - 1$ discs from $a$ to $c$ using $b$ as temporary storage
  \item move the top disc from $a$ to $b$
  \item move $n - 1$ discs from $c$ to $b$ using $a$ as temporary storage.
\end{enumerate}
For this exercise, define a function |hanoi| with the following type:
\begin{code}
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
\end{code}
Given the number of discs and names for the three pegs, |hanoi| should
return a list of moves to be performed to move the stack of discs from
the first peg to the second.

Note that a |type| declaration, like |type Peg = String| above, makes
a \emph{type synonym}.  In this case |Peg| is declared as a synonym
for |String|, and the two names |Peg| and |String| can now be used
interchangeably.  Giving more descriptive names to types in this way
can be used to give shorter names to complicated types, or (as here)
simply to help with documentation.

\begin{example}
  |hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]|  
\end{example}

\exercise \opt What if there are four pegs instead of three?  That is,
the goal is still to move a stack of discs from the first peg to the
last peg, without ever placing a larger disc on top of a smaller one,
but now there are two extra pegs that can be used as ``temporary''
storage instead of only one.  Write a function similar to |hanoi|
which solves this problem in as few moves as possible.

It should be possible to do it in far fewer moves than with three
pegs.  For example, with three pegs it takes $2^{15} - 1 = 32767$
moves to transfer $15$ discs.  With four pegs it can be done in $129$
moves. (See Exercise 1.17 in Graham, Knuth, and Patashnik,
\emph{Concrete Mathematics}, second ed., Addison-Wesley, 1994.)
\end{document}


\documentclass{tufte-handout}

\usepackage{../taor}

\title{The Art of Recursion: Problem Set 1}
\date{Due Tuesday, 11 September 2012}

\begin{document}

\taortitle

Consider the following recursive definition of the \emph{factorial}
function on natural numbers:
\begin{align*}
  \fact(0) &= 1 \\
  \fact(n) &= n \cdot \fact(n-1) \quad (n > 0)
\end{align*}
It is common to use the notation $n!$ as an abbreviation for
$\fact(n)$.

\begin{enumerate}

\item Implement the above definition directly as a recursive function,
  using any programming language you like.\footnote{You probably
    want to use a ``big integer'' type that allows unlimited-size
    integers, if such a type is available.}

\item What is the big-$O$ time complexity of computing $\fact(n)$ using
  this definition?\footnote{You may pretend that multiplication is
    $O(1)$, though with arbitrarily-sized integers this is an outright
    lie.} Empirically, how large can you make $n$ and still
  reasonably compute $\fact(n)$ with your implementation?

\item Prove by induction that given any natural number $n$ as input,
  evaluation of $\fact(n)$ will finish after a finite amount of time
  (that is, $\fact(n)$ \term{terminates} for all inputs).

\item Write down a recursively defined function on the natural numbers
  which does not terminate for some (or all) inputs.  What goes wrong
  if you try to prove that it always terminates using induction?

\item Prove that $n! \geq 2^n$ for all $n \geq 4$.

\item What goes wrong if you try using induction to prove $n! \geq
  2^n$ for all $n \geq 0$?
\item What goes wrong if you try using induction to prove $n! \geq
  2^n$ for all $n \geq 2$?

\item Prove by induction: for all $n \geq 0$, \[ \sum_{k = 0}^n k
  \cdot (k!) = 0 \cdot 0! + 1 \cdot 1! + 2 \cdot 2! + \dots + n \cdot n! = (n+1)! -
  1. \]

\item Consider the following infinite sequence of positive integers,
  with the first ten terms shown: \[ 0, 1, 3, 6, 10, 15, 21, 28, 36,
  45, \dots \]
  \begin{itemize}
  \item Write down a recursive definition of this sequence.
  \item Derive a (non-recursive) formula for computing the $n$th term
    of the sequence, and prove that it gives the same
    results as your recursive definition from part (a).
  \end{itemize}

\item Write down a recursive definition for the following sequence: \[
  0,1,3,13,183,33673,1133904603,1285739649838492213 \dots \]

\end{enumerate} \bigskip

The \term{Towers of Hanoi} is a classic puzzle with a solution that
can be described recursively.  Disks of different sizes are stacked on
three pegs; the goal is to get from a starting configuration with all
disks stacked on the first peg to an ending configuration with all
disks stacked on the last peg, as shown in \pref{fig:hanoi}.
\begin{marginfigure}[-8em]
\begin{center}
\begin{diagram}[width=150]
import Hanoi
dia = renderHanoi [[0..4], [], []] # pad 1.1
\end{diagram}

\vspace{1em}
{\LARGE $\Downarrow$}
\vspace{0.5em}

\begin{diagram}[width=150]
import Hanoi 
dia = renderHanoi [[], [], [0..4]] # pad 1.1
\end{diagram}
\end{center}
\caption{The Towers of Hanoi} \label{fig:hanoi}
\end{marginfigure}

The only rules are
\begin{itemize}
\item you may only move one disk at a time, and
\item a larger disk may never be stacked on top of a smaller one.
\end{itemize}
For example, as the first move all you can do is move the topmost,
smallest disk onto a different peg, since only one disk may be moved
at a time.
\begin{marginfigure}[-5em]
  \begin{center}
  \begin{diagram}[width=150]
import Hanoi 
dia = renderHanoi [[1..4], [0], []] # pad 1.1
  \end{diagram}
  \end{center}
  \caption{A valid first move.}
\end{marginfigure}
From this point, it is \emph{illegal} to move to the configuration
shown in \pref{fig:illegal}, because you are not allowed to put the
green disk on top of the smaller blue one.
\begin{marginfigure}[-3em]
  \begin{center}
  \begin{diagram}[width=150]
import Hanoi
dia = renderHanoi [[2..4], [1,0], []] # pad 1.1
  \end{diagram}
  \end{center}
\caption{An illegal configuration.} \label{fig:illegal}
\end{marginfigure}

\begin{enumerate}[resume]
\item Describe a recursive procedure for solving the puzzle.
\item How many steps does your procedure take to solve the puzzle with
  $n$ disks?  Prove your answer by induction.

\item \pref{fig:chords} shows several circles, cut by chords into one,
  two, four, and six regions, respectively.
  \begin{figure*}
    \centering
  \begin{diagram}[width=400]
import Chords
d0 = c
d1 = c <> ch 0.5 (1/7)
d2 = c <> ch 0.5 (-1/9) <> ch 0.3 (1/20)
d3 = c <> ch 0 (1/4 + 1/41) <> ch 0.7 (1/2 + 1/19) <> ch 0.1 (1/7)
dia = hcat' with {sep = 0.5} [d0, d1, d2, d3]
    # centerX # pad 1.1 # lw 0.02
  \end{diagram}    
    \caption{Circles cut by zero, one, two, and three chords.}
    \label{fig:chords}
  \end{figure*} 

The pictures with zero, one, and two chords show the maximum
  possible number of regions (one, two, and four, respectively) which
  can be created with that many chords.  However, using three chords it
  is possible to create more regions than shown.  

  In general, what is the maximum number of regions that can be created 
  using $n$ chords?  Prove your answer.
\end{enumerate}

\end{document}
