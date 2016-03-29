% -*- LaTeX -*-
\documentclass{tufte-handout}
%include lhs2TeX.fmt
\usepackage{../hshw}
\usepackage{amsmath}

\newcommand{\mat}[1]{\mathbf{#1}}
\newcommand{\F}{\mat{F}}

\title{CIS 194: Homework 6}
\date{}
\author{Due Monday, February 25}

\begin{document}

\maketitle

\begin{itemize}
\item Files you should submit: \texttt{Fibonacci.hs}
\end{itemize}

This week we learned about Haskell's \emph{lazy evaluation}.  This
homework assignment will focus on one particular consequence of lazy
evaluation, namely, the ability to work with infinite data structures.

\section{Fibonacci numbers}

\begin{marginfigure}
\includegraphics[width=2in]{fibonacci-sunflower}
\end{marginfigure}

The \emph{Fibonacci numbers} $F_n$ are defined as the sequence of
integers, beginning with $0$ and $1$, where every integer in the
sequence is the sum of the previous two.  That is,

\begin{align*}
  F_0 & = 0 \\
  F_1 & = 1 \\
  F_n & = F_{n-1} + F_{n-2} \qquad (n \geq 2)
\end{align*}

For example, the first fifteen Fibonacci numbers are \[
0,1,1,2,3,5,8,13,21,34,55,89,144,233,377, \dots \]

It's quite likely that you've heard of the Fibonacci numbers before.
The reason they're so famous probably has something to do with the
simplicity of their definition combined with the astounding variety of
ways that they show up in various areas of mathematics as well as art
and nature.\footnote{
Note that you may have seen a definition where $F_0 = F_1 = 1$.  This
definition is wrong.  There are several reasons; here are two of the
most compelling:
\begin{itemize}
\item If we extend the Fibonacci sequence \emph{backwards} (using the
  appropriate subtraction), we find \[ \dots,
  -8,5,-3,2,-1,1,0,1,1,2,3,5,8 \dots \] $0$ is the obvious center of
  this pattern, so if we let $F_0 = 0$ then $F_n$ and $F_{-n}$ are
  either equal or of opposite signs, depending on the parity of $n$.
  If $F_0 = 1$ then everything is off by two.
\item If $F_0 = 0$ then we can prove the lovely theorem ``If $m$
  evenly divides $n$ if and only if $F_m$ evenly divides $F_n$.''  If
  $F_0 = 1$ then we have to state this as ``If $m$ evenly divides $n$
  if and only if $F_{m-1}$ evenly divides $F_{n-1}$.'' Ugh.
\end{itemize}
}

\exercise

Translate the above definition of Fibonacci numbers directly into a
recursive function definition of type
\begin{code}
fib :: Integer -> Integer
\end{code}
so that |fib n| computes the $n$th Fibonacci number $F_n$.

Now use |fib| to define the \emph{infinite list} of all Fibonacci numbers,
\begin{code}
fibs1 :: [Integer]
\end{code}
(\emph{Hint}: You can write the list of all positive integers as
|[0..]|.)

Try evaluating |fibs1| at the \texttt{ghci} prompt.  You will probably
get bored watching it after the first $30$ or so Fibonacci numbers,
because |fib| is ridiculously slow.  Although it is a good way to
\emph{define} the Fibonacci numbers, it is not a very good way to
\emph{compute} them---in order to compute $F_n$ it essentially ends up
adding $1$ to itself $F_n$ times!  For example, shown at right is the
tree of recursive calls made by evaluating |fib 5|.
\begin{marginfigure}
  \begin{center}
  \includegraphics[width=4in,angle=270]{fibcalls}
  \end{center}
\end{marginfigure}
As you can see, it does a lot of repeated work.  In the end, |fib| has
running time $O(F_n)$, which (it turns out) is equivalent to
$O(\varphi^n)$, where $\varphi = \frac{1 + \sqrt{5}}{2}$ is the
``golden ratio''.  That's right, the running time is
\emph{exponential} in $n$.  What's more, all this work is also
repeated from each element of the list |fibs1| to the next.  Surely we
can do better.

\exercise

When I said ``we'' in the previous sentence I actually meant ``you''.
Your task for this exercise is to come up with more efficient
implementation.  Specifically, define the infinite list
\begin{code}
fibs2 :: [Integer]
\end{code}
so that it has the same elements as |fibs1|, but computing the first
$n$ elements of |fibs2| requires only $O(n)$ addition operations.  Be
sure to use standard recursion pattern(s) from the |Prelude| as
appropriate.  \marginnote[1in]{Of course there are several billion Haskell
  implementations of the Fibonacci numbers on the web, and I have no
  way to prevent you from looking at them; but you'll probably learn a
  lot more if you try to come up with something yourself first.  }

\section{Streams}

We can be more explicit about infinite lists by defining a type
|Stream| representing lists that \emph{must be} infinite. (The usual
list type represents lists that \emph{may} be infinite but may also
have some finite length.)
\begin{marginfigure}[0.5in]
  \includegraphics[width=2in]{stream}
\end{marginfigure}
In particular, streams are like lists but with \emph{only} a
``cons'' constructor---whereas the list type has two constructors,
|[]| (the empty list) and |(:)| (cons), there is no such thing as an
\emph{empty stream}.  So a stream is simply defined as an element
followed by a stream.

\exercise

\begin{itemize}

\item Define a data type of polymorphic streams, |Stream|.

\item Write a function to convert a |Stream| to an infinite list,
\begin{code}
streamToList :: Stream a -> [a]
\end{code}

\item To test your |Stream| functions in the succeeding exercises, it will
be useful to have an instance of |Show| for |Stream|s.  However, if
you put |deriving Show| after your definition of |Stream|, as one
usually does, the resulting instance will try to print an
\emph{entire} |Stream|---which, of course, will never finish.
Instead, you should make your own instance of |Show| for |Stream|,
\begin{code}
instance Show a => Show (Stream a) where
  show ...
\end{code}
which works by showing only some prefix of a stream (say, the first
$20$ elements).\marginnote{\emph{Hint}: you may find your |streamToList|
function useful.}

\end{itemize}

\exercise

Let's create some simple tools for working with |Stream|s.

\begin{itemize}
\item Write a function
  \begin{code}
streamRepeat :: a -> Stream a
  \end{code}
which generates a stream containing infinitely many copies of the
given element.

\item Write a function
  \begin{code}
streamMap :: (a -> b) -> Stream a -> Stream b
  \end{code}
which applies a function to every element of a |Stream|.

\item Write a function
  \begin{code}
streamFromSeed :: (a -> a) -> a -> Stream a
  \end{code}
  which generates a |Stream| from a ``seed'' of type |a|, which is the
  first element of the stream, and an ``unfolding rule'' of type
%
  |a -> a| which specifies how to transform the seed into a new seed,
  to be used for generating the rest of the stream.

\end{itemize}

\exercise

Now that we have some tools for working with streams, let's create a
few:

\begin{itemize}

\item Define the stream
  \begin{code}
nats :: Stream Integer
  \end{code}
  which contains the infinite list of natural numbers $0$, $1$, $2$,
  $\dots$

\item Define the stream
  \begin{code}
ruler :: Stream Integer
  \end{code}
which corresponds to the \emph{ruler function} \[
0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,\dots \] where the $n$th element in
the stream (assuming the first element corresponds to $n=1$) is the
largest power of $2$ which evenly divides $n$.\marginnote{\emph{Hint}:
  define a function |interleaveStreams| which alternates the elements
  from two streams.  Can you use this function to implement |ruler| in
  a clever way that does not have to do any divisibility testing?}
\end{itemize}

\section{Fibonacci numbers via generating functions}

In this section, we will use streams of |Integer|s to compute the
Fibonacci numbers in an astounding (though not incredibly efficient)
way.

The essential idea is to work with \emph{generating functions} of the
form \[ a_0 + a_1 x + a_2 x^2 + \dots + a_n x^n + \dots \] where $x$
is just a ``formal parameter'' (that is, we will never actually
substitute any values for $x$; we just use it as a placeholder) and
all the coefficients $a_i$ are integers.  We will store the
coefficients $a_0, a_1, a_2, \dots$ in a |Stream Integer|.

\exercise \opt
\begin{itemize}
\item First, define
  \begin{code}
x :: Stream Integer
  \end{code}
by noting that $x = 0 + 1x + 0x^2 + 0x^3 + \dots$.

\item Define an instance of the |Num| type class for
%
  |Stream Integer|.  \marginnote{Note that you will have to add
  \verb|{-# LANGUAGE FlexibleInstances #-}| to the top of your
  \texttt{.hs} file in order for this instance to be allowed.}
  Here's what should go in your
  |Num| instance:
  \begin{itemize}
  \item You should implement the |fromInteger| function.  Note that $n
    = n + 0x + 0x^2 + 0x^3 + \dots$.
  \item You should implement |negate|: to negate a generating
    function, negate all its coefficients.
  \item You should implement |(+)|, which works like you would expect:
    $(a_0 + a_1x + a_2x^2 + \dots) + (b_0 + b_1x + b_2x^2 + \dots) =
    (a_0 + b_0) + (a_1 + b_1)x + (a_2 + b_2)x^2 + \dots$
  \item Multiplication is a bit trickier.  Suppose $A = a_0 + xA'$ and
    $B = b_0 + xB'$ are two generating functions we wish to multiply.
    We reason as follows:
    \begin{align*}
      AB &= (a_0 + xA') B \\
      & = a_0B + xA'B \\
      & = a_0 (b_0 + xB') + xA'B \\
      & = a_0b_0 + x(a_0 B' + A' B)
    \end{align*}
    That is, the first element of the product $AB$ is the product of
    the first elements, $a_0b_0$; the remainder of the coefficient
    stream (the part after the $x$) is formed by multiplying every
    element in $B'$ (that is, the tail of $B$) by $a_0$, and to this
    adding the result of multiplying $A'$ (the tail of $A$) by $B$.
  \end{itemize}

Note that there are a few methods of the |Num| class I have not told
you to implement, such as |abs| and |signum|.  \texttt{ghc} will
complain that you haven't defined them, but don't worry about it.  We
won't need those methods.  (To turn off these warnings you can add
\begin{verbatim}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
\end{verbatim}
  to the top of your file.)

If you have implemented the above correctly, you should be able to
evaluate things at the \texttt{ghci} prompt such as

\begin{verbatim}
*Main> x^4
*Main> (1 + x)^5
*Main> (x^2 + x + 3) * (x - 5)
\end{verbatim}

(a_0 + xA')/(b_0 + xB') = Q

a_0 + xA' = (b_0 + xB')Q = b_0Q + xB'Q
a_0/b_0 + xA'/b_0 = Q + xB'Q/b_0
a_0/b_0 + xA'/b_0 - xB'Q = Q

\item The penultimate step is to implement an instance of the
  |Fractional| class for |Stream Integer|. Here the important method
  to define is division, |(/)|. Suppose $A = a_0 + xA'$ and $B = b_0 +
  xB'$ and we wish to compute $A/B$.  As long as $b_0 \neq 0$, we
  can reason as follows:

  \begin{sproof}
    \stmt{Q = \frac{a_0 + xA'}{b_0 + xB'}}
    \reason{\iff}{}
    \stmt{Q(b_0 + xB') = a_0 + xA'}
    \reason{\iff}{}
    \stmt{b_0 Q + xQB' = a_0 + xA'}
    \reason{\iff}{}
    \stmt{b_0 Q = a_0 + x(A' - QB')}
    \reason{\iff}{}
    \stmt{Q = \frac{a_0}{b_0} + x \frac{A' - QB'}{b_0}}
  \end{sproof}
  That is, the first element of the quotient $Q$ is
  $a_0/b_0$; the remainder is formed by computing $A' - QB'$ and
  dividing each of its elements by $b_0$.

  Of course, in general, this operation might not result in a stream
  of |Integer|s.  However, we will only be using this instance in
  cases where it does, so just use the |div| operation where
  appropriate.

\item Consider representing the Fibonacci numbers using a generating
  function, \[ F(x) = F_0 + F_1x + F_2x^2 + F_3x^3 + \dots \] Notice
  that $x + xF(x) + x^2F(x) = F(x)$:
\[
  \begin{array}{ccccccccccc}
    & & x \\
    & & F_0 x & + & F_1 x^2 & + & F_2 x^3 & + & F_3 x^4 & + & \dots \\
    & & & & F_0 x^2 & + & F_1 x^3 & + & F_2 x^4 & + & \dots \\
    \hline 0 & + & x & + & F_2 x^2 & + & F_3 x^3 & + & F_4 x^4 & + & \dots
  \end{array}
\]

Thus $x = F(x) - xF(x) - x^2F(x)$, and solving for $F(x)$ we find
that \[ F(x) = \frac{x}{1 - x - x^2}. \] Translate this into an
(amazing, totally sweet) definition
\begin{code}
fibs3 :: Stream Integer
\end{code}

  % \begin{align*}
  %   F_{n+2} & = F_{n+1} + F_{n} \\
  %   \sum_{n \geq 0} F_{n+2} x^n & = \sum_{n \geq 0} F_{n+1} x^n + \sum_{n
  %     \geq 0} F_{n} x^n \\
  %   \sum_{n \geq 2} F_{n} x^{n+2} & = \sum_{n \geq 1} F_{n} x^{n+1} + \sum_{n
  %     \geq 0} F_{n} x^n \\
  %   x^2 \sum_{n \geq 2} F_{n} x^n & = x \sum_{n \geq 1} F_{n} x^{n} + \sum_{n
  %     \geq 0} F_{n} x^n
  % \end{align*}

\end{itemize}


\section{Fibonacci numbers via matrices (extra credit)}

It turns out that it is possible to compute the $n$th Fibonacci number
with only $O(\log n)$ (arbitrary-precision) arithmetic operations.
This section explains one way to do it.

Consider the $2 \times 2$ matrix $\F$ defined by
\newcommand{\Fm}{\begin{bmatrix}1 & 1 \\1 & 0\end{bmatrix}}
\[ \F = \Fm. \]

Notice what happens when we take successive powers of $\F$ (see
\url{http://en.wikipedia.org/wiki/Matrix_multiplication} if you forget
how matrix multiplication works):
\begin{align*}
 \F^2 & = \Fm \Fm =
\begin{bmatrix}
  1\cdot 1 + 1 \cdot 1 & 1 \cdot 1 + 1 \cdot 0 \\
  1 \cdot 1 + 0 \cdot 1 & 1 \cdot 1 + 0 \cdot 0
\end{bmatrix} =
\begin{bmatrix}
  2 & 1 \\
  1 & 1
\end{bmatrix} \\
\F^3 & =
\begin{bmatrix}
  2 & 1 \\
  1 & 1
\end{bmatrix}
\Fm
=
\begin{bmatrix}
  3 & 2 \\
  2 & 1
\end{bmatrix} \\
\F^4 & =
\begin{bmatrix}
  3 & 2 \\
  2 & 1
\end{bmatrix}
\Fm
=
\begin{bmatrix}
  5 & 3 \\
  3 & 2
\end{bmatrix} \\
\F^5 & =
\begin{bmatrix}
  5 & 3 \\
  3 & 2
\end{bmatrix}
\Fm
=
\begin{bmatrix}
  8 & 5 \\
  5 & 3
\end{bmatrix} \\
\end{align*}
Curious!  At this point we might well conjecture that Fibonacci
numbers are involved, namely, that \[ \F^n =
\begin{bmatrix}
  F_{n+1} & F_n \\
  F_n & F_{n-1}
\end{bmatrix}
\] for all $n \geq 1$.  Indeed, this is not hard to prove by
induction on $n$.

The point is that exponentiation can be implemented in logarithmic
time using a \emph{binary exponentiation} algorithm.  The idea is that
to compute $x^n$, instead of iteratively doing $n$ multiplications of
$x$, we compute \[ x^n = \begin{cases} (x^{n/2})^2 & n \text{ even}
  \\ x \cdot (x^{(n-1)/2})^2 & n \text{ odd} \end{cases} \]  where
$x^{n/2}$ and $x^{(n-1)/2}$ are recursively computed by the same
method.  Since we approximately divide $n$ in half at every iteration,
this method requires only $O(\log n)$ multiplications.

The punchline is that Haskell's exponentiation operator |(^)|
\emph{already uses} this algorithm, so we don't even have to code it
ourselves!

\newpage
\exercise \opt

\begin{itemize}
\item Create a type |Matrix| which represents $2 \times 2$ matrices of
  |Integer|s.

\item Make an instance of the |Num| type class for |Matrix|.  In fact,
  you only have to implement the |(*)| method, since that is the only
  one we will use. (If you want to play around with matrix operations
  a bit more, you can implement |fromInteger|, |negate|, and |(+)| as
  well.)  \marginnote{Don't worry about the warnings telling you that you have not
  implemented the other methods.  (If you want to disable the warnings
  you can add
\begin{verbatim}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
\end{verbatim}
  to the top of your file.)}

\item We now get fast (logarithmic time) matrix exponentiation for
  free, since |(^)| is implemented using a binary exponentiation
  algorithm in terms of |(*)|.  Write a function
  \begin{code}
fib4 :: Integer -> Integer
  \end{code}
  which computes the $n$th Fibonacci number by raising $\F$ to the
  $n$th power and projecting out $F_n$ (you will also need a special
  case for zero).  Try computing the one millionth or even ten
  millionth Fibonacci number.\marginnote{On my computer the millionth Fibonacci
  number takes only 0.32 seconds to compute but more than four seconds
  to print on the screen---after all, it has just over two
  hundred thousand digits.}
\end{itemize}

\end{document}

% Local Variables:
% mode:latex
% compile-command:"mk build"
% End:
