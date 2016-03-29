% -*- LaTeX -*-
\documentclass{tufte-handout}
%include lhs2TeX.fmt

\title{CIS 194: Homework 3}
\date{}
\author{Due Monday, Feburary 4}

\usepackage{../hshw}

\graphicspath{{images/}}

\begin{document}

\maketitle

\section{Code golf!}

\begin{center}
  \includegraphics[width=3in]{golfball}
\end{center}

This assignment is simple: there are three tasks listed below. For each
task, you should submit a Haskell function with the required name and
type signature which accomplishes the given task and is \emph{as short
  as possible}.

\section{Rules}

\begin{itemize}
\item Along with your solution for each task you \emph{must} include a
  comment explaining your solution and how it works.
  \textbf{Solutions without an explanatory comment will get a score of
    zero.}  Your comment should demonstrate a complete understanding
  of your solution.  In other words, anything is fair game but you
  must demonstrate that your understand how it works.  If in doubt,
  include more detail.

\item Comments do not count towards the length of your solutions.

\item Type signatures do not count towards the length of your
  solutions.

\item \texttt{import} statements do not count towards the length of
  your solutions.  You may import any modules included in the Haskell
  Platform.

\item \textbf{Whitespace does not count towards the length} of your
  solutions.  So there is no need to shove all your code onto one line
  and take all the spaces out.  Use space as appropriate, indent
  nicely, \emph{etc.}, but otherwise try making your code as short as
  you can.

\item You are welcome to include additional functions beyond the ones
  required.  That is, you are welcome to break up your solutions into
  several functions if you wish (indeed, sometimes this may lead to a
  very short solution).  Of course, such additional functions will be
  counted towards the length of your solution (excluding their type
  signatures).

\item Your final submission should be named |Golf.hs|.  Your file
  should define a module named |Golf|, that is, at the top of your
  file you should have
\begin{verbatim}
module Golf where
\end{verbatim}

\item The three shortest solutions (counting the total number of
  characters, excluding whitespace and the other exceptions listed
  above) for each task will receive two points of extra credit each.
  You can get up to a total of four extra credit points.

\item Otherwise, the length does not really matter; long but correct
  solutions will receive full credit for correctness (although they
  may or may not get full credit for style, depending on their style).
\end{itemize}

\section{Hints}

\begin{itemize}
\item Use functions from the standard libraries as much as
  possible---that's part of the point of this assignment.  Using (say)
  |map| is much shorter than implementing it yourself!
\item In particular, try to use functions from the standard libraries
  that encapsulate recursion patterns, rather than writing explicitly
  recursive functions yourself.
\item You may want to start by getting something that works, without
  worrying about the length.  Once you have solved the task, try to
  figure out ways to make your solution shorter.
\item If the specification of a task is unclear, feel free to ask for
  a clarification on Piazza.
\item We will test your functions on other inputs besides the ones
  given as examples, so to be safe, so should you!
\end{itemize}

\section{Tasks}

\exercise \textbf{Hopscotch}

Your first task is to write a function
\begin{code}
skips :: [a] -> [[a]]
\end{code}
The output of |skips| is a list of lists.  The first list in the
output should be the same as the input list.  The second list in the
output should contain every second element from the input list\dots
and the $n$th list in the output should contain every $n$th element
from the input list.

For example:
\begin{code}
skips "ABCD"       == ["ABCD", "BD", "C", "D"]
skips "hello!"     == ["hello!", "el!", "l!", "l", "o", "!"]
skips [1]          == [[1]]
skips [True,False] == [[True,False], [False]]
skips []           == []
\end{code}

Note that the output should be the same length as the input.

\exercise \textbf{Local maxima}

A \emph{local maximum} of a list is an element of the list which is
strictly greater than both the elements immediately before and after
it.  For example, in the list |[2,3,4,1,5]|, the only local maximum is
|4|, since it is greater than the elements immediately before and
after it (|3| and |1|).  |5| is not a local maximum since there is no
element that comes after it.

Write a function
\begin{code}
localMaxima :: [Integer] -> [Integer]
\end{code}
which finds all the local maxima in the input list and returns them in
order.  For example:

\begin{code}
localMaxima [2,9,5,6,1] == [9,6]
localMaxima [2,3,4,1,5] == [4]
localMaxima [1,2,3,4,5] == []
\end{code}

\exercise \textbf{Histogram}

For this task, write a function
\begin{code}
histogram :: [Integer] -> String
\end{code}
which takes as input a list of |Integer|s between $0$ and $9$
(inclusive), and outputs a vertical histogram showing how many of each
number were in the input list.  You may assume that the input list
does not contain any numbers less than zero or greater than $9$ (that
is, it does not matter what your function does if the input does
contain such numbers).  Your output must exactly match the output
shown in the examples below.

\newpage

\begin{code}
histogram [1,1,1,5] ==
\end{code}
\begin{verbatim}
 *
 *
 *   *
==========
0123456789
\end{verbatim}

\begin{code}
histogram [1,4,5,4,6,6,3,4,2,4,9] ==
\end{code}

\begin{verbatim}
    *
    *
    * *
 ******  *
==========
0123456789
\end{verbatim}

\textbf{Important note:} If you type something like |histogram [3,5]|
at the |ghci| prompt, you should see something like this:
\begin{verbatim}
"   * *    \n==========\n0123456789\n"
\end{verbatim}
This is a textual \emph{representation} of the |String| output,
including \verb|\n| escape sequences to indicate newline characters.
To actually visualize the histogram as in the examples above, use
|putStr|, for example, |putStr (histogram [3,5])|.

% \section{Task 3: Convolution}
% \label{sec:convolution}

% XXX

% \section{Task 4: Swizzle}
% \label{sec:swizzle}

% \begin{code}
% swizzle   :: [((a, [b]), Maybe c)] -> ([(a,b)], [Maybe c])

% unswizzle :: ([(a,b)], [Maybe c]) -> [((a, [b]), Maybe c)]
% \end{code}


\end{document}
