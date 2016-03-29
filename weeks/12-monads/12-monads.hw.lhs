% -*- LaTeX -*-
\documentclass{tufte-handout}
%include lhs2TeX.fmt

\usepackage{../hshw}

\title{CIS 194: Homework 11}
\date{}
\author{Due Thursday, April 5}

\begin{document}

\maketitle

\begin{itemize}
\item Files you should submit: |Risk.hs|.  You should take the version
  we have provided and add your solutions to it.
\end{itemize}

\section{Risk}

\marginnote{
  \includegraphics[width=2in]{risk.jpg}
}

The game of \emph{Risk} involves two or more players, each vying to
``conquer the world'' by moving armies around a board representing the
world and using them to conquer territories.  The central mechanic of
the game is that of one army attacking another, with dice rolls used
to determine the outcome of each battle.

The rules of the game make it complicated to determine the likelihood
of possible outcomes.  In this assignment, you will write a
\emph{simulator} which could be used by Risk players to estimate the
probabilities of different outcomes before deciding on a course of
action.

\section{The |Rand StdGen| monad}
\label{sec:rand-monad}

Since battles in Risk are determined by rolling dice, your simulator
will need some way to access a source of randomness.  Many languages
include standard functions for getting the output of a pseudorandom
number generator.  For example, in Java one can write
\begin{verbatim}
  Random randGen = new Random();
  int dieRoll = 1 + randGen.nextInt(6);
\end{verbatim}
to get a random value between $1$ and $6$ into the variable
\texttt{dieRoll}.  It may seem like we can't do this in
Haskell, because the output of \verb|randGen.nextInt(6)| may be
different each time it is called---and Haskell functions must always
yield the same outputs for the same inputs.

However, if we think about what's going on a bit more carefully, we
can see how to successfully model this in Haskell.  The Java code
first creates a \verb|Random| object called \verb|randGen|.  This
represents a \emph{pseudorandom number generator}, which remembers a
bit of state (a few numbers), and every time something like
\verb|nextInt| is called, it uses the state to (deterministically)
generate an \verb|Int| and then updates the state according to some
(deterministic) algorithm. So the numbers which are generated are not
truly random; they are in fact completely deterministic, but computed
using an algorithm which generates random-seeming output.  As long as we
initialize (\emph{seed}) the generator with some truly random data, this is
often good enough for purposes such as simulations.

In Haskell we can cerainly have pseudorandom number generator
objects.  Instead of having methods which mutate them, however, we
will have functions that take a generator and return the next
pseudorandom value \emph{along with a new generator}.  That is,
the type signature for |nextInt| would be something like
\begin{spec}
nextInt :: Generator -> (Int, Generator)
\end{spec}
However, using |nextInt| would quickly get annoying: we have to
manually pass around generators everywhere.  For example, consider
some code to generate three random |Int|s:
\begin{spec}
threeInts :: Generator -> ((Int, Int, Int), Generator)
threeInts g = ((i1, i2, i3), g''')
  where (i1, g')   = nextInt g
        (i2, g'')  = nextInt g'
        (i3, g''') = nextInt g''
\end{spec}
Ugh!  Fortunately, there is a much better way.  The
\texttt{MonadRandom}
package\footnote{\url{http://hackage.haskell.org/package/MonadRandom}}
defines a \emph{monad} which encapsulates this generator-passing
behavior.  Using it, |threeInts| can be rewritten as
\begin{spec}
threeInts :: Rand StdGen (Int, Int, Int)
threeInts = 
  getRandom >>= \i1 ->
  getRandom >>= \i2 ->
  getRandom >>= \i3 ->
  return (i1,i2,i3)
\end{spec}
The type signature says that |threeInts| is a computation in the |Rand
StdGen| monad which returns a triple of |Int|s.  |Rand StdGen|
computations implicitly pass along a pseudorandom generator of type
|StdGen| (which is defined in the standard Haskell library
|System.Random|).

\newpage
\exercise

Type \texttt{cabal install MonadRandom} at a command prompt
(\emph{not} the |ghci| prompt) to download and install the
|MonadRandom| package from Hackage.  Then visit the documentation
(\url{http://hackage.haskell.org/package/MonadRandom}).  Take a look
at the |Control.Monad.Random| module, which defines various ways to
``run'' a |Rand| computation; in particular you will eventually (at
the very end of the assignment) need to use the |evalRandIO| function.
Take a look also at the |Control.Monad.Random.Class| module, which
defines a |MonadRandom| class containing methods you can use to access
the random generator in a |Rand| computation.  For example, this is
where the |getRandom| function (used above in the |threeInts| example)
comes from.  However, you probably won't need to use these methods
directly in this assignment.

In |Risk.hs| we have provided a type
\begin{spec}
newtype DieValue = DV { unDV :: Int } 
\end{spec}
for representing the result of rolling a six-sided die.  We have also
provided an instance of |Random| for |DieValue| (allowing it to be
used with |MonadRandom|), and a definition
\begin{spec}
die :: Rand StdGen DieValue
die = getRandom
\end{spec}
which represents the random outcome of rolling a fair six-sided die.

\section{The Rules}
\label{sec:rules}

The rules of attacking in Risk are as follows.  

\begin{itemize}
\item There is an attacking army (containing some number of units) and
  a defending army (containing some number of units).
\item The attacking player may attack with up to three units at a
  time.  However, they must always leave at least one unit behind.
  That is, if they only have three total units in their army they may
  only attack with two, and so on.
\item The defending player may defend with up to two units (or only
  one if that is all they have).
\item To determine the outcome of a single battle, the attacking and
  defending players each roll one six-sided die for every unit they
  have attacking or defending.  So the attacking player rolls one,
  two, or three dice, and the defending player rolls one or two dice.
\item The attacking player sorts their dice rolls in descending
  order.  The defending player does the same.
\item The dice are then matched up in pairs, starting with the highest
  roll of each player, then the second-highest.
\item For each pair, if the attacking player's roll is higher, then one
  of the defending player's units die.  If there is a tie, or the
  defending player's roll is higher, then one of the attacking
  player's units die.
\end{itemize}

For example, suppose player A has 3 units and player B has 5.  A can
attack with only 2 units, and B can defend with 2 units.  So A rolls 2
dice, and B does the same.  Suppose A rolls a 3 and a 5, and B rolls a
4 and a 3.  After sorting and pairing up the rolls, we have
\begin{center}
\begin{tabular}{cc}
  A & B \\
  \hline
  5 & 4 \\
  3 & 3
\end{tabular}
\end{center}
A wins the first matchup (5 \emph{vs.} 4), so one of B's units dies.  The
second matchup is won by B, however (since B wins ties), so one of A's
units dies.  The end result is that now A has 2 units and B has 4.  If
A wanted to attack again they would only be able to attack with 1 unit
(whereas B would still get to defend with 2---clearly this would give
B an advantage because the \emph{higher} of B's two dice rolls will get
matched with A's single roll.)

\exercise

Given the definitions
\begin{code}
type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
\end{code}
(which are also included in |Risk.hs|), write a function with the type
\begin{spec}
battle :: Battlefield -> Rand StdGen Battlefield
\end{spec}
which simulates a single battle (as explained above) between two
opposing armies. That is, it should simulate randomly rolling the
appropriate number of dice, interpreting the results, and updating the
two armies to reflect casualties.  You may assume that each player
will attack or defend with the maximum number of units they are
allowed.

\exercise

Of course, usually an attacker does not stop after just a single
battle, but attacks repeatedly in an attempt to destroy the entire
defending army (and thus take over its territory).

Now implement a function
\begin{spec}
invade :: Battlefield -> Rand StdGen Battlefield
\end{spec}
which simulates an entire invasion attempt, that is, repeated calls to
|battle| until there are no defenders remaining, or fewer than two
attackers.

\exercise

Finally, implement a function
\begin{spec}
successProb :: Battlefield -> Rand StdGen Double
\end{spec}
which runs |invade| $1000$ times, and uses the results to compute a
|Double| between $0$ and $1$ representing the estimated probability
that the attacking army will completely destroy the defending army.
For example, if the defending army is destroyed in $300$ of the $1000$
simulations (but the attacking army is reduced to $1$ unit in the
other $700$), |successProb| should return |0.3|.

\exercise \opt

Write a function
\begin{spec}
exactSuccessProb :: Battlefield -> Double
\end{spec}
which computes the exact probability of success based on
principles of probability, without running any simulations. (This
won't give you any particular practice with Haskell; it's just a
potentially interesting challenge in probability theory.)

\end{document}

% Local Variables:
% mode:latex
% compile-command:"make hw"
% End:
