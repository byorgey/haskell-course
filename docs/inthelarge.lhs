\documentclass{article}

%include lhs2TeX.fmt

\usepackage{url}

\newcommand{\etc}{\emph{etc.}}

\begin{document}

\begin{center}
{\LARGE CIS 194: Haskell Programming in the Large}
\end{center}
\bigskip

Throughout the semester we have concentrated on programming
assignments consisting of just one or two modules.  As you work on
your final projects you may need to use many modules, install and
interact with other packages which themselves contain many modules,
and so on.  This document is intended to guide you through the basics
of this aspect of the Haskell ecosystem.

\section{Hackage}
\label{sec:hackage}

\emph{Hackage} (\url{http://hackage.haskell.org/}) is the standard,
central repository for Haskell packages.  It contains a huge range of
all sorts of packages.\footnote{In fact, perhaps it contains \emph{too many}:
  it can be difficult to find things! An in-progress redesign of the
  site to allow better organization, searching by tags and/or
  popularity, \etc, may help with this.}  If you need some code to
accomplish a particular task, look on Hackage first to see whether a
package already exists that fits your need.

\section{\texttt{cabal}}
\label{sec:cabal-install}

The \texttt{cabal} tool, which comes as part of  the Haskell Platform,
assists in managing Haskell packages.  One of its most useful features
is the ability to automatically download and install packages from
Hackage.  For example, executing
\begin{verbatim}
cabal install pandoc
\end{verbatim}
at a command prompt (\emph{not} a \texttt{ghci} prompt) will
automatically download and compile the \texttt{pandoc} package,
\emph{along with all of its recursive dependencies}.

Try typing \texttt{cabal help} to see a list of commands that are
available.

It is not hard to make your own ``cabalized'' package which can be
uploaded to Hackage---use the \texttt{cabal init} command
to generate the initial infrastructure, then edit the \texttt{.cabal}
file which is generated. This is not required for your project but it's
a nice way to package it up for other people to try (even if you don't
upload it to Hackage).

\section{Modules}
\label{sec:modules}

So far in the course we have used module declarations like
\begin{verbatim}
module Parser where
\end{verbatim}
which suffices for simple, standalone files, but there are a few more
things you should know about modules when developing larger projects.

\subsection{Hierarchical module names}
\label{sec:hierarchical}

First, module names may be \emph{hierarchical}; that is, they may
consist of a sequence of names separated by periods, like this:
\begin{verbatim}
module Text.Pandoc.Writers.LaTeX where
\end{verbatim}
Note that GHC expects to find hierarchically-named modules in a
corresponding place in the filesystem. For example, the above module
should be in a file \texttt{Text/Pandoc/Writers/LaTeX.hs}, that is, a
file named \texttt{LaTeX.hs} contained within a \texttt{Writers}
directory which is itself a subdirectory of \texttt{Pandoc}, \dots and
so on.  This is a good way to give some organization to projects
containing many modules.

\subsection{Export and import lists}
\label{sec:export-import}

By default, everything defined in a module is \emph{exported}, that
is, made available to any other modules which import it.  However, you
can choose to explicitly export only certain things in a module with
an \emph{export list}.  An export list looks like this:
\begin{verbatim}
module My.Awesome.Module (Baz(..), Bar, mkBar) where

data Baz a = EmptyBaz | Node (Int -> Baz a)

data Bar = I Int | C Char

mkBar :: Int -> Bar
mkBar i = I (blerf i)

blerf :: Int -> Int
blerf = (+1)
\end{verbatim}
In this example, the type \texttt{Baz} is exported, along with its
constructors \texttt{EmptyBaz} and \texttt{Node} (that's what the \texttt{(..)} syntax
means---to export only some constructors you can also give an explicit
comma-separated list of them in place of the \texttt{..}).  In contrast, the
type \texttt{Bar} is exported but \emph{its constructors are not}.  So anyone
who imports \texttt{My.Awesome.Module} will be able to use and refer to
things of type \texttt{Bar} but they will not be able to directly construct
or pattern-match on them.  The function \texttt{mkBar} is exported, which
gives clients a way to indirectly construct values of type \texttt{Bar}.
Notice that \texttt{mkBar} calls \texttt{blerf} but \texttt{blerf} itself is not exported.
So clients of \texttt{My.Awesome.Module} can call \texttt{mkBar} (and hence
indirectly call \texttt{blerf}), but they cannot directly use \texttt{blerf}.

By the same token, one can have explicit \emph{import} lists, to
specify that you only want to import certain things from a module.
For example,
\begin{verbatim}
import Data.List (groupBy)
\end{verbatim}
means that you are only going to use the \texttt{groupBy} function from
\texttt{Data.List}.  This is useful to help document and keep track
of what you are actually using from each module, and also sometimes to
help prevent name clashes---see below.

\subsection{Dealing with name clashes}
\label{sec:qualified}

A problem arises if several modules export functions or types with the
same name.  There are several solutions to this problem.  For
concreteness, let's suppose modules \texttt{A} and \texttt{B} both
export a function named \texttt{foo}.

If you only need the \texttt{foo} from \texttt{A}, and you don't care
about the one from \texttt{B}, you could give an explicit import list
for \texttt{B} which doesn't include \texttt{foo}:
\begin{verbatim}
import A
import B (baz, bar)
\end{verbatim}
Sometimes this is tedious, however, if you are using \emph{lots} of
stuff from \texttt{B}.  In that case you can also specify a
\texttt{hiding} clause:
\begin{verbatim}
import A
import B hiding (foo)
\end{verbatim}
This means to import everything from \texttt{B} \emph{except for}
\texttt{foo}.

But what if you want to use both \texttt{foo}s?  In that case you need
to use \emph{qualified} imports.  For example,
\begin{verbatim}
import qualified A
import qualified B
\end{verbatim}
Now you can say \texttt{A.foo} and \texttt{B.foo} to disambiguate
which \texttt{foo} you want.  You can also do something like
\begin{verbatim}
import qualified Long.Module.Name as L
\end{verbatim}
and now you get to refer to things imported from
\texttt{Long.Module.Name} as \texttt{L.foo} instead of
\texttt{Long.Module.Name.foo}.

\end{document}
