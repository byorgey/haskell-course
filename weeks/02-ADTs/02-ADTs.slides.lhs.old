% -*- LaTeX -*-
\documentclass{beamer}

%include polycode.fmt

\mode<presentation>
{
 % \usetheme{Luebeck}
 
  \usetheme{default}
}

\usepackage{graphicx}
\usepackage{fancyvrb}

% \setbeameroption{show notes}
\usenavigationsymbolstemplate{}

\title{CIS 194: Haskell}
\author{Brent Yorgey}
\date{Week 2: January 19, 2012}

\AtBeginSection[]
{
  \begin{frame}<beamer>
    \frametitle{}
    \begin{center}
      {\Huge \insertsectionhead}
    \end{center}
  \end{frame}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{document}

{
  \usebackgroundtemplate{
    \parbox[c][\paperheight][c]{\paperwidth}{\centering\includegraphics[width=3in]{images/haskell-logo-light.png}}
  }

\begin{frame}
  \titlepage
\end{frame}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{
  \usebackgroundtemplate{
    \parbox[c][\paperheight][c]{\paperwidth}{\centering\includegraphics[width=2in]{images/count-light.jpg}}
  }

\section{Enumeration types}
}

\begin{frame}{A few Things}
\begin{code}
data Thing = Shoe 
           | Ship 
           | SealingWax 
           | Cabbage 
           | King
  deriving Show
\end{code}

\onslide<2->
\begin{code}
shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]
\end{code}
\end{frame}

\begin{frame}{Functions on Things}
\begin{code}
isSmall :: Thing -> Bool
isSmall Shoe       = True
isSmall Ship       = False
isSmall SealingWax = True
isSmall Cabbage    = True
isSmall King       = False
\end{code}
\end{frame}

\begin{frame}{Functions on Things, take II}
\begin{code}
isSmall2 :: Thing -> Bool
isSmall2 Ship = False
isSmall2 King = False
isSmall2 _    = True
\end{code}
\end{frame}

{
  \usebackgroundtemplate{
    \parbox[c][\paperheight][c]{\paperwidth}{\centering\includegraphics[width=3in]{images/algebra-light.jpg}}
  }

\section{Algebraic data types}
}

\begin{frame}{Not just an enumeration}
\begin{code}
data FailableDouble = Failure
                    | OK Double
  deriving Show
\end{code}

\onslide<2->
\begin{code}
fd1, fd2 :: FailableDouble
fd1 = Failure
fd2 = OK 3.4
\end{code}
\end{frame}

\begin{frame}{Working with FailableDoubles}
\begin{code}
safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)
\end{code}

\onslide<2->
\begin{code}
failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d
\end{code}
\end{frame}

\begin{frame}{Multiple-argument constructors}
\begin{code}
-- Store a person's name, age, and favourite Thing.
data Person = Person String Int Thing
  deriving Show

brent :: Person
brent = Person "Brent" 30 SealingWax

stan :: Person
stan  = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a
\end{code}
\end{frame}

\begin{frame}[fragile]{Algebraic data types in general}
\begin{verbatim}
data AlgDataType = Constr1 Type11 Type12
                 | Constr2 Type21
                 | Constr3 Type31 Type32 Type33
                 | Constr4
\end{verbatim}
\end{frame}

{
  \usebackgroundtemplate{
    \parbox[c][\paperheight][c]{\paperwidth}{\centering\includegraphics[width=2in]{images/fingerprint-light.jpg}}
  }

\section{Pattern matching}
}

\begin{frame}[fragile]{Matching algebraic data types}
\begin{verbatim}
data AlgDataType = Constr1 Type11 Type12
                 | Constr2 Type21
                 | Constr3 Type31 Type32 Type33
                 | Constr4
\end{verbatim}

\onslide<2->
\begin{verbatim}
foo (Constr1 a b)   = ...
foo (Constr2 a)     = ...
foo (Constr3 a b c) = ...
foo Constr4         = ...
\end{verbatim}
\end{frame}

\begin{frame}[fragile]{@@-patterns}
\begin{code}
baz :: Person -> String
baz p@(Person n _ _) = 
  "The name field of (" ++ show p ++ ") is " ++ n
\end{code}

\onslide<2->
\begin{verbatim}
*Main> baz brent
"The name field of (Person \"Brent\" 30 SealingWax) 
    is Brent"
\end{verbatim}
\end{frame}

\begin{frame}[fragile]{Nested patterns}
\begin{code}
checkFav :: Person -> String
checkFav (Person n _ SealingWax) 
  = n ++ ", you're my kind of person!"
checkFav (Person n _ _)          
  = n ++ ", your favorite thing is lame."
\end{code}

\onslide<2->
\begin{verbatim}
*Main> checkFav brent
"Brent, you're my kind of person!"
*Main> checkFav stan
"Stan, your favorite thing is lame."
\end{verbatim}
\end{frame}

\begin{frame}[fragile]{Patterns in general}
\begin{verbatim}
pat ::= _
     |  var
     |  var @ ( pat )
     |  ( Constructor pat1 pat2 ... patn )
\end{verbatim}
\end{frame}

\begin{frame}[fragile]{Matching literal values}
  \begin{verbatim}
fizz 2 = ...

buzz 'a' = ...
  \end{verbatim}

\onslide<2->
\begin{verbatim}
data Int  = 0 | 1 | -1 | 2 | -2 | ...
data Char = 'a' | 'b' | 'c' | ...
\end{verbatim}
\end{frame}

\begin{frame}[fragile]{|case| expressions}
\begin{verbatim}
case exp of
  pat1 -> exp1
  pat2 -> exp2
  ...
\end{verbatim}

\onslide<2->
\begin{code}
exCase = case "Hello" of
           []      -> 3
           ('H':s) -> length s
           _       -> 7
\end{code}
\end{frame}

\begin{frame}{Function clauses are really |case|}
\begin{code}
failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d

failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                     Failure -> 0
                     OK d    -> d
\end{code}
\end{frame}

{
  \usebackgroundtemplate{
    \parbox[c][\paperheight][c]{\paperwidth}{\centering\includegraphics[width=2.5in]{images/droste-light.jpg}}
  }

\section{Recursive types}
}

\begin{frame}{Our own lists}
\begin{code}
data IntList = Empty | Cons Int IntList
\end{code}

\onslide<2->
\begin{code}
lst :: IntList
lst = Cons 2 (Cons 3 (Cons 4 Empty))
\end{code}

\onslide<3->
\begin{code}
intListProd :: IntList -> Int
intListProd Empty      = 1
intListProd (Cons x l) = x * intListProd l
\end{code}
\end{frame}

\begin{frame}{Trees}
\begin{code}
data Tree = Leaf Char
          | Node Tree Int Tree
  deriving Show
\end{code}

\begin{overprint}
\onslide<1>
\begin{center}
\includegraphics[width=1.5in]{images/Tree}  
\end{center}

\onslide<2>
\begin{code}
tree :: Tree
tree = Node (Leaf 'x') 
            1 
            (Node (Leaf 'y') 2 (Leaf 'z'))
\end{code}
\end{overprint}
\end{frame}

\section{Fun with recursive types}

\begin{frame}{A funny list}
  \begin{code}
y = 1 : y
  \end{code}
\end{frame}

\begin{frame}{Natural numbers}
  Every natural number is either
  \begin{itemize}
  \item zero, or
  \item the successor of some other natural number.
  \end{itemize}

\onslide<2->
\begin{code}
data Nat = Zero
         | Succ Nat
\end{code}

\onslide<3->
\begin{verbatim}
three :: Nat
three = Succ (Succ (Succ Zero))
\end{verbatim}
\end{frame}

\begin{frame}{Natural numbers}
  \begin{overprint}
    \onslide<1>
    \begin{code}
plus :: Nat -> Nat -> Nat
    \end{code}
    \onslide<2>
    \begin{code}
plus :: Nat -> Nat -> Nat 
plus Zero     n = n 
plus (Succ m) n = Succ (plus m n)
    \end{code}
  \end{overprint}
\end{frame}

\begin{frame}{Trees again}
\begin{code}
data Tree = Leaf Char
          | Node Tree Int Tree
\end{code}

\begin{overprint}
  \onslide<2>
  \begin{code}
fringe :: Tree -> String
  \end{code}

  \begin{center}
  \parbox{2in}{
  \includegraphics[width=1.5in]{images/Tree}
  }
  \parbox{2in}{
    \begin{code}
"xyz"
    \end{code}
  }
  \end{center}

\onslide<3>
\begin{code}
fringe :: Tree -> String
fringe (Leaf c)     = [c]
fringe (Node l _ r) = fringe l ++ fringe r
\end{code}
\end{overprint}
\end{frame}

\begin{frame}{Foo}
  \begin{code}
data Foo = F Char (Int -> Foo)
  \end{code}

  \begin{overprint}
\onslide<2>
\begin{center}
  wtf!?
\end{center}

\onslide<3>
\begin{code}
foo :: Foo
foo = ?
\end{code}

\onslide<4>
\begin{code}
fooPath :: [Int] -> Foo -> Char
\end{code}

\onslide<5>
\begin{code}
fooPath :: [Int] -> Foo -> Char
fooPath []     (F c _) = c
fooPath (i:is) (F _ f) = fooPath is (f i)
\end{code}
  \end{overprint}
\end{frame}

\end{document}