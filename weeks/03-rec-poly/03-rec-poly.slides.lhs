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
\date{Week 3: January 28, 2013}

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

\section{Recursion patterns}

\begin{frame}{Big idea}
  \textit{First-class functions} let us abstract common
    recursion patterns.
\end{frame}

\begin{frame}{Lists, again}
Recall:

\begin{code}
data IntList = Empty | Cons Int IntList
  deriving Show
\end{code}

What sorts of things might we commonly do with an |IntList|?
\end{frame}

\begin{frame}{Mapping}
  \begin{code}
addOneToAll :: IntList -> IntList
  \end{code}
\onslide<2->
\begin{code}
addOneToAll Empty       = Empty
addOneToAll (Cons x xs) = Cons (x+1) (addOneToAll xs)
\end{code}
\end{frame}

\begin{frame}{More mapping}
\begin{code}
addOneToAll :: IntList -> IntList
addOneToAll Empty       = Empty
addOneToAll (Cons x xs) = Cons (x+1) (addOneToAll xs)

absAll :: IntList -> IntList
absAll Empty       = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

squareAll :: IntList -> IntList
squareAll Empty       = Empty
squareAll (Cons x xs) = Cons (x*x) (squareAll xs)
\end{code}
\end{frame}

\begin{frame}{|mapIntList|}
  \begin{overprint}
    \onslide<1>
  \begin{code}
mapIntList :: 
  \end{code}

  \onslide<2>
  \begin{code}
mapIntList :: (Int -> Int) -> IntList -> IntList
  \end{code}
    
  \onslide<3>
  \begin{code}
mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty       = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)
  \end{code}

  \end{overprint}
\end{frame}

\begin{frame}[fragile]{Using |mapIntList|}
\begin{code}
exampleList = Cons (-1) (Cons 2 (Cons (-6) Empty))

addOne x = x + 1
square x = x * x
\end{code}

\begin{verbatim}
mapIntList addOne exampleList
mapIntList abs    exampleList
mapIntList square exampleList
\end{verbatim}
\end{frame}

\begin{frame}{Filtering}
  \begin{overprint}
    \onslide<1>
    \begin{code}
keepOnlyPositive :: IntList -> IntList
    \end{code}  

    \onslide<2>
    \begin{code}
keepOnlyPositive :: IntList -> IntList
keepOnlyPositive Empty = Empty
keepOnlyPositive (Cons x xs) 
  | x > 0     = Cons x (keepOnlyPositive xs)
  | otherwise = keepOnlyPositive xs
    \end{code}
  \end{overprint}
\end{frame}

\begin{frame}{More filtering}
  \begin{code}
keepOnlyPositive :: IntList -> IntList
keepOnlyPositive Empty = Empty
keepOnlyPositive (Cons x xs) 
  | x > 0     = Cons x (keepOnlyPositive xs)
  | otherwise = keepOnlyPositive xs

keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs) 
  | even x    = Cons x (keepOnlyEven xs) 
  | otherwise = keepOnlyEven xs
  \end{code}
\end{frame}

\begin{frame}{|filterIntList|}
  \begin{overprint}
    \onslide<1>
    \begin{code}
filterIntList ::      
    \end{code}

    \onslide<2>
    \begin{code}
filterIntList :: (Int -> Bool) -> IntList -> IntList      
    \end{code}

    \onslide<3>
    \begin{code}
filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ Empty = Empty
filterIntList p (Cons x xs)
  | p x       = Cons x (filterIntList p xs)
  | otherwise = filterIntList p xs
    \end{code}
  \end{overprint}
\end{frame}

\begin{frame}[fragile]{Using |filterIntList|}
\begin{code}
exampleList = Cons (-1) (Cons 2 (Cons (-6) Empty))

positive x = x > 0
\end{code}

\begin{verbatim}
filterIntList positive exampleList
filterIntList even     exampleList
\end{verbatim}
\end{frame}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Polymorphism}

\begin{frame}{Polymorphism}
  \begin{center}
  ``Polymorphic'' = ``having many forms'' \bigskip

  \onslide<2->
  Polymorphic things have not just one type, but many. \\
  ML, Haskell $\rightarrow$ Java generics.
  \end{center}
\end{frame}

\begin{frame}{Polymorphic data types}
\begin{code}
data List t = E | C t (List t)
\end{code}

\onslide<2->
\begin{code}
lst1 :: List Int
lst1 = C 3 (C 5 (C 2 E))

lst2 :: List Char
lst2 = C 'x' (C 'y' (C 'z' E))

lst3 :: List Bool
lst3 = C True (C False E)
\end{code}
\end{frame}

\begin{frame}{Polymorphic filter}
\begin{overprint}
\onslide<1>
\begin{code}
filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ Empty = Empty
filterIntList p (Cons x xs)
  | p x       = Cons x (filterIntList p xs)
  | otherwise = filterIntList p xs
\end{code}

\onslide<2>
\begin{code}
filterList    :: ?
filterList    _ E     = E
filterList    p (C    x xs)
  | p x       = C    x (filterList    p xs)
  | otherwise = filterList    p xs
\end{code}

\onslide<3>
\begin{code}
filterList :: (a -> Bool) -> List a -> List a
filterList _ E = E
filterList p (C x xs)
  | p x       = C x (filterList p xs)
  | otherwise = filterList p xs
\end{code}
\end{overprint}
\end{frame}

\begin{frame}{Polymorphic map}
\begin{overprint}
\onslide<1>
\begin{code}
mapList :: ?
mapList _ E        = E
mapList f (C x xs) = C (f x) (mapList f xs)
\end{code}

\onslide<2>
\begin{code}
mapList :: (t -> t) -> List t -> List t
mapList _ E        = E
mapList f (C x xs) = C (f x) (mapList f xs)
\end{code}

\onslide<3>
\begin{code}
mapList :: (a -> b) -> List a -> List b
mapList _ E        = E
mapList f (C x xs) = C (f x) (mapList f xs)
\end{code}
\end{overprint}
\end{frame}

\section{The Prelude}

\section{Rant: total and partial functions}

\begin{frame}{A type puzzle}
\begin{center}
|? :: [a] -> a|
\end{center}
\end{frame}

\begin{frame}{Functions you should (almost) never use (short list)}
  \begin{code}
head     :: [a] -> a
last     :: [a] -> a
tail     :: [a] -> [a]
init     :: [a] -> [a]
(!!)     :: [a] -> Int -> a
fromJust :: Maybe a -> a
  \end{code}
\end{frame}

\begin{frame}{Don't be partial, pattern-match!}
\begin{code}
doStuff1 :: [Int] -> Int
doStuff1 []  = 0
doStuff1 [_] = 0
doStuff1 xs  = head xs + (head (tail xs)) 
\end{code}

\onslide<2->
\begin{code}
doStuff2 :: [Int] -> Int
doStuff2 []        = 0
doStuff2 [_]       = 0
doStuff2 (x1:x2:_) = x1 + x2
\end{code}
\end{frame}

\begin{frame}{Encoding partiality in the type system}
\begin{code}
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
\end{code}
\end{frame}

% \begin{frame}
% \begin{code}
% data NonEmptyList a = NEL a [a]

% nelToList :: NonEmptyList a -> [a]
% nelToList (NEL x xs) = x:xs

% listToNel :: [a] -> Maybe (NonEmptyList a)
% listToNel []     = Nothing
% listToNel (x:xs) = Just $ NEL x xs

% headNEL :: NonEmptyList a -> a
% headNEL (NEL a _) = a

% tailNEL :: NonEmptyList a -> [a]
% tailNEL (NEL _ as) = as
% \end{code}
% \end{frame}



\end{document}