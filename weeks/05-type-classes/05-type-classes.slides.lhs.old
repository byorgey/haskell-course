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
\date{Week 5: February 9, 2012}

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
    \parbox[c][\paperheight][c]{\paperwidth}{\centering\includegraphics[width=3in]{../images/haskell-logo-light.png}}
  }

\begin{frame}
  \titlepage
\end{frame}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{frame}{Code golf results!}
  \begin{center}
    \includegraphics[width=3in]{images/golfball.jpg}
  \end{center}
\end{frame}

\begin{frame}{Task 1: Hopscotch}
  \begin{center}
    \begin{overprint}
      \onslide<2>
      \includegraphics[width=4.5in]{images/1wo.pdf}

      \onslide<3>
      \includegraphics[width=4.5in]{images/1w.pdf}
    \end{overprint}
  \end{center}
\end{frame}

\begin{frame}{Task 1: Hopscotch}
  \begin{code}
skips :: [a] -> [[a]]
skips l = [[n | (i, n) <- zip [1..] l, i `mod` m == 0] 
           | (m, _) <- zip [1..] l]
  \end{code}
\end{frame}

\begin{frame}{Task 2: Local maxima}
  \begin{center}
    \begin{overprint}
      \onslide<2>
      \includegraphics[width=4.5in]{images/2wo.pdf}

      \onslide<3>
      \includegraphics[width=4.5in]{images/2w.pdf}      
    \end{overprint}
  \end{center}
\end{frame}

\begin{frame}{Task 2: Local maxima}
  \begin{code}
localMaxima :: [Integer] -> [Integer]
localMaxima l = 
  [b | (a,b,c) <- drop 1 (zip3 (0:l) l (drop 1 l))
     , a < b && b > c]
  \end{code}
\end{frame}

\begin{frame}{Task 3: Histogram}
  \begin{center}
    \begin{overprint}
      \onslide<2>
      \includegraphics[width=4.5in]{images/3wo.pdf}

      \onslide<3>
      \includegraphics[width=4.5in]{images/3w.pdf}      
    \end{overprint}
  \end{center}
\end{frame}

\begin{frame}{Task 3: Histogram}
  \begin{code}
b :: [Integer] -> String
b [] = ""
b l = b (l \\ [0..9]) ++ [c i l | i<-[0..9]] ++ "\n"

c :: Integer -> [Integer] -> Char
c n l
    | elem n l       = '*'
    | True           = ' '

histogram :: [Integer] -> String
histogram l = b l ++ "==========\n0123456789\n" 
  \end{code}
\end{frame}

\end{document}