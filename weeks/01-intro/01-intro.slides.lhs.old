% -*- LaTeX -*-
\documentclass{beamer}

%include polycode.fmt

\mode<presentation>
{ 
 \usetheme{default}
}

\usepackage{graphicx}

% \setbeameroption{show notes}
\usenavigationsymbolstemplate{}

\title{CIS 194: Haskell}
\author{Brent Yorgey}
\date{January 12, 2012}

\AtBeginSection[]
{
  \begin{frame}<beamer>{Outline}
    \tableofcontents[currentsection,currentsubsection]
  \end{frame}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{document}

% \lstset{language=Haskell } %% , basicstyle=\ttfamily, keywordstyle=\bfseries}

{
  \usebackgroundtemplate{
    \parbox[c][\paperheight][c]{\paperwidth}{\centering\includegraphics[width=3in]{images/haskell-logo-light.png}}
  }

\begin{frame}
  \titlepage
\end{frame}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%  Intro  %%%%%%%%%%%%%%%

\section{Haskell is\dots}

\subsection{Functional}

{
  \usebackgroundtemplate{
    \parbox[c][\paperheight][c]{\paperwidth}{\centering\includegraphics[width=2.5in]{images/function-machine-light.png}
    }
  }

\begin{frame}{Haskell is\dots}

  {\Large Functional} \bigskip

  \begin{itemize}
  \item Functions are \emph{first-class}

    \begin{code}
f g = g 3 + g 5

f (2*) = (2*3) + (2*5) = 6 + 10 = 16
    \end{code}
  \item Focus is on \emph{evaluation} rather than \emph{execution}
  \end{itemize}
\end{frame}

}

\subsection{Pure}

{
  \usebackgroundtemplate{
    \parbox[c][\paperheight][c]{\paperwidth}{\centering\includegraphics[height=\paperheight]{images/pure-light.jpg}
    }
  }

\begin{frame}{Haskell is\dots}

  {\Large Pure} \bigskip

\begin{overprint}
\onslide<1>
  \begin{itemize}
  \item No ``side effects''
  \item No mutation
  \item No ``spooky action-at-a-distance''
  \item Functions always give the same output for the same inputs
  \end{itemize}

\onslide<2>
  So what?

  \begin{itemize}
  \item Reasoning and refactoring
  \item Parallelism and concurrency
  \item Fewer bugs and headaches
  \end{itemize}

\end{overprint}

\end{frame}
}

\subsection{Lazy}

{
  \usebackgroundtemplate{
    \parbox[c][\paperheight][c]{\paperwidth}{\centering\includegraphics[width=\paperwidth]{images/relax-light.jpg}
    }
  }

\begin{frame}{Haskell is\dots}

  {\Large Lazy} \bigskip

  \begin{overprint}

    \onslide<1>
    Computation happens only when needed!

    \begin{code}
f :: Int -> Int -> Int
f x y = y + 3

(f (3^(3^(3^3)))  2) == 5

(f (error "Urgh") 3) == 6
    \end{code}
    
    \onslide<2>
    So what?

    \begin{itemize}
    \item More \emph{compositional}
    \item We can define our own control structures
    \item We can work with infinite data structures
    \item (But harder to reason about space usage!)
    \end{itemize}    
  \end{overprint}

  \note{
    Note 3^(3^(3^3)) has about 3.6 trillion (decimal) digits; would
    need about 1.4 TB just to store it.
  }

\end{frame}

}

\subsection{Statically typed}

{
  \usebackgroundtemplate{
    \parbox[c][\paperheight][c]{\paperwidth}{\centering\includegraphics[width=\paperwidth]{images/static-light.jpg}
    }
  }

\begin{frame}{Haskell is\dots}
  
  {\Large Statically typed} \bigskip

  \begin{itemize}
  \item Everything has a type
  \item All types checked at compile time
  \item Trust me, this is awesome
  \end{itemize}

\end{frame}

}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Themes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Themes for the semester}

\subsection{Theme I: Types}

\begin{frame}{Theme I: Types}

  \begin{center}
    \includegraphics[width=2in]{images/suit.jpg}

    Type systems are like suits\dots
  \end{center}

  
\end{frame}

\begin{frame}{Theme I: Types}

  \begin{center}
    \only<1>{
      \includegraphics[width=2in]{images/straightjacket.jpg} \bigskip

      {\Large Static types?}
    }
    \only<2>{
      \includegraphics[width=2in]{images/icarus.jpg} \bigskip

      {\Large Dynamic types?}
    }
    \only<3>{
      \includegraphics[width=1.5in]{images/icarus2.jpg} \bigskip

      {\Large Dynamic types.}
    }
    \only<4>{
      \includegraphics[width=2in]{images/exoskeleton.jpg} \bigskip

      {\Large Static types.}
    }
  \end{center}
\end{frame}

{
  \usebackgroundtemplate{
    \parbox[c][\paperheight][c]{\paperwidth}{\centering\includegraphics[width=2in]{images/exoskeleton-light.jpg}
    }
  }

\begin{frame}{Theme I: Types}

A good static type system:

\begin{itemize}
\item Aids in planning and reasoning about code
\item Enables the compiler to catch many common errors
\item Makes refactoring easier
\item Serves as a form of documentation
\end{itemize}

% XXX any other benefits to list here?

\end{frame}

}

\subsection{Theme II: Abstraction}

\begin{frame}{Theme II: Abstraction}

  \begin{center}
    \includegraphics[height=2in]{images/widgets.pdf}
  \end{center}

\end{frame}

\begin{frame}{Theme II: Abstraction}
  
  \begin{center}
    \includegraphics[height=2in]{images/abstraction.pdf}
  \end{center}

\end{frame}

\begin{frame}{Theme II: Abstraction}

  Haskell has many features enabling sophisticated abstraction:

  \begin{itemize}
  \item Algebraic data types
  \item Higher-order functions
  \item Polymorphism
  \item Type classes
  \item \dots
  \end{itemize}
  
\end{frame}

\subsection{Theme III: Wholemeal Programming}
\label{sec:wholemeal}

\begin{frame}{Theme III: Wholemeal programming}
  \begin{center}
    \includegraphics[width=3in]{images/bread.jpg}
  \end{center}
\end{frame}

{
  \usebackgroundtemplate{
    \parbox[c][\paperheight][c]{\paperwidth}{\centering\includegraphics[width=3in]{images/bread-light.jpg}
    }
  }

\begin{frame}{Theme III: Wholemeal programming}
  Work with \emph{whole data structures}, not individual
  elements. \bigskip

\hrule

\begin{verbatim}
int acc = 0;
for (int i = 0; i < lst.length; i++) {
  acc = acc + 3*lst[i];
}
\end{verbatim}

\hrule

  \begin{code}
acc = sum (map (3*) lst)    
  \end{code}
 
\end{frame}

}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Administrivia}

\begin{frame}{Course website}
  \begin{center}
    \includegraphics[width=0.8 \textwidth]{images/website.jpg}
  \end{center}
\end{frame}

\begin{frame}{Piazza}
  \begin{center}
    \includegraphics[width=0.8 \textwidth]{images/piazza.jpg}
  \end{center}
\end{frame}

\begin{frame}{Homework}

  \begin{itemize}
  \item Weekly, due Thursday before class
  \item 75\% correctness, 25\% style
  \item Comments back by Monday
  \item Revised version optionally due the following Thursday (along
    with the next assignment)
  \item Final HW grade = average of initial and revised
  \end{itemize}

\end{frame}

% Syllabus.  homework, grading.

% Installing GHC + Haskell Platform.

% Editors/IDEs.


\end{document}


