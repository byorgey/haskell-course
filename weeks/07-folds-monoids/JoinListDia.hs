{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import JoinList (JoinList, (+++))
import qualified JoinList as JL

import Diagrams.TwoD.Layout.Tree

sng p x = JL.Single (Product p) x

jl :: JoinList (Product Int) Char
jl = (sng 5 'y' +++ (sng 2 'e' +++ sng 3 'a')) +++ sng 7 'h'

data JLData m a = JLLeaf m a | JLInternal m

jlToBTree :: JoinList m a -> BTree (JLData m a)
jlToBTree JL.Empty = Empty
jlToBTree (JL.Single m a) = leaf (JLLeaf m a)
jlToBTree (JL.Append m l r) = BNode (JLInternal m) (jlToBTree l) (jlToBTree r)


main = do
  let Just t = uniqueXLayout 2 2 (jlToBTree jl)
      t'     = forceLayoutTree
                 defaultForceLayoutTreeOpts { edgeLen = 2 * sqrt 3 }
                 t

  defaultMain . pad 1.1 . lw 0.03 . centerXY $
    renderTree drawNode
               (~~)
               t'

drawNode (JLInternal (Product p)) = drawAnn p
drawNode (JLLeaf (Product p) c)   = drawAnn p === drawChar c

drawAnn p  = text (show p) # fontSize 0.75 <> circle 0.75 # fc white
drawChar c = text (show c) <> square 1.5 # fc white

