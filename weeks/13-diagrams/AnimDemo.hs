{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

sq = fc <$> (blend <$> ui <*> pure blue <*> pure red)
        <*> (pure (square 1))

anim = (rotateBy <$> ui <*> sq)

main = animMain (anim)
