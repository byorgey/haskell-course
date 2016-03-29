files=$1

cat > ./Calc.hs <<EOF

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Calc where
EOF

cat "$files" | grep -v '^module' >> ./Calc.hs
runhaskell HW5Test.hs
runhaskell Part5Test.hs
runhaskell Part6Test.hs
