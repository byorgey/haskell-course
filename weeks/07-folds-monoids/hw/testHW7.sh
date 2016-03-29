jl_file=$1

cat > ./JoinList.hs <<EOF

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
module JoinList where
EOF

cat $jl_file | grep -v '^module' >> ./JoinList.hs

scrab_file=$2

cat > ./Scrabble.hs <<EOF

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
module Scrabble where
EOF

cat $scrab_file | grep -v '^module' >> ./Scrabble.hs


runhaskell HW7Test.hs
