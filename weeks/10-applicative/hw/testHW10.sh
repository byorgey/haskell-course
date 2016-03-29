aparser_file=$1

cat > ./AParser.hs <<EOF

{-# OPTIONS_GHC -Wall #-}
EOF

cat $aparser_file >> ./AParser.hs

runhaskell HW10Test.hs
