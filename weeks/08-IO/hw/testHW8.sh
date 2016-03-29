party_file=$1

cat > ./Party.hs <<EOF

{-# OPTIONS_GHC -Wall #-}
EOF

cat $party_file > ./Party.hs

runhaskell HW8Test.hs


if runhaskell Party.hs | diff correct_gl.txt -
then
    echo "Consistent Guest List"
else
    echo "Bad Guest List!!!"
fi
