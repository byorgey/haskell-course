{-

   Tool for scoring golf HW submissions.

   In preparation:


   1. Create a file containing a list of submission filenames, one per line.

   2. In each of these files, put "!!!!!" to mark the boundary between
      problems.  Make sure to delete any code which should not be graded
      (but the tool will deal with getting rid of module declarations,
      imports, comments, and type signatures.)


   3. Run the tool.  Note there are some assumptions below about file
      structure and so on, these may have to be tweaked.
-}

import Control.Monad
import Data.Functor
import Control.Arrow
import Data.List.Split
import Data.List
import Data.Data
import Data.Char

import Language.Haskell.Exts.Annotated

countGolf :: String -> [Int]
countGolf
  =   lines
  >>> splitWhen ("!!!!!" `isInfixOf`)
  >>> map (unlines >>> count)

count :: String -> Int
count code =
  case parseFileContents code of
    f@(ParseFailed _ _) -> error $ show f
    ParseOk m           ->
      countModuleLength m

countModuleLength :: Module SrcSpanInfo -> Int
countModuleLength (Module _ _ _ _ decls)
  = length
  . filter (not . isSpace)
  . concat
  . map (\d -> exactPrint d [])
  . filter ((/= "TypeSig") . show . toConstr)
  $ decls

main :: IO ()
main = do
  files <- lines <$> readFile "GolfList.txt"
  forM_ files $ \f -> do
    let user = takeWhile (/= '/') . drop 3 $ f

    txt <- readFile f
    let counts = countGolf txt
    putStrLn (intercalate " " (user : map show counts))
