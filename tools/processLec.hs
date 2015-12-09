{-# LANGUAGE DeriveDataTypeable #-}

-- CIS 194, F'10, S'12, S'13.
-- Utility to prepare in-class versions and HTML versions of .lhs files:
--   + strip HTML comments
--   + consecutively number examples
--   + add compile date+time
--
-- See http://johnmacfarlane.net/pandoc/scripting.html

import           System.Console.CmdArgs hiding (def)

import           Data.Default
import           Data.List              (isPrefixOf)
import qualified Data.Set               as S
import           Data.Time
import           Text.Pandoc            hiding (Target, def)
import           Text.Printf

import           Control.Applicative    ((<$>))
import           Control.Monad.Supply
import           Data.Maybe

import           Data.Char

------------------------------------------------------------
-- Command-line arguments.
------------------------------------------------------------

data Target = Class { inputFile :: FilePath, outputFile :: Maybe FilePath }
            | Html  { inputFile :: FilePath, outputFile :: Maybe FilePath }
            | Slides
  deriving (Show, Data, Typeable, Eq)

------------------------------------------------------------
-- Process lecture notes
------------------------------------------------------------

-- Strip comments not intended for the current target.
stripComments :: Target -> [Block] -> [Block]
stripComments targ = concatMap stripComments'
  where
    stripComments' :: Block -> [Block]
    stripComments' (Plain [RawInline "html" s]) = doStrip s
    stripComments' (RawBlock "html" s)          = doStrip s
    stripComments' b = [b]

    doStrip s
      | "<!--" `isPrefixOf` s && forTarget = inc
      | otherwise = []
      where (Pandoc _ inc) = readLHS . unlines . init . tail . lines $ s
            forTarget      = upper (head . words . show $ targ) `elem`
                             (map upper . words . head . lines . drop 4 $ s)
    upper = map toUpper

-- Number examples consecutively, using the Supply monad to provide
-- consecutive numbers as needed.
numberExamples :: Pandoc -> Pandoc
numberExamples = flip evalSupply [1..] . bottomUpM numberBlock
  where numberBlock :: Block -> Supply Int Block
        numberBlock (CodeBlock attr str)
          = CodeBlock attr . unlines <$> mapM numberLine (lines str)
        numberBlock b = return b

        numberLine :: String -> Supply Int String
        numberLine s
          | "ex" `isPrefixOf` s && (s !! 2 `notElem` ['a'..'z']) = do
              n <- supply
              return $ "ex" ++ printf "%02d" n ++ " " ++ dropWhile (/= '=') s
          | otherwise = return s

addTimestamp :: String -> Pandoc -> Pandoc
addTimestamp time (Pandoc meta blocks) = Pandoc meta (blocks ++ [HorizontalRule, genMsg])
  where genMsg = Para [Code nullAttr $ "Generated " ++ time]

transformDoc :: Target -> String -> Pandoc -> Pandoc
transformDoc targ time = bottomUp (stripComments targ)
                       . numberExamples
                       . addTimestamp time

------------------------------------------------------------
-- Slide generation
------------------------------------------------------------

{-
data SlideItem = Frame { frameTitle   :: String
                       , frameContent :: Pandoc
                       }
               | Section String
               | Subsection String

type Deck = [SlideItem]
-}

-- Extract all the code blocks
data Code = Hask String
          | Verb String

genSlides :: Pandoc -> [Code]
genSlides = queryWith getCode
  where getCode (CodeBlock (_,ts,_) c)
          | "haskell" `elem` ts = [Hask c]
          | otherwise           = [Verb c]
        getCode _               = []

writeSlides :: [Code] -> String
writeSlides = unlines . map mkFrame
  where mkFrame (Hask c) = env "frame" . env "code" $ (c ++ "\n")
        mkFrame (Verb c) = env' "frame" (Just "fragile") . env "verbatim" $ (c ++ "\n")

env :: String -> String -> String
env s = env' s Nothing

env' :: String -> Maybe String -> String -> String
env' e optArg body = "\\begin{" ++ e ++ "}" ++ arg ++ "\n" ++ body ++ "\\end{" ++ e ++ "}\n"
  where arg = case optArg of { Nothing -> "" ; Just o -> "[" ++ o ++ "]" }

------------------------------------------------------------
-- General format reading/writing
------------------------------------------------------------

readLHS :: String -> Pandoc
readLHS = readMarkdown
            def { readerExtensions
                    = Ext_literate_haskell `S.insert` readerExtensions def
                , readerSmart      = True
                }

writeLHS :: Pandoc -> String
writeLHS = writeMarkdown def

writeHTML :: Pandoc -> String
writeHTML = writeHtmlString def

------------------------------------------------------------
-- Main
------------------------------------------------------------

main :: IO ()
main = do
  opts    <- cmdArgs (modes [ Class
                              { inputFile = def
                                         &= argPos 0
                                         &= typFile
                              , outputFile = def
                                         &= typFile
                                         &= help "Output file"
                              }
                              &= help "Output in-class .lhs"
                            , Html
                              { inputFile = def
                                         &= argPos 0
                                         &= typFile
                              , outputFile = def
                                         &= typFile
                                         &= help "Output file"
                              }
                              &= help "Output .html"
                            ]
                            &= summary "Process CIS 194 lecture notes"
                            &= program "processLec"
                            )
  utcTime <- getCurrentTime
  tz      <- getCurrentTimeZone
  let time = show $ utcToLocalTime tz utcTime
  f <- readFile (inputFile opts)
  let output = chooseTransform time opts f
  case outputFile opts of
    Nothing -> putStr output
    Just o  -> writeFile o output

chooseTransform time t@(Class{})  = writeLHS  . transformDoc t time . readLHS
chooseTransform time t@(Html{})   = writeHTML . transformDoc t time . readLHS
-- chooseTransform _    Slides = writeSlides . genSlides . readLHS
