{-# LANGUAGE DeriveDataTypeable #-}

import           Control.Monad              (forM_, replicateM_, when)
import           Data.Functor               ((<$>))
import           Data.List                  (find, isPrefixOf)
import           Data.Monoid                ((<>))

import           Development.Shake
import           Development.Shake.FilePath

import           System.Cmd                 (system)
import           System.Console.CmdArgs
import           System.Directory           (doesDirectoryExist)
import           System.FilePath            (takeFileName)

data CIS194Mode =
    Build
  | Preview
  | Clean
  | Deploy
  deriving (Show, Data, Typeable)

cis194Modes = modes
  [ Build   &= auto
  , Preview
  , Clean
  , Deploy
  ]

main :: IO ()
main = (shake shakeOptions . chooseMode) =<< cmdArgs cis194Modes

chooseMode :: CIS194Mode -> Rules ()
chooseMode mode = cis194Rules <> choose mode
  where
    choose (Build {})   = doBuild
    choose (Preview {}) = doPreview
    choose (Clean {})   = doClean
    choose (Deploy {})  = doDeploy

cis194Rules :: Rules ()
cis194Rules = genericRules <> webRules <> weekRules

doBuild = action $ requireBuild

doClean = action $ do
  alwaysRerun
  system' "rm" ["-rf", "web/_site", "web/_cache"]

doPreview = action $ do
  alwaysRerun
  requireBuild
  need ["web/hakyll.hs.exe"]
  systemCwd "web" ("./hakyll.hs.exe") ["preview", "8000"]

doDeploy = action $ do
  alwaysRerun
  liftIO $ system "rsync -tvr web/_site/* cis194@minus.seas.upenn.edu:html/ && ssh cis194@minus.seas.upenn.edu chmod -R o+rX html/"

--------------------------------------------------

webRules :: Rules ()
webRules = do
  "web/lectures/*.markdown" *> \out -> do
    let base = takeBaseName out
        f    = weekFile base "" "markdown"
    copyFile' f out

  "web/lectures/*.lhs" *> \out -> do
    let base = takeBaseName out
        f    = weekFile base "lec" "lhs"
    copyFile' f out

  "web/lectures/*.html" *> \out -> do
    let base = takeBaseName out
        f    = weekFile base "lec" "lhs"
    need [f, "tools/processLec.hs.exe"]
    system' "tools/processLec.hs.exe"
      [ "html",  f, "-o", out ]

  "web/hw/*.pdf" *> \out -> do
    let base = takeBaseName out
        f    = weekFile base "hw" "pdf"
    copyFile' f out

  "web/docs/*" *> \out -> do
    copyFile' (dropDirectory1 out) out

  "web/extras//*" *> \out -> do
    let (week,f) = splitFileName . dropDirectory1 . dropDirectory1 $ out
        loc = "weeks" </> week </> "hw" </> "skel" </> f
    e <- doesFileExist loc
    let loc' = case e of
                 True -> loc
                 False -> "weeks" </> week </> "hw" </> f
    copyFile' loc' out

weekRules :: Rules ()
weekRules = do
  "weeks//*.inclass.lhs" *> \out -> do
    let base = takeBaseName . takeBaseName $ out
        f    = weekFile base "lec" "lhs"
    need [f, "tools/processLec.hs.exe"]
    system' "tools/processLec.hs.exe"
      [ "class",  f, "-o", out ]

requireBuild :: Action ()
requireBuild = do
  weekDirs <- getDirectoryDirs "weeks"
  mapM_ copyImages weekDirs
  need =<< (concat <$> mapM mkWeek weekDirs)
  need ["web/docs/inthelarge.pdf", "web/docs/style.pdf"]
 where
  copyImages week = do
    let imgDir = "weeks" </> week </> "images"
    exists <- doesFileExist imgDir
    when exists $ do
      imgFiles <- getDirectoryFiles imgDir ["*"]
      mapM_ (\f -> copyFile' (imgDir </> f) ("web/images" </> f)) imgFiles
  mkWeek week = do
    extras      <- getExtras week
    solsExist   <- doesFileExist (weekFile week "sols" "lhs")
    slidesExist <- doesFileExist (weekFile week "slides" "lhs")
    return $
      [ weekFile week "hw" "pdf"
      , weekFile week "inclass" "lhs"]
      ++
      [ weekFile week "sols" "pdf" | solsExist ]
      ++
      [ weekFile week "slides" "pdf" | slidesExist ]
      ++
      [ "web/lectures" </> week <.> "markdown"
      , "web/lectures" </> week <.> "lhs"
      , "web/lectures" </> week <.> "html"
      , "web/hw" </> week <.> "pdf"
      ]
      ++
      map (("web/extras" </> week </>) . takeFileName) extras

weekFile :: FilePath -> String -> String -> FilePath
weekFile week tag ext = "weeks" </> week </> (week <.> tag) <.> ext

getExtras :: FilePath -> Action [FilePath]
getExtras week = do
  wws <- map words <$> readFileLines (weekFile week "" "markdown")
  case find (\ws -> (not . null $ ws) && (head ws == "extras:")) wws of
    Nothing -> return []
    Just fs -> return (tail fs)

--------------------------------------------------

genericRules :: Rules ()
genericRules = do

  "//*.hs.exe" *> \out -> do
    let hs = dropExtension out
    need [hs]
    system' "ghc" ["--make", "-o", out, hs]

  ["docs//*.pdf", "weeks//*.pdf"] **> \out -> do
    let tex = replaceExtension out "tex"
        dir = takeDirectory out
    need [tex]
    pkgs <- ( map (takeWhile (/= '}') . drop 1 . dropWhile (/= '{'))
            . filter ("\\usepackage" `isPrefixOf`)
            )
            <$> readFileLines tex
    forM_ pkgs $ \pkg -> do
      let sty = dir </> pkg <.> "sty"
      e <- doesFileExist sty
      when e $ need [sty]
    hs <- getDirectoryFiles dir ["*.hs"]
    need (map (dir </>) hs)

    let tex' = takeFileName tex
    replicateM_ 2 $
      systemCwd dir "pdflatex"
        ["--enable-write18", tex']

  "//*.tex" *> \out -> do
    let lhs = replaceExtension out "lhs"
        dir = takeDirectory out
    useLhs <- doesFileExist lhs
    when useLhs $ do
      need [lhs]
      system' "lhs2TeX" ["--verb", lhs, "-o", out]


--------------------------------------------------
