{-# LANGUAGE GeneralizedNewtypeDeriving
           , ScopedTypeVariables
   #-}
module Editor where

import System.IO

import Buffer

import Control.Exception
import Control.Monad.State

import Control.Applicative
import Control.Arrow       (first, second)

import Data.Char
import Data.List

-- Editor commands

data Command = View
             | Edit
             | Load String
             | Line Int
             | Next
             | Prev
             | Quit
             | Help
             | Noop
  deriving (Eq, Show, Read)

commands :: [String]
commands = map show [View, Edit, Next, Prev, Quit]

-- Editor monad

newtype Editor b a = Editor (StateT (b,Int) IO a)
  deriving (Functor, Monad, MonadIO, MonadState (b,Int))

runEditor :: Buffer b => Editor b a -> b -> IO a
runEditor (Editor e) b = evalStateT e (b,0)

getCurLine :: Editor b Int
getCurLine = gets snd

setCurLine :: Int -> Editor b ()
setCurLine = modify . second . const

onBuffer :: (b -> a) -> Editor b a
onBuffer f = gets (f . fst)

getBuffer :: Editor b b
getBuffer = onBuffer id

modBuffer :: (b -> b) -> Editor b ()
modBuffer = modify . first

io :: MonadIO m => IO a -> m a
io = liftIO

-- Utility functions

readMay :: Read a => String -> Maybe a
readMay s = case reads s of
              [(r,_)] -> Just r
              _       -> Nothing

-- Main editor loop

editor :: Buffer b => Editor b ()
editor = io (hSetBuffering stdout NoBuffering) >> loop
    where loop = do prompt
                    cmd <- getCommand
                    when (cmd /= Quit) (doCommand cmd >> loop)

prompt :: Buffer b => Editor b ()
prompt = do
  s <- onBuffer value
  io $ putStr (show s ++ "> ")

getCommand :: Editor b Command
getCommand = io $ readCom <$> getLine
  where
    readCom ""        = Noop
    readCom inp@(c:cs) | isDigit c = maybe Noop Line (readMay inp)
                       | toUpper c == 'L' = Load (unwords $ words cs)
                       | c == '?' = Help
                       | otherwise = maybe Noop read $
                                       find ((== toUpper c) . head) commands

doCommand :: Buffer b => Command -> Editor b ()
doCommand View = do
  cur  <- getCurLine
  let ls = [(cur - 2) .. (cur + 2)]
  ss <- mapM (\l -> onBuffer $ line l) ls
  zipWithM_ (showL cur) ls ss
 where
  showL _ _ Nothing  = return ()
  showL l n (Just s) = io $ putStrLn (m ++ show n ++ ": " ++ s)
    where m | n == l    = "*"
            | otherwise = " "

doCommand Edit = do
  l <- getCurLine
  io $ putStr $ "Replace line " ++ show l ++ ": "
  new <- io getLine
  modBuffer $ replaceLine l new

doCommand (Load filename) = do
  mstr <- io $ handle (\(_ :: IOException) -> 
                         putStrLn "File not found." >> return Nothing
                      ) $ do
                 h <- openFile filename ReadMode
                 hSetEncoding h utf8
                 Just <$> hGetContents h
  maybe (return ()) (modBuffer . const . fromString) mstr

doCommand (Line n) = modCurLine (const n) >> doCommand View

doCommand Next = modCurLine (+1) >> doCommand View
doCommand Prev = modCurLine (subtract 1) >> doCommand View

doCommand Quit = return ()  -- do nothing, main loop notices this and quits

doCommand Help = io . putStr . unlines $
  [ "v --- view the current location in the document"
  , "n --- move to the next line"
  , "p --- move to the previous line"
  , "l --- load a file into the editor"
  , "e --- edit the current line"
  , "q --- quit"
  , "? --- show this list of commands"
  ]

doCommand Noop = return ()

inBuffer :: Buffer b => Int -> Editor b Bool
inBuffer n = do
  nl <- onBuffer numLines
  return (n >= 0 && n < nl)

modCurLine :: Buffer b => (Int -> Int) -> Editor b ()
modCurLine f = do
  l  <- getCurLine
  nl <- onBuffer numLines
  setCurLine . max 0 . min (nl - 1) $ f l
