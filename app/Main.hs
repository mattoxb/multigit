{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Main where

import           Turtle
import qualified Turtle as Turtle
import           Options.Applicative
import qualified Options.Applicative as O
import qualified Control.Foldl as Fold
import           Control.Concurrent
import           Control.Monad

data Opts = Check [Text]
          | Distribute Text Text Text [Text]
          | Add Text [Text]
          | Commit Text [Text]
          | Clone [Text]
          | Pullpush [Text]
          | Push [Text]
          | Pull [Text]
          | Rm Text [Text]
  deriving (Eq,Show)

-- Threadpool stuff that will be spit into different module.

startThread job masterMVar = do
    myMVar <- newEmptyMVar
    forkIO (waitForWork myMVar)
  where waitForWork myMVar = do
        -- tell master we want work
           putMVar masterMVar myMVar
           n <- takeMVar myMVar
           case n of
             Nothing -> return ()
             Just s -> do _ <- job s
                          waitForWork myMVar

threadPool job argList = do
    masterMVar <- newEmptyMVar
    stopMVar <- newEmptyMVar
    -- start 10 worker threads
    mapM_ (\n -> startThread job masterMVar) [1..10]
    forkIO (work stopMVar masterMVar 10 argList)
    takeMVar stopMVar
    return ()
  where work stopMVar _ 0 [] = putMVar stopMVar ()
        work stopMVar masterMVar n [] = do
          -- wait for worker
          wmv <- takeMVar masterMVar
          -- tell it to die
          putMVar wmv Nothing
          work stopMVar masterMVar (n-1) []
        work stopMVar masterMVar n (x:xs) = do
          -- wait for worker
          wmv <- takeMVar masterMVar
          -- Give it some work.
          putMVar wmv (Just x)
          work stopMVar masterMVar n xs

-- Parser

check :: Parser Opts
check = Check <$> many (argument str (metavar "DIR..."))

add :: Parser Opts
add = Add <$> argument str (metavar "SOURCE")
          <*> many (argument str (metavar "DIR..."))

distribute :: Parser Opts
distribute = Distribute <$> argument str (metavar "SOURCE")
                         <*> argument str (metavar "DEST")
                         <*> argument str (metavar "MESSAGE")
                         <*> many (argument str (metavar "DIR..."))

commit :: Parser Opts
commit = Commit <$> argument str (metavar "MESSAGE")
                 <*> many (argument str (metavar "DIR..."))

push :: Parser Opts
push = Push <$> many (argument str (metavar "DIR..."))

pull :: Parser Opts
pull = Pull <$> many (argument str (metavar "DIR..."))

pullpush :: Parser Opts
pullpush = Pullpush <$> many (argument str (metavar "DIR..."))

prm :: Parser Opts
prm = Rm <$> argument str (metavar "PATH") <*> many (argument str (metavar "DIR..."))

pcmd :: Parser Opts
pcmd = subparser $
          command "check" (info check (progDesc "Check repository"))
       <> command "distribute" (info distribute (progDesc "Distribute/update files"))
       <> command "add" (info add (progDesc "Add a path to repositories"))
       <> command "commit" (info commit (progDesc "Commit contents of repositories"))
       <> command "push" (info push (progDesc "Pull --rebase"))
       <> command "pull" (info pull (progDesc "push repositories"))
       <> command "pullpush" (info pullpush (progDesc "Pull --rebase then push repositories"))
       <> command "rm" (info prm (progDesc "Remove a file"))

opts = info (pcmd <**> helper)
      ( fullDesc
     <> progDesc "Manipulate class git repositories"
     <> O.header "g421 - git utilities for CS 421" )

exec (Check []) = return ()
exec (Check (x:xs)) = do
    let output = inproc "git" ["-C", x, "status", "--porcelain"] empty
    l <- fold output Fold.length
    if l == 0 then
      return ()
    else
      echo $ unsafeTextToLine x
    exec (Check xs)

exec (Commit msg xx) =
  threadPool (\dir -> Turtle.proc "git" ["-C", dir, "commit", "-m", msg] empty) xx

exec (Push xx) =
  threadPool (\dir -> Turtle.proc "git" ["-C", dir, "push"] empty) xx

exec (Pull xx) =
  threadPool (\dir -> Turtle.proc "git" ["-C", dir, "pull", "--rebase"] empty) xx

exec (Pullpush xx) =
  threadPool (\dir -> do
                 Turtle.proc "git" ["-C", dir, "pull", "--rebase"] empty
                 Turtle.proc "git" ["-C", dir, "push"] empty) xx

exec (Rm path xx) =
  threadPool (\dir -> Turtle.proc "git" ["-C", dir, "rm", path] empty) xx

exec (Add dest xx) =
  threadPool (\dir -> do
    Turtle.proc "git" ["-C", dir, "add", dest] empty) xx

exec (Distribute src dest msg xx) =
  threadPool (\dir -> do
    let target = dir `mappend` "/" `mappend` dest
    Turtle.proc "rsync" ["-r", src, target] empty
    Turtle.proc "git" ["-C", dir, "add", dest] empty
    Turtle.proc "git" ["-C", dir, "commit", "-m", msg] empty) xx

main = exec =<< customExecParser (prefs showHelpOnEmpty) opts
