#!/usr/bin/env runhaskell

import System.Process
import System.IO

ls :: String -> IO [String]
ls dir =
    createProcess (proc "9p" ["ls", dir]){ 
      std_out = CreatePipe,
      std_err = CreatePipe
    } >>= \(_, Just hout, _, _) ->
    hGetContents hout >>=
    return . lines

main = tree 20 "" "" "acme"

tree :: Int -> String -> String -> String -> IO ()
tree 0 _ _ _ = return ()
tree n pad base root =
    ls (base ++ root) >>= \dirs ->
    putStrLn (pad ++ root) >>
    if (null dirs)
    then
      return ()
    else
      mapM_ (tree (n - 1) ( pad ++ "    ") (base ++ root ++ "/")) dirs
      