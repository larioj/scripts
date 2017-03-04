#!/usr/bin/env runhaskell

import           Lib.Extensions
import           System.Directory
import           System.Environment
import           System.FilePath.Posix
import           Text.Regex.Posix

data Alias =
    Alias { alias    :: String
          , identity :: String
          } deriving(Show, Eq)

data ShellScript =
    ShellScript { name :: String
                , body :: String
                } deriving(Show, Eq)

main :: IO ()
main = getArgs >>= withUsage

withUsage :: [String] -> IO ()
withUsage [aliasesFile, outputDirectory] = unalias aliasesFile outputDirectory
withUsage _ = getProgName $>> (\prog ->
              "usage: " ++ prog ++ " [aliases file] [output directory]") >>=
              putStrLn

unalias :: FilePath -> FilePath -> IO ()
unalias aliasesFile outputDirectory =
    readFile aliasesFile >>= \content ->
    mapM_ (writeShellScript outputDirectory . aliasToShellScript)
          (extractAliases content)

writeShellScript :: FilePath -> ShellScript -> IO ()
writeShellScript outputDirectory (ShellScript name body) =
    writeFile (outputDirectory </> name) body >>
    putStrLn (name ++ " script written to " ++  (outputDirectory </> name))

extractAliases :: String -> [Alias]
extractAliases content =
    map regexMatchToAlias
        (content =~ "^alias (.*)=['\"]?([^'\"]*)['\"]?$" :: [[String]])

regexMatchToAlias :: [String] -> Alias
regexMatchToAlias [_, alias, identity] = Alias alias identity

aliasToShellScript :: Alias -> ShellScript
aliasToShellScript (Alias alias identity) =
    ShellScript (alias ++ ".sh")
                ("#!/bin/sh\n\n" ++
                  identity ++ " $@\n")
