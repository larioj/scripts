#!/usr/bin/env runhaskell

import           Data.List
import           System.Directory
import           System.Environment
import           System.FilePath.Posix

data Fso = Directory FilePath
         | File FilePath
           deriving(Show, Eq)

printActions :: Bool
printActions = True

main :: IO ()
main = getCurrentDirectoryFsos >>= \directoryFsos ->
       interpretStdinAsFsos >>= \stdinFsos ->
       (createFsos (stdinFsos \\ directoryFsos) >>
       trashFsos (directoryFsos \\ stdinFsos))

putStrLnConditionally :: String -> IO ()
putStrLnConditionally s =
    if' printActions (putStrLn s) (return ())

if' :: Bool -> a -> a -> a
if' True x _  = x
if' False _ y = y

bool :: a -> a -> Bool -> a
bool x _ True  = x
bool _ y False = y

infixl 4 $>>
($>>) :: Functor f => f a -> (a -> b) -> f b
functor $>> function = function <$> functor

getFso :: FilePath -> IO Fso
getFso p = doesDirectoryExist p $>>
           bool (Directory p) (File p)

listCurrentDirectory :: IO [FilePath]
listCurrentDirectory =
    getCurrentDirectory >>= listDirectory

getCurrentDirectoryFsos :: IO [Fso]
getCurrentDirectoryFsos =
    listCurrentDirectory >>= mapM getFso

interpretStdinAsFsos :: IO [Fso]
interpretStdinAsFsos =
    getContents $>> \stdin ->
    map stringToFso (words stdin)

stringToFso :: String -> Fso
stringToFso s =
    if' ("/" `isSuffixOf` s) (Directory (init s)) (File s)

createFsos :: [Fso] -> IO ()
createFsos = mapM_ createFso

createFso :: Fso -> IO ()
createFso (Directory p) = createDirectoryIfNotExists p
createFso (File p)      = createFileIfNotExists p

createDirectoryIfNotExists :: FilePath -> IO ()
createDirectoryIfNotExists p =
    doesPathExist p >>= \exists ->
    if' (not exists)
        (createDirectory p >> putStrLnConditionally ("creating directory " ++ p))
        (putStrLnConditionally ("directory " ++ p ++ " already exists"))

createFileIfNotExists :: FilePath -> IO ()
createFileIfNotExists p  =
    doesPathExist p >>= \exists ->
    if' (not exists)
        (createFile p >> putStrLnConditionally ("creating file " ++ p))
        (putStrLnConditionally ("file " ++ p ++ " already exists"))

createFile :: FilePath -> IO ()
createFile p = writeFile p ""

trashFsos :: [Fso] -> IO ()
trashFsos = mapM_ trashFso

trashFso :: Fso -> IO ()
trashFso (Directory p) = trashDirectory p
trashFso (File p)      = trashFile p

getTrashLocation :: IO FilePath
getTrashLocation = getEnv "HOME" $>> \home ->
                   home </> ".Trash"

trashDirectory :: FilePath -> IO ()
trashDirectory p =
    getTrashLocation >>=
    moveDirectory p >>
    putStrLnConditionally ("trashing directory " ++ p)

trashFile :: FilePath -> IO ()
trashFile p =
    getTrashLocation >>=
    moveFile p >>
    putStrLnConditionally ("trashing file " ++ p)

-- TODO: this requires a relative at the lowest level ie
--       foo.txt or bar/ not bar/foo.txt
moveDirectory :: FilePath -> FilePath -> IO ()
moveDirectory src dest =
    renameDirectory src (dest </> src)

-- TODO: this requires a relative at the lowest level ie
--       foo.txt or bar/ not bar/foo.txt
moveFile :: FilePath -> FilePath -> IO ()
moveFile src dest =
    renameFile src (dest </> src)
