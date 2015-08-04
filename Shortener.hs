import System.Directory (getDirectoryContents, setCurrentDirectory, renameFile, doesFileExist)
import System.Environment (getArgs)
import System.FilePath (takeExtension, takeFileName, splitPath)
import System.Exit (exitWith, ExitCode(..))
import Data.Char -- String helpers
import Data.List 
import Control.Monad (when)
import System.Directory.Tree (
    AnchoredDirTree(..), DirTree(..),
    filterDir, readDirectoryWith
    )

-- Covert string to lowercase
lowercase :: String -> String
lowercase = map toLower

-- Shorten the string to 4 characters
shorten :: String -> String
shorten = take 4

-- Remove all spaces (' ') from a string
strip :: String -> String
strip = filter (/= ' ')

-- Replace all occurances of one char with another inside a string
replace :: Char -> Char -> String -> String
replace from to = map (\c -> if c == from then to else c)

-- Modify the file name to be lowercase, with no whitespace, all .'s replaced with
-- underscores and shortened to 4 characters.
shortFileName :: FilePath -> String
-- FIXME: Replace more than just .
shortFileName = shorten . strip . replace '.' ' ' . lowercase . takeFileName

-- Perform file reformating, keeping the file extension
formatFile :: FilePath -> String
formatFile path = dirPath ++ shortFileName path ++ takeExtension path
    where dirPath = concat $ init (splitPath path)

-- Returns true if the current file is a hidden file
isHidden :: FilePath -> Bool
-- Exclude invalid . and .. path renaming
isHidden path = "." `isPrefixOf` path

moveFile :: FilePath -> IO ()
moveFile path = do
    doesExist <- doesFileExist path

    when doesExist $ do
        let newPath = formatFile path

        putStrLn (path ++ "\t==>\t" ++ newPath)
        renameFile path newPath


preorder :: DirTree a -> [a] 
preorder (Failed name err)      = []
preorder (Dir name contents)    = concat (map preorder contents)
preorder (File name x)          = [x]

listFiles :: FilePath -> IO [FilePath]
listFiles path = do 
    _:/tree <- readDirectoryWith return "."
    return . preorder $ filterDir pred tree 
  where pred (Dir ('.':_) _)    = False 
        pred (File ('.':_) _)   = False 
        pred _                  = True

checkArgs :: [a] -> IO ()
checkArgs [] = do
    putStrLn "Usage: shortener PATH [OPTIONS]"
    exitWith $ ExitFailure 1
-- Just a stub, does nothing
checkArgs _ = return ()

main :: IO ()
main = do
    -- FIXME: Handle lack of CMD arguments
    args <- getArgs

    checkArgs args

    listFiles (head args) >>= mapM_ moveFile
