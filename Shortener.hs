import System.Directory (getDirectoryContents, setCurrentDirectory, renameFile)
import System.Environment (getArgs)
import System.FilePath (splitExtension)
import Data.Char -- String helpers

fileExtension :: FilePath -> String
fileExtension = snd . splitExtension

-- Covert string to lowercase
lowercase :: String -> String
lowercase = map toLower 

-- Shorten the string to 4 characters
shorten :: String -> String
shorten = take 4

-- Remove all spaces (' ') from a string
strip :: String -> String
strip [] = []
strip (' ':a) = strip a
strip (a:b) = a : strip b

-- Replace all occurances of one char with another inside a string
replace :: Char -> Char -> String -> String
replace _    _  []      = []
replace from to (x:xs)  = if x == from 
                          then to : replace from to xs
                          else x : replace from to xs

-- Modify the file name to be lowercase, with no whitespace, all .'s replaced with 
-- underscores and shortened to 4 characters.
shortFileName :: FilePath -> String 
-- FIXME: Replace more than just .
shortFileName = shorten . replace '.' '_' . strip . lowercase

-- Perform file reformating, keeping the file extension
formatFile :: FilePath -> String
formatFile path = shortFileName path ++ fileExtension path

-- Returns true if the current file path is not allowed to be renamed
isRenamable :: FilePath -> Bool 
-- Exclude invalid . and .. path renaming
isRenamable path =  (path /= ".") && (path /= "..")

moveFile :: FilePath -> IO ()
moveFile path = do 
    let newPath = formatFile path 

    putStrLn (path ++ "\t==>\t" ++ newPath)
    renameFile path newPath

main :: IO ()
main = do 
    -- FIXME: Handle lack of CMD arguments
    args <- getArgs

    -- Switch so that we perform all operations in this dir, without having to 
    -- prepend the path
    setCurrentDirectory $ head args
    dirContents <- getDirectoryContents "."

    let filesToRename = filter isRenamable dirContents

    mapM_ moveFile filesToRename 
