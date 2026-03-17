-- ============================================================
--  Text Processing Tool - CLI Haskell Project
--  Features: Count, Search, Replace
-- ============================================================

module Main where

import System.IO        -- for hSetBuffering
import Data.Char        -- for toLower, toUpper
import Data.List        -- for isPrefixOf, intercalate

-- ============================================================
--  MAIN ENTRY POINT
-- ============================================================

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering  -- flush output immediately
    putStrLn "======================================"
    putStrLn "   Welcome to Text Processing Tool   "
    putStrLn "======================================"
    mainMenu

-- ============================================================
--  MAIN MENU
-- ============================================================

mainMenu :: IO ()
mainMenu = do
    putStrLn ""
    putStrLn "Choose an option:"
    putStrLn "  1. Count words, lines, and characters"
    putStrLn "  2. Search for a word/pattern"
    putStrLn "  3. Replace a word in text"
    putStrLn "  4. Exit"
    putStr "Enter choice (1-4): "
    choice <- getLine
    case choice of
        "1" -> countMenu
        "2" -> searchMenu
        "3" -> replaceMenu
        "4" -> putStrLn "Goodbye!"
        _   -> do
            putStrLn "Invalid choice. Please try again."
            mainMenu

-- ============================================================
--  INPUT HELPER: Get text from user or from a file
-- ============================================================

getText :: IO String
getText = do
    putStrLn ""
    putStrLn "How do you want to provide the text?"
    putStrLn "  1. Type text manually"
    putStrLn "  2. Load from a file"
    putStr "Enter choice (1-2): "
    choice <- getLine
    case choice of
        "1" -> do
            putStrLn "Enter your text (press Enter twice when done):"
            readMultiLine ""
        "2" -> do
            putStr "Enter file path: "
            path <- getLine
            content <- readFile path
            putStrLn "File loaded successfully!"
            return content
        _ -> do
            putStrLn "Invalid choice. Defaulting to manual input."
            putStrLn "Enter your text (press Enter twice when done):"
            readMultiLine ""

-- Read multiple lines until user enters an empty line
readMultiLine :: String -> IO String
readMultiLine acc = do
    line <- getLine
    if line == ""
        then return acc
        else readMultiLine (acc ++ "\n" ++ line)

-- ============================================================
--  FEATURE 1: COUNT words, lines, characters
-- ============================================================

countMenu :: IO ()
countMenu = do
    putStrLn "\n--- COUNT MODE ---"
    text <- getText
    let wc = countWords text
        lc = countLines text
        cc = countChars text
    putStrLn "\n====== Results ======"
    putStrLn $ "Words      : " ++ show wc
    putStrLn $ "Lines      : " ++ show lc
    putStrLn $ "Characters : " ++ show cc
    putStrLn "====================="
    backToMenu

-- Count words by splitting on whitespace
countWords :: String -> Int
countWords = length . words

-- Count lines by splitting on newline characters
countLines :: String -> Int
countLines text = length (lines text)

-- Count all characters including spaces
countChars :: String -> Int
countChars = length

-- ============================================================
--  FEATURE 2: SEARCH for a word/pattern
-- ============================================================

searchMenu :: IO ()
searchMenu = do
    putStrLn "\n--- SEARCH MODE ---"
    text <- getText
    putStr "Enter the word/pattern to search: "
    pattern <- getLine
    let results = searchText pattern text
        total   = length results
    putStrLn $ "\n====== Search Results for \"" ++ pattern ++ "\" ======"
    if total == 0
        then putStrLn "No matches found."
        else do
            putStrLn $ "Found " ++ show total ++ " match(es):\n"
            mapM_ printResult (zip [1..] results)
    putStrLn "============================================"
    backToMenu

-- Returns list of (lineNumber, lineContent) where pattern is found
searchText :: String -> String -> [(Int, String)]
searchText pattern text =
    [ (n, l)
    | (n, l) <- zip [1..] (lines text)
    , containsPattern pattern l
    ]

-- Case-insensitive check if a line contains the pattern
containsPattern :: String -> String -> Bool
containsPattern pattern line =
    let lowerPattern = map toLower pattern
        lowerLine    = map toLower line
    in lowerPattern `isSubsequenceOf'` lowerLine

-- Simple substring check
isSubsequenceOf' :: String -> String -> Bool
isSubsequenceOf' [] _  = True
isSubsequenceOf' _  [] = False
isSubsequenceOf' pat str
    | pat `isPrefixOf` str = True
    | otherwise            = isSubsequenceOf' pat (tail str)

-- Print a single search result
printResult :: (Int, (Int, String)) -> IO ()
printResult (_, (lineNum, lineContent)) =
    putStrLn $ "  Line " ++ show lineNum ++ ": " ++ lineContent

-- ============================================================
--  FEATURE 3: REPLACE a word in text
-- ============================================================

replaceMenu :: IO ()
replaceMenu = do
    putStrLn "\n--- REPLACE MODE ---"
    text <- getText
    putStr "Enter the word to find: "
    oldWord <- getLine
    putStr "Enter the replacement word: "
    newWord <- getLine
    let result = replaceAll oldWord newWord text
        count  = countOccurrences oldWord text
    putStrLn $ "\n====== Replaced \"" ++ oldWord ++ "\" with \"" ++ newWord ++ "\" ======"
    putStrLn $ "Total replacements made: " ++ show count
    putStrLn "\n--- New Text ---"
    putStrLn result
    putStrLn "================================"
    putStr "\nDo you want to save the result to a file? (y/n): "
    ans <- getLine
    if ans == "y" || ans == "Y"
        then saveToFile result
        else putStrLn "Result not saved."
    backToMenu

-- Replace all occurrences of oldWord with newWord in text
replaceAll :: String -> String -> String -> String
replaceAll _ _ [] = []
replaceAll old new str
    | old `isPrefixOf` str =
        new ++ replaceAll old new (drop (length old) str)
    | otherwise =
        head str : replaceAll old new (tail str)

-- Count how many times a word appears in text
countOccurrences :: String -> String -> Int
countOccurrences _ [] = 0
countOccurrences old str
    | old `isPrefixOf` str =
        1 + countOccurrences old (drop (length old) str)
    | otherwise =
        countOccurrences old (tail str)

-- ============================================================
--  SAVE TO FILE
-- ============================================================

saveToFile :: String -> IO ()
saveToFile content = do
    putStr "Enter output file name (e.g. output.txt): "
    filename <- getLine
    writeFile filename content
    putStrLn $ "File saved as: " ++ filename

-- ============================================================
--  BACK TO MENU
-- ============================================================

backToMenu :: IO ()
backToMenu = do
    putStrLn ""
    putStr "Press Enter to go back to the main menu..."
    _ <- getLine
    mainMenu
