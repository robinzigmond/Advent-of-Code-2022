{-# LANGUAGE OverloadedStrings #-}

module Day7 (part1, part2) where

import Data.Text (Text)
import qualified Data.Text as T (unpack, lines, take, drop)
import qualified Data.Text.IO as TIO (readFile)
import Data.List (find, sort)

data FileSystemItem = File String Int | Directory String [FileSystemItem]

data Command = ChangeDirectory String | GoToStart | GoUp | List

data OutputItem = OutputFile String Int | OutputDir String

newtype CommandsAndResponses = CommandsAndResponses [(Command, [OutputItem])]

data FileSystemMove = MoveDown String | MoveUp

newtype FileSystemPath = FileSystemPath [FileSystemMove]

data PartialFileSystemItem = FoundFile String Int | PartialDirectory String (Maybe [PartialFileSystemItem])

-- note that the input is assumed to have the "$ " stripped off
parseCommand :: Text -> Command
parseCommand cmd = case T.unpack cmd of
    "cd /" -> GoToStart
    "cd .." -> GoUp
    ('c' : 'd' : ' ' : dirName) -> ChangeDirectory dirName
    "ls" -> List
    other -> error $ "unexpected command: " ++ other

parseOutputItem :: Text -> OutputItem
parseOutputItem output = case T.unpack output of
    ('d' : 'i' : 'r' : ' ' : dirName) -> OutputDir dirName
    other -> let [size, name] = words other in OutputFile name (read size)

parseFile :: Text -> CommandsAndResponses
parseFile fileData = CommandsAndResponses . reverse . buildResult $ T.lines fileData 
    where buildResult output = go [] [] Nothing output
          go :: [(Command, [OutputItem])] -> [OutputItem] -> Maybe Command -> [Text] -> [(Command, [OutputItem])]
          go completedCommands outputSoFar (Just lastCmd) [] = (lastCmd, outputSoFar) : completedCommands
          go completedCommands outputSoFar maybeCommand (nextLine : otherLines)
            | T.take 2 nextLine == "$ " = let actualCmd = parseCommand $ T.drop 2 nextLine
                                          in case maybeCommand of
                                                Just lastCommand ->
                                                     go ((lastCommand, outputSoFar) : completedCommands)
                                                        [] (Just actualCmd) otherLines
                                                Nothing -> go completedCommands [] (Just actualCmd) otherLines
            | otherwise = let nextOutput = parseOutputItem nextLine
                          in go completedCommands (nextOutput : outputSoFar) maybeCommand otherLines

puzzleData :: IO CommandsAndResponses
puzzleData = fmap parseFile $ TIO.readFile "input/input7.txt"

-- now come the functions for building the filesystem from the commands and responses

-- when we process a command, we increase our knowledge of the filesystem, and also may (in case of a cd command)
-- change our current path

updatePath :: FileSystemPath -> Command -> FileSystemPath
updatePath (FileSystemPath currentPath) (ChangeDirectory dir) = FileSystemPath (MoveDown dir : currentPath)
updatePath _ GoToStart = FileSystemPath [] -- GoToStart, ie cd /, only appears at the start
updatePath (FileSystemPath currentPath) List = FileSystemPath currentPath
-- if we're going up, we just remove the most recent move (which by construction must have been a MoveDown)
updatePath (FileSystemPath (_ : rest)) GoUp = FileSystemPath rest

-- simpler helper to get the name of a filesystem item (file or directory)
getName :: PartialFileSystemItem -> String
getName (FoundFile name _) = name
getName (PartialDirectory name _) = name

processOutput :: OutputItem -> PartialFileSystemItem
processOutput (OutputFile name size) = FoundFile name size
processOutput (OutputDir name) = PartialDirectory name Nothing

updateKnowledge :: PartialFileSystemItem -> FileSystemPath -> (Command, [OutputItem]) -> PartialFileSystemItem
-- follow the path to the end. Note we assume only MoveDown commands are there!
updateKnowledge (PartialDirectory name (Just children)) (FileSystemPath (MoveDown childName : rest)) cmd =
    let (Just child) = find ((== childName) . getName) children
        deepReplacement = updateKnowledge child (FileSystemPath rest) cmd
        newChildren = map (\originalChild -> if getName originalChild == childName then deepReplacement else originalChild) children
    in PartialDirectory name (Just newChildren)
-- if we already know the children, leave them in place!
updateKnowledge known@(PartialDirectory _ (Just _)) _ _ = known
updateKnowledge (PartialDirectory name Nothing) _ (List, output) = PartialDirectory name (Just $ map processOutput output)
-- no command other than "ls" can update our knowledge, and we assume we only do that from a directory
updateKnowledge currentPartial _ _ = currentPartial

-- needed at the end to convert a "partial" file system to the correct "known filesystem" type. Assumes it's actually
-- fully known, otherwise it errors
convertToKnown :: PartialFileSystemItem -> FileSystemItem
convertToKnown (FoundFile name size) = File name size
convertToKnown (PartialDirectory name (Just subitems)) = Directory name $ map convertToKnown subitems
convertToKnown (PartialDirectory name Nothing) = error $ "can't convert to known as we don't know what's in directory " ++ name

-- the main function to follow the commands and build knowledge of the file system
buildPartial :: CommandsAndResponses -> PartialFileSystemItem
buildPartial (CommandsAndResponses clues) = recursivelyBuild (FileSystemPath []) (PartialDirectory "/" Nothing) clues
    where recursivelyBuild :: FileSystemPath -> PartialFileSystemItem -> [(Command, [OutputItem])] -> PartialFileSystemItem
          recursivelyBuild _ currentKnowledge [] = currentKnowledge
          recursivelyBuild path@(FileSystemPath actualList) currentKnowledge (firstcmd@(command, _) : rest)
            = let newPath = updatePath path command
                  updatedKnowledge = updateKnowledge currentKnowledge (FileSystemPath $ reverse actualList) firstcmd
              in recursivelyBuild newPath updatedKnowledge rest

buildFileSystem :: CommandsAndResponses -> FileSystemItem
buildFileSystem = convertToKnown . buildPartial

getDirSize :: FileSystemItem -> Int
getDirSize (File _ size) = size
getDirSize (Directory _ items) = sum $ map getDirSize items

getAllDirectories :: FileSystemItem -> [FileSystemItem]
getAllDirectories (File _ _) = []
getAllDirectories (Directory _ children) = concatMap getDirAndChildren children
    where getDirAndChildren (File _ _) = []
          getDirAndChildren directory = directory : getAllDirectories directory

solvePart1 :: CommandsAndResponses -> Int
solvePart1 = getWantedSize . buildFileSystem
    where getWantedSize :: FileSystemItem -> Int
          getWantedSize item = sum . filter (< 100000) . map getDirSize $ getAllDirectories item

part1 :: IO Int
part1 = fmap solvePart1 puzzleData

solvePart2 :: CommandsAndResponses -> Int
solvePart2 input = let fileSystem = buildFileSystem input
                       totalSize = getDirSize fileSystem
                       targetSize = totalSize - 40000000
                   in head $ dropWhile (< targetSize) . sort . map getDirSize $ getAllDirectories fileSystem

part2 :: IO Int
part2 = fmap solvePart2 puzzleData
