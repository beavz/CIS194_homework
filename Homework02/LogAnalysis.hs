-- UPenn CIS194 | Spring '13
-- Homework 2 | KRB

{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Exercise 1

parseMessage :: String -> LogMessage
parseMessage = parseWordList . words where
  parseWordList :: [String] -> LogMessage
  parseWordList ("I":time:msg) = LogMessage Info (read time) (unwords msg)
  parseWordList ("W":time:msg) = LogMessage Warning (read time) (unwords msg)
  parseWordList ("E":severity:time:msg) = LogMessage (Error (read severity)) (read time) (unwords msg)
  parseWordList msg = Unknown (unwords msg)
  -- this is problems if the message begins with I, W, or E but is otherwise not properly formatted

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines


-- Exercise 2
-- right child = bigger; left, smaller

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ msgTime _) (Node left mid@(LogMessage _ midTime _) right)
  | (msgTime > midTime)  = Node left mid (insert msg right)
  | (msgTime <= midTime) = Node (insert msg left) mid right
insert _ _ = error "Why is there an Unknown in your tree?"

-- Exercise 3

build :: [LogMessage] -> MessageTree
build (a:as) = insert a (build as)
build [] = Leaf

-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder (Node left mid right) = (inOrder left) ++ [mid] ++ (inOrder right)
inOrder Leaf                  = []

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages = inOrder . build

-- Exercise 5

getSeriousErrors :: [LogMessage] -> [LogMessage]
getSeriousErrors (e@(LogMessage (Error x) _ _):others)
  | x >= 50   = [e] ++ (getSeriousErrors others)
  | otherwise = getSeriousErrors others
getSeriousErrors (_:others) = getSeriousErrors others
getSeriousErrors [] = []

message :: LogMessage -> String
message (LogMessage _ _ msg) = msg
message (Unknown msg)        = msg

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map message) . inOrder . build . getSeriousErrors
