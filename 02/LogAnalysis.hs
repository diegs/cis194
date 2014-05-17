{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage str = case chunks of
  ("E":level:ts:msg) -> LogMessage (Error (parseLevel level)) (parseTs ts) (unwords msg)
  ("W":ts:msg) -> LogMessage Warning (parseTs ts) (unwords msg)
  ("I":ts:msg) -> LogMessage Info (parseTs ts) (unwords msg)
  _ -> Unknown str
  where chunks = words str
        parseTs = read
        parseLevel = read
        
parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m Leaf = Node Leaf m Leaf
insert m1@(LogMessage _ ts1 _) (Node l m2@(LogMessage _ ts2 _) r)
  | ts1 < ts2 = Node (insert m1 l) m2 r
  | otherwise = Node l m2 (insert m1 r)
insert _ (Node _ (Unknown _) _) = error "invalid state"

build :: [LogMessage] -> MessageTree
build msgs = foldr insert Leaf msgs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l msg r) = (inOrder l) ++ (msg:(inOrder r))

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = mapMsgs . filterMsgs . inOrder . build
  where filterMsgs = filter highSeverity
        highSeverity (LogMessage (Error l) _ _) = l >= 50
        highSeverity _ = False
        mapMsgs = map (\(LogMessage _ _ msg) -> msg)
