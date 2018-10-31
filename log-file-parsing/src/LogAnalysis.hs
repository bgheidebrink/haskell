{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage m = case words m of
                  ("I":t:s) ->    LogMessage
                                    Info
                                    (read t :: Int)
                                    (unwords s :: String)
                  ("W":t:s) ->    LogMessage
                                    Warning
                                    (read t :: Int)
                                    (unwords s :: String)
                  ("E":i:t:s) ->  LogMessage
                                    (Error (read i :: Int))
                                    (read t :: Int)
                                    (unwords s :: String)
                  s ->            Unknown (unwords s :: String)


parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (LogMessage nc nt ns) (Node lnode (LogMessage c t s) rnode)
  | nt < t && lnode == Leaf   = Node
                                  (Node Leaf (LogMessage nc nt ns) Leaf)
                                  (LogMessage c t s)
                                  rnode
  | nt > t && rnode == Leaf   = Node
                                  lnode
                                  (LogMessage c t s)
                                  (Node Leaf (LogMessage nc nt ns) Leaf)
  | nt < t                    = insert (LogMessage nc nt ns) lnode
  | nt > t                    = insert (LogMessage nc nt ns) rnode
insert (LogMessage c nt s) Leaf = Node Leaf (LogMessage c nt s) Leaf
insert (Unknown _) tree = tree

-- (Node Leaf (LogMessage (Error 70) 4 "Way too many pickles") Leaf)
-- build :: [LogMessage] -> MessageTree
