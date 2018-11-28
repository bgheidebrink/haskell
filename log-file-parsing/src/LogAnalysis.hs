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
  | nt < t                    = Node
                                  (insert (LogMessage nc nt ns) lnode)
                                  (LogMessage c t s)
                                  rnode
  | nt > t                    = Node
                                  lnode
                                  (LogMessage c t s)
                                  (insert (LogMessage nc nt ns) rnode)
insert lm Leaf = Node Leaf lm Leaf
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Lea   f
-- build [] = Leaf
-- build (x:xs) = insert x (build xs)

-- inOrder :: MessageTree -> [LogMessage]
-- inOrder Node lnode logMessage rnode
--   | lnode == Node
