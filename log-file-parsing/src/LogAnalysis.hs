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


-- parseMessage l = LogMessage messageType timeStamp message
--                       where
--                         s = words l
--                         (messageType, timeStamp, message) =
--                           case head split of
--                             'E' ->
--                               (Error read (s !! 1) :: Int,
--                               read (s !! 2) :: Int,
--                               unwords (drop 3 s))
--                             'I' ->
--                               (Info,
--                               read (s !! 2) :: Int,
--                               unwords (drop 3 s))
--                             'W' ->
--                               (Warning,
--                               read (s !! 2) :: Int,
--                               unwords (drop 3 s))

-- parse :: String -> [LogMessage]
