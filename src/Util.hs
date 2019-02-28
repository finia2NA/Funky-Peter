module Util where

-- helper function for Maybe: returns wether the argument is a Just or a Nothing
notNothing :: Maybe a -> Bool
notNothing (Just _) = True
notNothing _        = False

-- Unwraps the Data from a Maybe container. Will throw an exception on Nothing.
unwrap :: Maybe a -> a
unwrap (Just a) = a 