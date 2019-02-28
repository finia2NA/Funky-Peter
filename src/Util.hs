module Util where

-- helper function for Maybe: returns wether the argument is a Just or a Nothing
notNothing :: Maybe a -> Bool
notNothing (Just _) = True
notNothing _        = False

unwrap :: Maybe a -> a
unwrap (Just a) = a 