module Util where

-- | helper function for Maybe: returns whether the argument is a Just or Nothing
notNothing :: Maybe a -> Bool
notNothing (Just _) = True
notNothing _        = False

-- | Unwraps the Data from a Maybe container. Will throw an exception on Nothing.
unwrap :: Maybe a -> a
unwrap (Just a) = a 
unwrap Nothing = error "Tried to unwrap a Nothing instance of Maybe type."
