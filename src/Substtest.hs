import Test.QuickCheck
import Subst
import Term

instance Arbitrary Term where
  arbitrary =
    sized asTerm
     where
      asTerm :: Int -> Gen (Term)
      asTerm s
        | s == 0 = do
          name <- arbitrary
          return (Var name)
        | otherwise = do
          name <- arbitrary
          listLength <- choose (0, s - 1)
          xs <- arbitrary
          return (Comb name xs)
      genN :: Int -> Gen ([Term])
      genN 1 = do
        theTree <- asTerm 0
        return ([theTree])
      genN n = do
        theTree <- asTerm (n-1)
        xs <- genN (n-1)
        return (theTree:xs)