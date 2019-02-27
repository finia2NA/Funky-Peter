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
          termList <- [term | tlength <- choose(0, s-1), term <- asTerm tlength]
          return (Comb name termList)