import Test.QuickCheck
import Subst

instance Arbitrary Term where
  arbitrary =
    sized asTerm
    where
      asTerm :: Int -> Gen (Term)
      asTerm s
      | s == 0 = do
        name = arbtirary
        return (Var name)
      | otherwise = do
        listLength <- choose (0, s - 1)
        treeList