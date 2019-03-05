import Test.QuickCheck
import Subst
import Term

-- ============================== arbitrary Term ==============================
instance Arbitrary Term where
  arbitrary =
    -- complexity of generated terms:
    --  numberOfArgumentsPerTerm: O(log(sized))
    --  averageTermDepth: O(sized)
    sized asTerm
     where
      asTerm :: Int -> Gen (Term)
      asTerm s
        | s == 0 = do
          name <- arbitrary
          return (Var name)
        | otherwise = do
          name <- arbitrary
          listLength <- choose (0, (s-1))
          termLength <- choose (0, quot s 2)
          xs <- genN listLength termLength
          return (Comb name xs)

      -- generates a list of Length n of Terms of Length m.
      genN :: Int -> Int -> Gen ([Term])
      -- TODO there probably is a better way to write this than do and immediately return
      genN 0 _ = do
        return []
      genN n m = do
        currentTerm <- asTerm m
        xs <- genN (n - 1) (quot m 2)
        return (currentTerm:xs)


-- ============================== arbitrary Substitution =======================
instance Arbitrary Subst where
  arbitrary = sized asSub
   where
    asSub :: Int -> Gen Subst
    asSub n = do
      composeProb <- choose (0, n)
      re <- frequency[(10, genIdentity), (10, genSingle), (n, genCompose n)]
      return re

    genIdentity :: Gen Subst
    genIdentity = do return identity

    genSingle :: Gen Subst
    genSingle = do
      varname <- arbitrary
      term <- arbitrary
      return (single varname term)

    genCompose :: Int ->  Gen Subst
    genCompose n = do
      s1 <- (asSub (quot n 2))
      s2 <- (asSub (quot n 2))
      return (compose s1 s2)

-- ================================ Properties ================================

prop_identity :: Term -> Bool
prop_identity t1 = apply identity t1 == t1

prop_single_easy :: String -> Term -> Bool
prop_single_easy s t = apply (single s t) (Var s) == t

prop_single_hard :: String -> String -> Term -> Property
prop_single_hard o s t = o /= s ==> apply (single s t) (Var o) == Var o

prop_apply_args :: String -> Subst -> [Term] -> Bool
prop_apply_args c s ts = apply s (Comb c ts) == Comb c (map (apply s) ts)

prop_identity_neutral :: Subst -> Term -> Bool
prop_identity_neutral s t = 
  apply (compose identity s) t == apply (compose s identity) t

prop_componse_apply :: Subst -> Subst -> Term -> Bool
prop_componse_apply s1 s2 t = apply (compose s2 s1) t == apply s2 (apply s1 t)