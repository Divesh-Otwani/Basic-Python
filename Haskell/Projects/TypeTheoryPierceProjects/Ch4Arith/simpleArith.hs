{-# LANGUAGE GADTs #-}
module SimpleArith where


-- note: should probably re try this my way later.
data Term where
  TTrue :: Term
  TFalse :: Term
  TIf :: Term -> Term -> Term -> Term
  TZero :: Term
  TSucc :: Term -> Term
  TPred :: Term -> Term
  TIsZero :: Term -> Term
  deriving (Show, Eq)


isnum :: Term -> Bool
isnum TZero = True
isnum (TSucc _) = True
isnum _ = False

isval :: Term -> Bool
isval TTrue = True
isval TFalse = True
isval t = isnum t


-- simplifies one step if it can
evalOneStep :: Term -> Maybe Term
evalOneStep (TIf TTrue t2 _) = Just t2
evalOneStep (TIf TFalse _ t3) = Just t3
evalOneStep (TIf t1 t2 t3) = do
  t1step <- evalOneStep t1
  return $ TIf t1step t2 t3
evalOneStep (TPred TZero) = Just TZero
evalOneStep (TPred (TSucc nv)) | isnum nv = Just nv
evalOneStep (TPred notnv) = do
  next <- evalOneStep notnv
  return $ TPred next
evalOneStep (TIsZero TZero) = Just TTrue
evalOneStep (TIsZero (TSucc nv)) | isnum nv = Just TFalse
evalOneStep (TIsZero notnv) = do
  next <- evalOneStep notnv
  return $ TIsZero next
evalOneStep (TSucc tm) = do
  next <- evalOneStep tm
  return $ TSucc next
evalOneStep _ = Nothing



evaluate :: Term -> Term
evaluate t = case evalOneStep t of
  Just next -> evaluate next
  Nothing -> t





