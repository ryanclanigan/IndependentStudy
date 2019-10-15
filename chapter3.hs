--T defines the terms to be interpreted
data T = Tru
      | Fal
      | If T T T
      | Z
      | Succ T
      | Pred T
      | IsZ T
  deriving Show

--Values, in this case, Bool and Natural numbers
data Value = VTrue
           | VFalse
           | Nat NVal
  deriving Show

--Natural Numbers
data NVal = ZVal
          | SuccVal NVal
  deriving Show


--Evaluator
eval :: T -> Value
eval (Succ term) = Nat (doSucc (Succ term))
eval (Pred Z) =  Nat ZVal
eval (Pred (Succ n)) = eval n
eval Tru = VTrue
eval Fal = VFalse
eval (If a b c) = 
  case (eval a) of VTrue -> eval b
                   VFalse -> eval c
eval Z = Nat ZVal
eval (IsZ term) = case (eval term) of (Nat ZVal) -> VTrue
                                      ___ -> VFalse
-- Evaluates Succ Nats as Values
doSucc :: T -> NVal
doSucc Z = ZVal
doSucc (Succ term) = SuccVal (doSucc term)
