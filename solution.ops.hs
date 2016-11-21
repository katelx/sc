module Solution.Ops where
import Data.Word

type W = Word16

data Op =
  Halt |
  Set W W |
  Push W |
  Pop W |
  Eq W W W |
  Gt W W W |
  Jmp W |
  Jt W W |
  Jf W W |
  Add W W W |
  Mul W W W |
  Mod W W W |
  And W W W |
  Or W W W |
  Not W W |
  Rmem W W |
  Wmem W W |
  Call W |
  Ret |
  Out W |
  In |
  Noop deriving (Show)

sizeOp :: Op -> W
sizeOp op = case op of
  (Set _ _) -> 3
  (Push _) -> 2
  (Pop _) -> 2
  (Eq _ _ _) -> 4
  (Gt _ _ _) -> 4
  (Jmp _) -> 2
  (Jt _ _) -> 3
  (Jf _ _) -> 3 
  (Add _ _ _) -> 4
  (Mul _ _ _) -> 4
  (Mod _ _ _) -> 4
  (And _ _ _) -> 4
  (Or _ _ _) -> 4
  (Not _ _) -> 3
  (Rmem _ _) -> 3
  (Wmem _ _) -> 3
  (Call _) -> 2
  (Ret) -> 1
  (Out _) -> 2
  _ -> 1

