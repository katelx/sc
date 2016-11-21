module Solution.Procs where

import Data.Array.IO
import Data.Array.Base
import Solution.Ops

data Proc = Proc { mem :: IOUArray W W, stack :: [W], addr :: W }

reg0 :: W
reg0 = 32768

memSize :: W
memSize = reg0 + 8

nextAddr :: Proc -> IO Proc
nextAddr p = createOp p >>= \op -> return p { addr = addr p + sizeOp op }

jmpAddr :: Proc -> W -> IO Proc
jmpAddr p j = return p { addr = j }

pushStack :: Proc -> W -> IO Proc
pushStack p a = return p { stack = a:stack p }

peekStack :: Proc -> IO W
peekStack = return . head . stack

popStack :: Proc -> IO Proc
popStack p = return p { stack = tail . stack $ p }

readMem :: Proc -> IO W
readMem p = readMemAddr p (addr p)

readMemAddr :: Proc -> W -> IO W
readMemAddr p a = unsafeRead (mem p) (fromIntegral a)

readRegAddr :: Proc -> W -> IO W
readRegAddr p a = readMemAddr p a >>= \v -> if v < reg0 then return v else readMemAddr p v

writeMemAddr :: Proc -> W -> W -> IO Proc
writeMemAddr p a v = unsafeWrite (mem p) (fromIntegral a) v >> return p

createOp :: Proc -> IO Op
createOp p = do
  let offset = addr p
  code <- readMemAddr p $ offset
  a <- readMemAddr p $ offset + 1
  b <- readMemAddr p $ offset + 2
  c <- readMemAddr p $ offset + 3
  ra <- readRegAddr p $ offset + 1
  rb <- readRegAddr p $ offset + 2
  rc <- readRegAddr p $ offset + 3
  return $ case code of
    0 -> Halt
    1 -> Set a rb
    2 -> Push ra
    3 -> Pop a
    4 -> Eq a rb rc
    5 -> Gt a rb rc
    6 -> Jmp a
    7 -> Jt ra b
    8 -> Jf ra b
    9 -> Add a rb rc
    10 -> Mul a rb rc
    11 -> Mod a rb rc
    12 -> And a rb rc
    13 -> Or a rb rc
    14 -> Not a rb
    15 -> Rmem a rb
    16 -> Wmem ra rb
    17 -> Call ra
    18 -> Ret
    19 -> Out ra
    20 -> In
    _ -> Noop
