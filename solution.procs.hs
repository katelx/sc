module Solution.Procs where

import Data.Array.IO
import Data.Array.Base
import Solution.Ops

data Proc = Proc { mem :: IOUArray W W, stack :: [W], addr :: W }

nextAddr :: Proc -> IO Proc
nextAddr p = readMem p >>= return . createOp (addr p) >>= \op -> return p { addr = addr p + sizeOp op }

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
readRegAddr p a = readMemAddr p a >>= \v -> if v < 32768 then return v else readMemAddr p v

writeMemAddr :: Proc -> W -> W -> IO Proc
writeMemAddr p a v = unsafeWrite (mem p) (fromIntegral a) v >> return p
