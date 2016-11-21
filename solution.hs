import Debug.Trace
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Lazy as B
import Data.Binary.Get
import Data.Word
import Data.Char
import Data.Array.IO
import Data.Array.Base
import Data.Bits
import Solution.Ops
import Solution.Procs

run p = do
  op <- createOp p
  trace (show op) (return ())
  unless (term op) (action p op >>= run)

term Halt = True
term In = True
term _ = False

action p (Set a b) = writeMemAddr p a b >>= nextAddr

action p (Push a) = pushStack p a >>= nextAddr

action p (Pop a) = peekStack p >>= writeMemAddr p a >>= popStack >>= nextAddr

action p (Jmp a) = jmpAddr p a

action p (Jt a b) = if a /= 0 then action p (Jmp b) else nextAddr p

action p (Jf a b) = if a == 0 then action p (Jmp b) else nextAddr p

action p (Eq a b c) = actionBinaryCalcSet p a b c (\a b -> if a == b then 1 else 0)

action p (Gt a b c) = actionBinaryCalcSet p a b c (\a b -> if a > b then 1 else 0)

action p (Add a b c) = actionBinaryCalcSet p a b c (+)

action p (Mul a b c) = actionBinaryCalcSet p a b c (*)

action p (Mod a b c) = actionBinaryCalcSet p a b c mod

action p (And a b c) = actionBinaryCalcSet p a b c (.&.)

action p (Or a b c) = actionBinaryCalcSet p a b c (.|.)

action p (Not a b) = actionBinaryCalcSet p a b b (\a _ -> complement a)

action p (Rmem a b) = readMemAddr p b >>= writeMemAddr p a >>= nextAddr

action p (Wmem a b) = writeMemAddr p a b >>= nextAddr

action p op@(Call a) = pushStack p (addr p + sizeOp op) >>= \pa -> action pa (Jmp a)

action p Ret = peekStack p >>= \a -> popStack p >>= \pa -> action pa (Jmp a)

action p (Out a) = (putChar . chr . fromIntegral $ a) >> nextAddr p 

--action p (In a) = 

action p Noop = nextAddr p

actionBinaryCalcSet p a b c calc = writeMemAddr p a (rem (calc b c) reg0) >>= nextAddr

decode = do
  empty <- isEmpty
  if empty
    then return []
    else do op <- getWord16le
            ops <- decode
            return (op:ops)

load mem _ [] = return mem
load mem addr (op:ops) = unsafeWrite mem addr op >> load mem (addr+1) ops

main = do
  arr <- newArray (0, memSize) 0 :: IO (IOUArray Word16 Word16)
  pmem <- runGet decode <$> B.readFile "challenge.bin" >>= load arr 0
  run $ Proc { mem = pmem, stack = [], addr = 0 }
