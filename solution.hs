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
  op_code <- readMem p
  trace (show op_code) (return ())
  let op = createOp (addr p) op_code
  trace (show op) (return ())
  unless (term op) (action p op >>= \pa -> if addr pa == -1 then return () else run pa)

term Halt = True
term _ = False

action p (Set a b) = readMemAddr p a >>= \aa -> readRegAddr p b >>= writeMemAddr p aa >>= nextAddr

action p (Push a) = readRegAddr p a >>= pushStack p >>= nextAddr

action p (Pop a) = readMemAddr p a >>= \aa -> peekStack p >>= writeMemAddr p aa >>= popStack >>= nextAddr

action p (Out a) = readRegAddr p a >>= putChar . chr . fromIntegral >> nextAddr p 

action p (Jmp a) = readRegAddr p a >>= jmpAddr p 

action p (Jt a b) = readRegAddr p a >>= \v -> if v /= 0 then action p (Jmp b) else nextAddr p

action p (Jf a b) = readRegAddr p a >>= \v -> if v == 0 then action p (Jmp b) else nextAddr p

action p (Eq a b c) = actionBinaryCalcSet p a b c (\a b -> if a == b then 1 else 0)

action p (Gt a b c) = actionBinaryCalcSet p a b c (\a b -> if a > b then 1 else 0)

action p (Add a b c) = actionBinaryCalcSet p a b c (+)

action p (Mul a b c) = actionBinaryCalcSet p a b c (*)

action p (Mod a b c) = actionBinaryCalcSet p a b c mod

action p (And a b c) = actionBinaryCalcSet p a b c (.&.)

action p (Or a b c) = actionBinaryCalcSet p a b c (.|.)

action p (Not a b) = actionBinaryCalcSet p a b b (\a _ -> complement a)

action p (Rmem a b) = readRegAddr p b >>= readMemAddr p >>= \v -> readMemAddr p a >>= \wa -> writeMemAddr p wa v >>= nextAddr

action p (Wmem a b) = readRegAddr p b >>= \v -> readRegAddr p a >>= \wa -> writeMemAddr p wa v >>= nextAddr

action p op@(Call a) = pushStack p (addr p + sizeOp op) >>= \pa -> action pa (Jmp a)

action p Ret = peekStack p >>= \a -> popStack p >>= \pa -> action pa (Jmp a)

action p Noop = nextAddr p

action p _ = return p { addr = -1 }

actionBinaryCalcSet p a b c calc = readMemAddr p a >>= \aa -> readRegAddr p b >>= \bb -> readRegAddr p c >>= return . (`rem` 32768) . calc bb >>= writeMemAddr p aa >>= nextAddr

{-action p (Set d v) = uwrite p d v
action p (Pop d) = action p (Set d (head . stack $ p))
action p (Not d a) = action p (Set d (rem (complement a) 32768))
action p (Rmem d a) = uread p { addr = a } >>= \w -> uwrite p d w
action p (Wmem d a) = uwrite p d a
action _ (Out c) = putChar . chr . fromIntegral $ c
action _ _ = return ()

update p (Push v) = p { stack = v:stack p }
update p (Pop _) = p { stack = tail . stack $ p }
update p (Call _) = p { stack = addr p+2:stack p }
update p Ret = p { stack = tail . stack $ p }
update p _ = p
-}

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
  arr <- newArray (0, 65535) 0 :: IO (IOUArray Word16 Word16)
  pmem <- runGet decode <$> B.readFile "challenge.bin" >>= load arr 0
  run $ Proc { mem = pmem, stack = [], addr = 0 }
