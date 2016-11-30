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
import System.IO

run p = do
  op <- createOp p
  unless (term op) (action p op >>= run)

term Halt = True
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

action p (Out a) = (putChar . chr . fromIntegral $ a) >> hFlush stdout >> nextAddr p 

action p (In a) = getChar >>= writeMemAddr p a . fromIntegral . ord >>= nextAddr

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
  In W |
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
  (In _) -> 2
  _ -> 1

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
    20 -> In a
    _ -> Noop
