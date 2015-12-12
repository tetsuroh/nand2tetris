module Language.Hack.VM.Compiler (compile) where

import Control.Monad.RWS (RWS, runRWS, get, put, modify, tell, ask)
import Control.Monad (forM)
import Prelude hiding (compare)

import Codec.Hack.Types (Jump(..))
import Language.Hack.VM.Types
import Language.Hack.VM.Parser (parseHackVM)
    
newtype CodeWriterState = CodeWriterState (String, Int) deriving (Show)
    
type CodeWriter a = RWS String () CodeWriterState a

getFilename :: CodeWriter String
getFilename = ask

getVariableCout :: CodeWriter Int
getVariableCout = do
  (CodeWriterState (f, n)) <- get
  put (CodeWriterState (f, n+1))
  return n

getNewLabelName :: String -> CodeWriter String
getNewLabelName s  = do
  fn <- getFilename
  v  <- getVariableCout
  return $ fn ++ ":" ++ s ++ ":label" ++ show v

getCurrentFunctionName :: CodeWriter String
getCurrentFunctionName = do
  (CodeWriterState (f, _)) <- get
  return f
         
setCurrentFunctionName :: String -> CodeWriter ()
setCurrentFunctionName f = do
  (CodeWriterState (_, n)) <- get
  put $ CodeWriterState (f, n)

getValueWithOffsetD :: String -> Int -> String
getValueWithOffsetD s o = unlines ["@" ++ s
                                  ,"D=M"
                                  ,init setAddress
                                  ,"D=M"]
    where
      setAddress = if o < 0 then
                       unlines ["@" ++ show (o*(-1))
                               ,"A=D-A"]
                   else
                       unlines ["@" ++ show o
                               ,"A=D+A"]

getStaticVariableName :: Int -> CodeWriter String
getStaticVariableName i = do
  fileName <- getFilename
  return $ fileName ++ "." ++ show i

(<++>) :: CodeWriter String -> CodeWriter String -> CodeWriter String
a <++> b = do
  a' <- a
  b' <- b
  return $ a' ++ b'

compile :: [(HackVML, String)] -> String
compile sources = let asm = compiles (callSysInit:sources) 0
                  in initCode ++ asm
    where
      compiles :: [(HackVML, String)] -> Int -> String
      compiles [] _ = ""
      compiles ((vml, filename):ss) n = let (c, n')  = compiler vml filename n
                                            cs = compiles ss n'
                                        in c ++ cs
      initCode = unlines ["// init"
                         ,"@256"
                         ,"D=A"
                         ,"@SP"
                         ,"M=D"
                         ,"@LCL"]
      callSysInit = (HackVML [FunctionCall (Call "Sys.init" 0)], "bootstrap")

                 
compiler :: HackVML -> String -> Int -> (String, Int)
compiler (HackVML vml) filename n =
    case runRWS (compile' vml) filename (CodeWriterState ("", n)) of
                                   (s, CodeWriterState (_, x), _) -> (s, x)
    where
      compile' :: [HackVMCommand] -> CodeWriter String
      compile' [] = return ""
      compile' (StackOperation    c:cs) = go stack c cs
      compile' (ArithmeticCommand c:cs) = go arithmetic c cs
      compile' (ProgramFlow       c:cs) = go programFlow c cs
      compile' (FunctionCall      c:cs) = go functionCall c cs
      go f c cs = do
        e1 <- f c
        e2 <- compile' cs
        return $ e1 ++ e2

stack :: StackOperation -> CodeWriter String
stack (Push segment n) = push segment n
stack (Pop  segment n) = pop  segment n

push :: MemorySegment -> Int -> CodeWriter String
push Constant n = return $ pushC n
push Pointer  n = return $ pushM $ show (baseAddress Pointer + n)
push Temp     n = return $ pushM $ show (baseAddress Temp + n)
push Static   n = getStaticVariableName n >>= return . pushM 
push segment  n = return $ unlines ["// push" ++ show segment ++ "[" ++ show n ++ "]"
                                   ,"@" ++ show n
                                   ,"D=A"
                                   ,"@" ++ segmentToLabel segment
                                   ,"A=D+M"
                                   ,"D=M"
                                   ,init pushD]

pop :: MemorySegment -> Int -> CodeWriter String
pop Constant _ = error $ "Unexpected segment to pop: Constant"
pop Pointer  n = return . popS . show $ baseAddress Pointer + n
pop Temp     n = return . popS . show $ baseAddress Temp    + n
pop Static   n = getStaticVariableName n >>= return . popS
pop segment  n = return $ unlines ["// pop " ++ show segment ++ "[" ++ show n ++ "]"
                                  ,"@" ++ show n
                                  ,"D=A"
                                  ,"@" ++ segmentToLabel segment
                                  ,"D=D+M"
                                  ,"@R13 // save target address"
                                  ,"M=D"
                                  ,init popD
                                  ,"@R13"
                                  ,"A=M"
                                  ,"M=D"]

arithmetic :: ArithmeticCommand -> CodeWriter String
arithmetic Add = return $ "// Add\n" ++ popD ++ popM ++ add ++ pushD
arithmetic Sub = return $ "// Sub\n" ++ popD ++ popM ++ sub ++ pushD
arithmetic Neg = return $ "// Neg\n" ++ popD ++ neg ++ pushD
arithmetic Eq  = compare JEQ
arithmetic Gt  = compare JGT
arithmetic Lt  = compare JLT
arithmetic And = return $ popD ++ popM ++ "D=D&M\n" ++ pushD
arithmetic Or  = return $ popD ++ popM ++ "D=D|M\n" ++ pushD
arithmetic Not = return $ popM ++ "D=!M\n" ++ pushD

programFlow :: ProgramFlow -> CodeWriter String
programFlow (Label  s) = do
  functionName <- getCurrentFunctionName
  return $ "(" ++ functionName ++ "$" ++ s ++ ")\n"
programFlow (Goto   s) = do
  functionName <- getCurrentFunctionName
  return $ unlines ["@" ++ functionName ++ "$" ++ s, "0;JMP"]
programFlow (IfGoto s) = do
  functionName <- getCurrentFunctionName
  return $ popD ++ unlines ["@" ++ functionName ++ "$" ++ s, "D;JNE"]
                 
functionCall :: FunctionCall -> CodeWriter String
-- | function f k
--     repeat k times:
--     push 0            // Initialize local variables
functionCall (Function f n) = setCurrentFunctionName f >>
                              return ("(" ++ f ++ ")\n") <++>
                              fmap concat (forM [0..n-1] $ \x ->
                                  return ("// Init " ++ show x ++ "\n") <++>
                                  stack (Push Constant 0) <++>
                                  stack (Pop Local x) <++>
                                  return (unlines ["@SP// incr", "M=M+1"])
                                          )
functionCall (Call     f n) = do returnLabel <- returnAddress
                                 (return "// set return address \n" <++>
                                  pushReturnAddress returnLabel <++>
                                  pushLocal <++>
                                  pushArgument <++>
                                  pushThis <++>
                                  pushThat <++>
                                  setArgument <++>
                                  setLocal <++>
                                  gotoFunction <++>
                                  labelReturnAddress returnLabel)
    where
      pushReturnAddress = return . pushA 
      pushLocal         = return . pushM $ segmentToLabel Local
      pushArgument      = return . pushM $ segmentToLabel Argument
      pushThis          = return . pushM $ segmentToLabel This
      pushThat          = return . pushM $ segmentToLabel That
      setArgument       = return . unlines $ ["@SP"
                                              ,"D=M"
                                              ,"@" ++ show (n + 5)
                                              ,"D=D-A"
                                              ,"@ARG"
                                              ,"M=D"]
      setLocal           = return . unlines $ ["@SP"
                                              ,"D=M"
                                              ,"@LCL"
                                              ,"M=D"]
      gotoFunction       = return . unlines $ ["@" ++ f, "0;JMP"]
      labelReturnAddress = return . (\l -> "(" ++ l ++ ")\n")
      returnAddress      = (return . ("return-address"++) .  show) =<< getVariableCout
functionCall Return         = return "// return \n" <++>
                              -- R13 = return address
                              getValueWithOffset (segmentToLabel Local) (-5) <++>
                              storeValue "R14" <++>
                              -- *ARG = pop() // pop result of function for callee
                              stack (Pop Argument 0) <++>
                              -- SP = ARG + 1
                              setStackPointer <++>
                              substitute That Local (-1) <++>
                              substitute This Local (-2) <++>
                              substitute Argument Local (-3) <++>
                              substitute Local Local (-4) <++>
                              gotoReturnAddress
    where
      getValueWithOffset v n = return $ getValueWithOffsetD v n
      storeValue v = return $ storeValueD v
      substitute' seg = return $ unlines ["@" ++ seg
                                        ,"M=D"]
      substitute seg v n = getValueWithOffset (segmentToLabel v) n <++> 
                           substitute' (segmentToLabel seg)
      setStackPointer = return $ unlines ["@ARG"
                                         ,"D=M"
                                         ,"@SP"
                                         ,"M=D+1"]
      gotoReturnAddress = getValueWithOffset "R14" 0 <++>
                          return (unlines ["0;JMP"])

segmentToLabel :: MemorySegment -> String
segmentToLabel Argument = "ARG"
segmentToLabel Local    = "LCL"
segmentToLabel This     = "THIS"
segmentToLabel That     = "THAT"
segmentToLabel Temp     = "R5"
segmentToLabel segment  = error $ "Unexpected segment to label: " ++ show segment

baseAddress :: MemorySegment -> Int
baseAddress Pointer  = 3
baseAddress Temp     = 5
baseAddress segment  = error $ "Unexpected segment to address: " ++ show segment

storeValueD :: String -> String
storeValueD s = unlines ["@" ++ s
                       ,"M=D"]

pushA :: String -> String
pushA l = unlines ["@" ++ l
                  ,"D=A"
                  ,init pushD]
                
pushC :: Int -> String
pushC n = unlines ["@" ++ show n
                  ,"D=A"
                  ,init pushD]
                   
pushM :: String -> String
pushM var = unlines ["@" ++ var
                    ,"D=M"
                    ,init pushD]

pushD :: String
pushD = unlines ["@SP // Push value in D register to *SP"
               ,"A=M"
               ,"M=D"
               ,"@SP"
               ,"M=M+1"]

popM :: String
popM = unlines ["@SP"
               ,"M=M-1"
               ,"A=M"]

popD :: String
popD = popM ++ unlines ["D=M"]
       
popS :: String -> String
popS symbol = popD ++
              unlines ["@" ++ symbol
                      ,"M=D"]

-- | Assume M is left hand side operand.
add :: String
add = unlines ["D=D+M"]

sub :: String
sub = unlines ["D=M-D"]

neg :: String
neg = unlines ["D=-M"]

compare :: Jump -> CodeWriter String
compare jmp = do
  substruction <- arithmetic Sub
  not' <- arithmetic Not
  filename <- getFilename
  v <- getVariableCout
  let label1 = filename ++ "$compare_true" ++ show v
      label2 = filename ++ "$push_compare_result" ++ show v
  return $ unlines ["// compare"
                   ,init substruction
                   ,"@" ++ label1
                   ,"D;" ++ show jmp
                   ,init popM
                   ,"D=0"
                   ,init pushD
                   ,"@" ++ label2
                   ,"0;JMP"
                   ,"(" ++ label1 ++ ")"
                   ,init popM
                   ,"D=0"
                   ,init pushD
                   ,init not'
                   ,"(" ++ label2 ++ ")"
                   ]

