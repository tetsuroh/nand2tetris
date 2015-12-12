module Language.Hack.VM.Types where

data ArithmeticCommand = Add 
                       | Sub
                       | Neg
                       | Eq
                       | Gt
                       | Lt
                       | And
                       | Or
                       | Not
                         deriving (Show, Read, Eq)

data MemorySegment = Argument
                   | Local
                   | Static
                   | Constant
                   | This
                   | That
                   | Pointer
                   | Temp
                     deriving (Show, Read, Eq)

data ProgramFlow = Label String
                 | Goto String
                 | IfGoto String
                   deriving (Show, Read, Eq)

data StackOperation = Push MemorySegment Int
                    | Pop  MemorySegment Int
                      deriving (Show, Read, Eq)

data FunctionCall = Function String Int
                  | Call     String Int
                  | Return
                    deriving (Show, Read, Eq)

data HackVMCommand = ArithmeticCommand ArithmeticCommand
                   | StackOperation StackOperation
                   | ProgramFlow ProgramFlow
                   | FunctionCall FunctionCall
                     deriving (Show, Read, Eq)

data HackVML = HackVML [HackVMCommand]
               deriving (Show, Read, Eq)
