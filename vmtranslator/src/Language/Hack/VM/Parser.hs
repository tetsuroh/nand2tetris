{-# LANGUAGE OverloadedStrings #-}
module Language.Hack.VM.Parser where

import Text.Parsec hiding (label)
import Text.Parsec.String
import Control.Applicative ((<$>), (<*>), (<*), (*>))

import Language.Hack.VM.Types
import Language.Hack.VM.Lexer (symbol)

comment :: Parser ()
comment = do
  string "//"
  many $ noneOf "\n"
  char '\n'
  return ()

-- \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
-- Arithmetic commands
-- \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
  
add :: Parser ArithmeticCommand
add = string "add" *> return Add

sub :: Parser ArithmeticCommand
sub = string "sub" >> return Sub
      
neg :: Parser ArithmeticCommand
neg = string "neg" >> return Neg

eq :: Parser ArithmeticCommand
eq = string "eq" >> return Eq

gt :: Parser ArithmeticCommand
gt = string "gt" >> return Gt

lt :: Parser ArithmeticCommand
lt = string "lt" >> return Lt

and :: Parser ArithmeticCommand
and = string "and" >> return And

or :: Parser ArithmeticCommand
or = string "or" >> return Or

not :: Parser ArithmeticCommand
not = string "not" >> return Not

arithmeticCommand :: Parser ArithmeticCommand
arithmeticCommand = try add <|>
                    sub <|>
                    try neg <|>
                    eq <|>
                    gt <|>
                    lt <|>
                    Language.Hack.VM.Parser.and <|>
                    Language.Hack.VM.Parser.or <|>
                    Language.Hack.VM.Parser.not

-- \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
-- Stack commands
-- \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
                            
argument :: Parser MemorySegment
argument = string "argument" >> return Argument

local :: Parser MemorySegment
local = string "local" >> return Local

static :: Parser MemorySegment
static = string "static" >> return Static
        
constant :: Parser MemorySegment
constant = string "constant" >> return Constant

this :: Parser MemorySegment
this = string "this" >> return This

that :: Parser MemorySegment
that = string "that" >> return That

pointer:: Parser MemorySegment
pointer= string "pointer" >> return Pointer

temp:: Parser MemorySegment
temp= string "temp" >> return Temp

memorySegment :: Parser MemorySegment
memorySegment = argument <|>
                local <|>
                static <|>
                constant <|>
                try this <|>
                try that <|>
                pointer <|>
                temp

push :: Parser StackOperation
push = do
  string "push"
  spaces
  seg <- memorySegment
  spaces
  d <- many1 digit
  return $ Push seg (read d)

pop :: Parser StackOperation
pop = do
  string "pop"
  spaces
  seg <- memorySegment
  spaces
  d <- many1 digit
  return $ Pop seg (read d)

stackOperation :: Parser StackOperation
stackOperation = try push <|> pop

-- \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
-- Label commands
-- \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_

label :: Parser ProgramFlow
label = do
  string "label"
  spaces
  label_ <- symbol
  return . Label $ label_

goto :: Parser ProgramFlow
goto = do
  string "goto"
  spaces
  label_ <- symbol
  return . Goto $ label_

ifGoto :: Parser ProgramFlow
ifGoto = do
  string "if-goto"
  spaces
  label_ <- symbol
  return . IfGoto $ label_

programFlow :: Parser ProgramFlow
programFlow = label <|> goto <|> ifGoto
              
-- \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
-- Function call commands
-- \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_

functionCall :: Parser FunctionCall
functionCall = function <|> call <|> ret
    where
      function :: Parser FunctionCall
      function = do
        string "function"
        spaces
        functionName <- symbol
        spaces
        localVariableLength <- many1 digit
        return $ Function functionName (read localVariableLength)

      call :: Parser FunctionCall
      call = do
        string "call"
        spaces
        functionName <- symbol
        spaces
        argumentLength <- many1 digit
        return $ Call functionName (read argumentLength)

      ret :: Parser FunctionCall
      ret = do
        string "return"
        return Return

-- \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
-- Hack commands
-- \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_

parserHackVM :: Parser [HackVMCommand]
parserHackVM = do 
  skipMany spaceOrComment
  sepEndBy (a <|> p <|> f <|> s) (skipMany spaceOrComment)
      where
        spaceOrComment = (space >> return ()) <|> comment
        a = ArithmeticCommand <$> try arithmeticCommand
        p = ProgramFlow <$> try programFlow
        f = FunctionCall <$> try functionCall
        s = StackOperation <$> stackOperation


parseHackVM :: String -> String -> Either ParseError HackVML
parseHackVM n s = fmap HackVML $ parse parserHackVM n s
