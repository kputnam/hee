{-# LANGUAGE OverloadedStrings #-}

module Hee.Parser.Term
  ( parseTerm
  , parseFile
  ) where

import Language.Hee.Syntax

import Prelude hiding (takeWhile, length, exponent)
import Control.Applicative hiding (empty)
import Data.Bits (Bits, shiftL, (.|.))
import Data.Char (isOctDigit, isDigit, chr, ord)
import Data.Text (Text, cons, pack, foldl', length)
import Data.Attoparsec.Text hiding (parse)

-- parseFile :: Parser (Bind Text)
-- parseFile
--   = Rec <$> many1 parseBind
-- 
-- parseBind :: Parser (Text, Expr Text)
-- parseBind
--   = (,) <$> name <*> body
--   where
--     name = optional flushLine  *> parseNameId
--     body = indentLine *> "= " .*> parseTerm

parseTerm :: Parser (Expr Text)
parseTerm
  = pruneExpr <$> (skipSpace *> loop)
  where
    loop = Compose <$> expr <*> (indentLine *> loop <|> parseEmpty)
    expr = parseQuote
       <|> parseLiteral
       <|> parseName
       <|> parseEmpty

pruneExpr :: Expr a -> Expr a
pruneExpr (Quote e)     = Quote (pruneExpr e)
pruneExpr (Compose a b) = compose (pruneExpr a) (pruneExpr b)
  where
    compose m Empty         = m
    compose Empty n         = n
    compose (Compose m n) o = Compose m (Compose n o)
    compose m n             = Compose m n
pruneExpr e               = e

parseEmpty :: Parser (Expr Text)
parseEmpty
  = pure Empty

parseName :: Parser (Expr Text)
parseName
  = Name <$> parseNameId

parseNameId :: Parser Text
parseNameId
  = cons <$> satisfy startChar <*> takeWhile otherChar
  where
    startChar = notInClass " \t\r\n\f\v[]\"'"
    otherChar = notInClass " \t\r\n\f\v[]"

parseQuote :: Parser (Expr Text)
parseQuote
  = parenthesized open inside close
  where
    open   = char '[' *> skipSpace
    close  = skipSpace <* char ']'
    inside = Quote <$> parseTerm

parseLiteral :: Parser (Expr Text)
parseLiteral
  = Literal <$> literal
  where
    literal = parseChr
          <|> parseStr
          <|> parseRat
          <|> parseInt

parseChr :: Parser Literal
parseChr
  = Chr <$> (delim *> (escapedChar <|> anyChar))
  where
    delim = char '\''

parseStr :: Parser Literal
parseStr
  = Str . pack <$> (delim *> inside)
  where
    delim  = char '"'
    inside = manyTill (escapedChar <|> anyChar) delim

parseRat :: Parser Literal
parseRat
  = Rat <$> (build <$> integer <*> fraction <*> exponent)
  where
    integer  = signed decimal :: Parser Integer
    fraction = parse <$> (char '.' *> takeWhile isDigit)
    parse xs = case parseOnly number xs of
                 Right n -> fromRational (toRational n) / 10 ^^ length xs
                 _       -> 0
    exponent = ((char 'e' <|> char 'E') *> integer) <|> pure 0
    build a b c = (fromIntegral a + b) * 10 ^^ c

parseInt :: Parser Literal
parseInt
  = bin <|> oct <|> hex <|> dec
  where
    bin = Int Bin <$> signed ("0b" .*> binary)
    oct = Int Oct <$> signed ("0o" .*> octal)
    hex = Int Hex <$> signed ("0x" .*> hexadecimal)
    dec = Int Dec <$> signed decimal

escapedChar :: Parser Char
escapedChar
  = char '\\' *> (number <|> named)
  where
    number = chr  <$> digits <* char ';'
    digits = "0b" .*> binary      -- \0b11..;
         <|> "0o" .*> octal       -- \0o77..;
         <|> "0x" .*> hexadecimal -- \0xFF..;
         <|>          decimal     -- \99....;
    named  = char 'r' *> pure '\r'
         <|> char 'n' *> pure '\n'
         <|> char 't' *> pure '\t'
         <|> char 'v' *> pure '\v'
         <|> char 'f' *> pure '\f'
         <|> anyChar

-------------------------------------------------------------------------------

parenthesized :: Applicative f => f a -> f b -> f c -> f b
parenthesized open inside close
  = open *> inside <* close

octal :: (Integral a, Bits a) => Parser a
octal
  = foldl' step 0 <$> takeWhile1 isOctDigit
  where
    step a c = (a `shiftL` 3) .|. fromIntegral (ord c - 48)

binary :: (Integral a, Bits a) => Parser a
binary
  = foldl' step 0 <$> takeWhile isBinDigit
  where
    step a c     = (a `shiftL` 1) .|. fromIntegral (ord c - 48)
    isBinDigit c = c == '0' || c == '1'

-------------------------------------------------------------------------------

isVerticalSpace :: Char -> Bool
isVerticalSpace c = c == '\n' || c == '\r'

-- Consume whitespace such that the following text begins flush with a new line
flushLine :: Parser ()
flushLine
  =   takeWhile1 isVerticalSpace   *> flushLine'
  <|> takeWhile1 isHorizontalSpace *> flushLine
  where
    flushLine' = flushLine <|> pure ()

-- Consume whitespace such that the following text is not flush with a new line
indentLine :: Parser ()
indentLine
  =   takeWhile1 isVerticalSpace   *> indentLine
  <|> takeWhile1 isHorizontalSpace *> indentLine'
  where
    indentLine' = indentLine <|> pure ()
