{-# LANGUAGE OverloadedStrings #-}

module Language.Hee.Parser
  ( parseExpr
  ) where

import Language.Hee.Syntax

import Prelude hiding (takeWhile, length, exponent)
import Control.Applicative hiding (empty)
import Data.Bits (Bits, shiftL, (.|.))
import Data.Char (isOctDigit, isDigit, chr, ord)
import Data.Text (Text, cons, pack, foldl', length)
import Data.Attoparsec.Text hiding (parse)

parseExpr :: Parser (Expression Text)
parseExpr
  = pruneExpr <$> (skipSpace *> loop)
  where
    loop = ExCompose <$> expr <*> (indentLine *> loop <|> parseEmpty)
    expr = parseQuote
       <|> parseLiteral
       <|> parseName
       <|> parseEmpty

pruneExpr :: Expression a -> Expression a
pruneExpr (ExQuote e)     = ExQuote (pruneExpr e)
pruneExpr (ExCompose a b) = compose (pruneExpr a) (pruneExpr b)
  where
    compose m ExEmpty         = m
    compose ExEmpty n         = n
    compose (ExCompose m n) o = ExCompose m (ExCompose n o)
    compose m n               = ExCompose m n
pruneExpr e               = e

parseEmpty :: Parser (Expression Text)
parseEmpty
  = pure ExEmpty

parseName :: Parser (Expression Text)
parseName
  = ExName <$> parseNameId

parseNameId :: Parser Text
parseNameId
  = cons <$> satisfy startChar <*> takeWhile otherChar
  where
    startChar = notInClass " \t\r\n\f\v[]\"'"
    otherChar = notInClass " \t\r\n\f\v[]"

parseQuote :: Parser (Expression Text)
parseQuote
  = parenthesized open inside close
  where
    open   = char '[' *> skipSpace
    close  = skipSpace <* char ']'
    inside = ExQuote <$> parseExpr

parseLiteral :: Parser (Expression Text)
parseLiteral
  = ExLiteral <$> literal
  where
    literal = parseChar
          <|> parseString
          <|> parseFloat
          <|> parseInteger
          <|> parseBool

parseChar :: Parser Literal
parseChar
  = LiChar <$> (delim *> (escapedChar <|> anyChar))
  where
    delim = char '\''

parseString :: Parser Literal
parseString
  = LiString . pack <$> (delim *> inside)
  where
    delim  = char '"'
    inside = manyTill (escapedChar <|> anyChar) delim

parseFloat :: Parser Literal
parseFloat
  = LiFloat <$> (build <$> integer <*> fraction <*> exponent)
  where
    integer  = signed decimal :: Parser Integer
    fraction = parse <$> (char '.' *> takeWhile isDigit)
    parse xs = case parseOnly number xs of
                 Right n -> fromRational (toRational n) / 10 ^^ length xs
                 _       -> 0
    exponent = ((char 'e' <|> char 'E') *> integer) <|> pure 0
    build a b c = (fromIntegral a + b) * 10 ^^ c

parseInteger :: Parser Literal
parseInteger
  = bin <|> oct <|> hex <|> dec
  where
    bin = flip LiInteger Binary      <$> signed ("0b" .*> binary)
    oct = flip LiInteger Octal       <$> signed ("0o" .*> octal)
    hex = flip LiInteger Hexadecimal <$> signed ("0x" .*> hexadecimal)
    dec = flip LiInteger Decimal     <$> signed decimal

parseBool :: Parser Literal
parseBool
  = LiBool <$> (true <|> false)
  where
    true  = "true"  .*> pure True
    false = "false" .*> pure False

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
