#! /usr/bin/env stack
-- stack --no-docker --resolver lts-5.11 --install-ghc runghc --package classy-prelude --package turtle --package optparse-applicative
{-# LANGUAGE EmptyDataDecls, FlexibleContexts, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, LambdaCase,
             MultiWayIf, NamedFieldPuns, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, QuasiQuotes, RankNTypes,
             RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Werror #-}

import ClassyPrelude hiding (print)
import Control.Lens (Prism', preview)
import Control.Lens.TH (makePrisms)
import qualified Data.Attoparsec.Text as AT
import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NEL

data Lisp
  = NilL
  | BoolL Bool
  | IntL Int
  | ConstL Text
  | StringL Text
  | QuotedL Lisp
  | ListL (NonEmpty Lisp)
  deriving (Eq, Show)

makePrisms ''Lisp

parse :: Text -> Either String Lisp
parse = AT.parseOnly lispP
  where
    lispP :: AT.Parser Lisp
    lispP = AT.skipSpace *> ( AT.char '\'' *> (QuotedL <$> lispP)
                              <|> AT.char '"' *> stringP <* AT.char '"'
                              <|> AT.char '(' *> listP <* AT.char ')'
                              <|> primitiveP )

    stringP :: AT.Parser Lisp
    stringP = StringL . concat <$> AT.many' (AT.takeWhile1 (\ ch -> not $ ch == '"'))

    listP :: AT.Parser Lisp
    listP = maybe NilL ListL . NEL.nonEmpty <$> (lispP `AT.sepBy` AT.char ' ')

    primitiveP :: AT.Parser Lisp
    primitiveP = (\ case
                     "true" -> BoolL True
                     "false" -> BoolL False
                     other -> maybe (ConstL other) IntL . readMay $ other
                 ) <$> AT.takeWhile1 (\ ch -> not $ isSpace ch || ch == '(' || ch == ')')

eval :: Lisp -> IO Lisp
eval = \ case
  ListL xs -> operator xs <|> lambda xs
  QuotedL lisp -> pure lisp
  other -> pure other
  where
    lambda _ = fail "boooo"
    operator (x:|xs) = case x of
      ConstL "+" -> IntL . sum <$> expect _IntL "int" xs
      ConstL "-" -> IntL . sum . map negate <$> expect _IntL "int" xs -- heh heh this doesn't do a thing
      ConstL "*" -> IntL . product <$> expect _IntL "int" xs
      ConstL "++" -> StringL . concat <$> expect _StringL "string" xs
      ConstL "&&" -> BoolL . and <$> expect _BoolL "bool" xs
      ConstL "||" -> BoolL . any <$> expect _BoolL "bool" xs
      _ -> fail ""
    expect :: Prism' Lisp a -> String -> [Lisp] -> IO [a]
    expect aPrism explanation = mapM (\ lisp -> eval lisp >>= \ res ->
                                         maybe (fail $ "expected " <> explanation <> ", but got " <> show res <> " instead") pure
                                         . preview aPrism $ res )

main :: IO ()
main = do
  forever $ do
    lisp <- read
    either (print . (<>) "Failed to parse: ") (\ x -> eval x >>= print) lisp
  where
    read = putStr "> " >> parse . pack <$> getLine
    print = putStrLn . tshow
