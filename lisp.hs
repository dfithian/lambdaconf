#! /usr/bin/env stack
-- stack --no-docker --resolver lts-6.0 --install-ghc runghc --package classy-prelude --package either --package attoparsec
{-# LANGUAGE EmptyDataDecls, FlexibleContexts, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, LambdaCase,
             MultiWayIf, NamedFieldPuns, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, QuasiQuotes, RankNTypes,
             RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Werror #-}

import ClassyPrelude
import Control.Lens (Prism', preview)
import Control.Lens.TH (makePrisms)
import Control.Monad.State.Strict (MonadState, gets, modify, evalStateT)
import Control.Monad.Trans.Either (EitherT(EitherT, runEitherT), left)
import qualified Data.Attoparsec.Text as AT
import Data.Char (isSpace)
import Data.Function (fix)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map

type ReplEnv = Map Text Lisp
newtype Function = Function { unFunction :: forall m . (MonadIO m, MonadState ReplEnv m) => Lisp -> EitherT String m Lisp }

data Lisp
  = NilL
  | BoolL Bool
  | IntL Int
  | ConstL Text
  | StringL Text
  | QuotedL Lisp
  | ListL (NonEmpty Lisp)
  | LambdaL Function

instance Monoid Lisp where
  mempty = NilL
  mappend a b = case (a, b) of
    (NilL, _) -> b
    (_, NilL) -> a
    _ -> ListL $ a:|[b]

instance Show Lisp where
  show NilL = "()"
  show (BoolL b) = show b
  show (IntL i) = show i
  show (ConstL t) = unpack t
  show (StringL t) = "\"" <> unpack t <> "\""
  show (QuotedL l) = "'" <> show l
  show (ListL xs) = "(" <> (intercalate " " . map show $ xs) <> ")"
  show (LambdaL _) = "lambda"

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
    listP = maybe NilL ListL . NEL.nonEmpty <$> lispP `AT.sepBy` AT.char ' '

    primitiveP :: AT.Parser Lisp
    primitiveP = (\ case
                     "true" -> BoolL True
                     "false" -> BoolL False
                     other -> maybe (ConstL other) IntL . readMay $ other
                 ) <$> AT.takeWhile1 (\ ch -> not $ isSpace ch || ch == '(' || ch == ')')

eval :: (MonadIO m, MonadState ReplEnv m) => Lisp -> EitherT String m Lisp
eval lisp = do
  let special :: (MonadIO m, MonadState ReplEnv m) => NonEmpty Lisp -> EitherT String m Lisp
      special (x:|xs) = case x of
        ConstL "define" -> case xs of
          -- when defining an expression, make sure to flatten the arguments
          -- we don't want a NonEmpty here because it's possible to both
          -- `(define add +)` and `(define add (lambda (x y) (+ x y)))`
          ((ConstL name):args) -> modify (Map.insert name (mconcat args)) $> (StringL $ "defined " <> name)
          _ -> left $ "could not define " <> (intercalate " " . map show $ xs)
        ConstL "lambda" -> case xs of
          terms:body:[] -> do
            args <- expect _ConstL "term" terms
            pure . LambdaL $ Function $ \ case
              ListL inputs | length inputs == length args -> do
                -- modify the local state for the lambda, since we don't want arguments escaping this scope
                localState <- gets (Map.union (Map.fromList (args `zip` NEL.toList inputs)))
                EitherT . evalStateT (runEitherT $ eval body) $ localState
              other -> left $ "did not understand inputs: " <> show other
          _ -> left $ "lambda not formatted correctly"
        ConstL name -> do
          f <- maybe (left $ unpack name <> " not defined") eval =<< gets (Map.lookup name)
          args <- mapM eval xs
          pure . ListL $ f:|args
        LambdaL (Function f) -> maybe (left $ "must supply an argument") (f . ListL) . NEL.nonEmpty $ xs
        other -> left $ show other <> " " <> (intercalate ", " . map show $ xs) <> " is not a valid argument list"

  case lisp of
    -- evaluate twice when encountering a list:
    -- first, to extract lambdas
    -- second, to run them
    ListL xs -> eval =<< special xs
    ConstL name -> pure . maybe (ConstL name) id =<< gets (Map.lookup name) -- lookup with fallback
    _ -> pure lisp

-- use fancy prisms to expect a list of some primitive type
expect :: (MonadIO m, MonadState ReplEnv m) => Prism' Lisp a -> String -> Lisp -> EitherT String m [a]
expect aPrism explanation = \ case
  ListL xs -> mapM (\ x -> eval x >>= \ res ->
                       maybe (left $ "expected " <> explanation <> ", but got " <> show res <> " instead") pure
                       . preview aPrism $ res ) $ NEL.toList xs
  other -> left $ "expected a list of " <> explanation <> ", but got " <> show other <> " instead"

main :: IO ()
main = do
  let initialEnv = Map.fromList
        [ ("+", LambdaL $ Function $ \ lisp -> IntL . sum <$> expect _IntL "int" lisp)
        , ("-", LambdaL $ Function $ \ lisp -> IntL . sum . map negate <$> expect _IntL "int" lisp) -- this doesn't do a thing
        , ("*", LambdaL $ Function $ \ lisp -> IntL . product <$> expect _IntL "int" lisp)
        , ("/", LambdaL $ Function $ \ lisp -> do
              ints <- expect _IntL "int" lisp
              IntL <$> maybe (left "not enough arguments") (\ (x:|xs) -> pure $ foldl' div x xs) (NEL.nonEmpty ints) )
        , ("and"  , LambdaL $ Function $ \ lisp -> BoolL . and <$> expect _BoolL "bool" lisp)
        , ("or"   , LambdaL $ Function $ \ lisp -> BoolL . or <$> expect _BoolL "bool" lisp)
        , ("not"  , LambdaL $ Function $ \ lisp -> BoolL . not . or <$> expect _BoolL "bool" lisp)
        , ("print", LambdaL $ Function (pure . StringL . tshow)) ]
      read = putStr "> " >> parse . pack <$> getLine
      runEnv = fix $ \ continue -> do
        lisp <- read
        case lisp of
          Left err -> print ("Failed to parse: " <> err) >> continue
          Right (ConstL ":q") -> putStrLn "bye"
          Right expr -> do
            let onFailure x = print ("Failed: " <> x)
            runEitherT (eval expr) >>= either onFailure print >> continue
  evalStateT runEnv initialEnv
