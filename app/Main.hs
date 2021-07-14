{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  ) where

import Control.Monad
import Data.Aeson (Value(..))
import Data.Bool (bool)
import Data.Either (fromRight)
import Data.Text (Text)
import Prettyprinter (Doc, LayoutOptions(..), layoutSmart, PageWidth(..))
import Prettyprinter.Render.Text (renderStrict)

import qualified Data.Aeson as J
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text.IO as T
import qualified Data.Vector as DV
import qualified System.Environment as IO
import qualified Prettyprinter as PP

intercalate :: Doc ann -> [Doc ann] -> Doc ann
intercalate d as = PP.flatAlt (intercalateGenerous d as) (intercalateCompact d as)

intercalateCompact :: Doc ann -> [Doc ann] -> Doc ann
intercalateCompact _ [a] = a
intercalateCompact d (a:as) = a <> d <> PP.space <> intercalateCompact d as
intercalateCompact _ [] = mempty

intercalateGenerous :: Doc ann -> [Doc ann] -> Doc ann
intercalateGenerous _ [a] = a
intercalateGenerous d (a:as) = a <> PP.line <> d <> PP.space <> intercalateGenerous d as
intercalateGenerous _ [] = mempty

encloseSep2 :: Doc ann -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
encloseSep2 lt rt sep ds = PP.align $ PP.flatAlt (encloseSepGenerous lt rt sep ds) (encloseSepCompact lt rt sep ds)

encloseSepCompact :: Doc ann -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
encloseSepCompact lt rt sep ds = case ds of
  []  -> lt <> rt
  [d] -> lt <> d <> rt
  _   -> lt <> intercalate sep ds <> rt

encloseSepGenerous :: Doc ann -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
encloseSepGenerous lt rt sep ds = case ds of
  []  -> lt <> rt
  [d] -> lt <> PP.space <> d <> PP.line <> rt
  _   -> mconcat (zipWith (<>) ((lt <> PP.space):repeat (PP.line <> sep <> PP.space)) ds) <> PP.line <> rt

renderEntry :: (Text, Value) -> Doc ann
renderEntry kv = PP.group $ PP.flatAlt (renderEntryGenerous kv) (renderEntryCompact kv)

renderEntryCompact :: (Text, Value) -> Doc ann
renderEntryCompact (k, v) = PP.pretty (show k) <> PP.pretty ":" <> PP.space <> render v

renderEntryGenerous :: (Text, Value) -> Doc ann
renderEntryGenerous (k, v) = PP.pretty (show k) <> PP.pretty ":" <> PP.hardline <> PP.space <> PP.space <> PP.align (render v)

renderRecords :: [(Text, Value)] -> Doc ann
renderRecords kvs = encloseSep2 PP.lbrace PP.rbrace PP.comma (fmap renderEntry kvs)

renderElements :: [Value] -> Doc ann
renderElements = encloseSep2 PP.lbracket PP.rbracket PP.comma . fmap render

render :: Value -> Doc ann
render v = case v of
  Object hm -> renderRecords (HM.toList hm)
  Array vs -> renderElements (DV.toList vs)
  Number n -> PP.pretty (show n)
  Bool b -> PP.pretty (bool "false" "true" b)
  String s -> PP.pretty (show s)
  Null -> PP.pretty "null"

main :: IO ()
main = do
  args <- IO.getArgs
  forM_ args $ \arg -> do
    json <- fmap (fromRight Null) (J.eitherDecodeFileStrict @Value arg)

    T.putStrLn $ renderStrict $ layoutSmart (LayoutOptions (AvailablePerLine 80 1)) $ render json
    
    return ()
