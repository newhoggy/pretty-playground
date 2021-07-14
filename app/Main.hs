{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  ) where

import Control.Monad
import Data.Aeson (Value(..))
import Data.Bool (bool)
import Data.Either (fromRight)
import Data.Text (Text)
import Text.PrettyPrint.ANSI.Leijen (Doc)

import qualified Data.Aeson as J
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as DV
import qualified System.Environment as IO
import qualified System.IO as IO
import qualified Text.PrettyPrint.ANSI.Leijen as PP

intercalate :: Doc -> [Doc] -> Doc
intercalate d as = PP.flatAlt (intercalateGenerous d as) (intercalateCompact d as)

intercalateCompact :: Doc -> [Doc] -> Doc
intercalateCompact _ [a] = a
intercalateCompact d (a:as) = a <> d <> PP.space <> intercalateCompact d as
intercalateCompact _ [] = mempty

intercalateGenerous :: Doc -> [Doc] -> Doc
intercalateGenerous _ [a] = a
intercalateGenerous d (a:as) = a <> PP.line <> d <> PP.space <> intercalateGenerous d as
intercalateGenerous _ [] = mempty

encloseSep2 :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep2 lt rt sep ds = PP.align $ PP.flatAlt (encloseSepGenerous lt rt sep ds) (encloseSepCompact lt rt sep ds)

encloseSepCompact :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSepCompact lt rt sep ds = case ds of
  []  -> lt <> rt
  [d] -> lt <> d <> rt
  _   -> lt <> intercalate sep ds <> rt

encloseSepGenerous :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSepGenerous lt rt sep ds = case ds of
  []  -> lt <> rt
  [d] -> lt <> PP.space <> d <> PP.line <> rt
  _   -> mconcat (zipWith (<>) ((lt <> PP.space):repeat (PP.line <> sep <> PP.space)) ds) <> PP.line <> rt

renderEntry :: (Text, Value) -> Doc
renderEntry kv = PP.flatAlt (renderEntryGenerous kv) (renderEntryCompact kv)

renderEntryCompact :: (Text, Value) -> Doc
renderEntryCompact (k, v) = PP.group $ PP.text (show k) <> PP.text ":" <> PP.space <> render v

renderEntryGenerous :: (Text, Value) -> Doc
renderEntryGenerous (k, v) = PP.group $ PP.text (show k) <> PP.text ":" <> PP.line <> PP.space <> PP.space <> PP.align (render v)

renderRecords :: [(Text, Value)] -> Doc
renderRecords kvs = encloseSep2 PP.lbrace PP.rbrace PP.comma (fmap renderEntry kvs)

renderElements :: [Value] -> Doc
renderElements = encloseSep2 PP.lbracket PP.rbracket PP.comma . fmap render

render :: Value -> Doc
render v = case v of
  Object hm -> renderRecords (HM.toList hm)
  Array vs -> renderElements (DV.toList vs)
  Number n -> PP.text (show n)
  Bool b -> PP.text (bool "false" "true" b)
  String s -> PP.text (show s)
  Null -> PP.text "null"

main :: IO ()
main = do
  args <- IO.getArgs
  forM_ args $ \arg -> do
    json <- fmap (fromRight Null) (J.eitherDecodeFileStrict @Value arg)

    PP.displayIO IO.stdout $ PP.renderPretty 0.0 120 $ render json
    return ()
