-----------------------------------------------------------------------------
-- Module      :  Grammar.English.Generate
--
-- Generate random valid sentences of the "simplified English" grammar.
-----------------------------------------------------------------------------

module Grammar.English.Generate where

import Data.Maybe (fromJust)
import System.Random (randomRIO)

import Grammar.English

applyWhileM :: Monad f => f a -> (a -> Bool) -> (a -> f a) -> f a
applyWhileM a p f = do
  v <- a
  if p v then
    applyWhileM (f v) p f
  else
    a

getExpansions :: Symbol -> Maybe [Product]
getExpansions s = lookup s english

expandNonTerminal :: Symbol -> IO [Symbol]
expandNonTerminal s =
    let exps = fromJust (getExpansions s)
    in (exps !!) <$> randomRIO (0, length exps - 1)

expandSentence :: [Symbol] -> IO [Symbol]
expandSentence [] = return []
expandSentence (s:ss) =
    if (nonTerminal s) then
        do
            s' <- expandNonTerminal s
            return (s' ++ ss)
    else
        do
            ss' <- expandSentence ss
            return (s : ss')

generateSentence :: IO [Symbol]
generateSentence =
    applyWhileM (pure [Sentence]) nonTerminalSentence expandSentence

insertInBetween :: a -> [a] -> [a]
insertInBetween _      []  = []
insertInBetween _      [x] = [x]
insertInBetween a (x:y:ys) = x : a : (insertInBetween a (y:ys))

printTerminal :: Symbol -> String
printTerminal (Terminal t) = t
printTerminal _            = ""

printTerminals :: [Symbol] -> String
printTerminals ss = concat $ insertInBetween " " (map printTerminal ss)

main :: IO ()
main = do
    s <- generateSentence
    print $ printTerminals s