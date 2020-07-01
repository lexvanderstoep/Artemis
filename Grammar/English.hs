-----------------------------------------------------------------------------
-- Module      :  Grammar.English
--
-- A "simplified English" grammar.
-----------------------------------------------------------------------------

module Grammar.English where

data Symbol =
    Sentence
  | Subject
  | Object
  | Verb
  | Article
  | Noun
  | Adjective
  | Terminal String
  deriving (Eq, Show)

type Product = [Symbol]
type Production = (Symbol, [Product])
type Grammar = [Production]

english :: Grammar
english =
  [ (Sentence , [[Subject, Verb, Object]])
  , (Subject  , [[Article, Noun]])
  , (Object   , [[Article, Noun],
                 [Article, Adjective, Noun]])
  , (Verb     , [[Terminal "watches"],
                 [Terminal "hears"],
                 [Terminal "likes"]])
  , (Article  , [[Terminal "a"],
                 [Terminal "the"]])
  , (Noun     , [[Terminal "man"],
                 [Terminal "woman"],
                 [Terminal "bicycle"],
                 [Terminal "book"]])
  , (Adjective, [[Terminal "red"],
                 [Terminal "big"],
                 [Terminal "beautiful"]])
  ]

terminal :: Symbol -> Bool
terminal (Terminal _) = True
terminal _            = False

terminalSentence :: [Symbol] -> Bool
terminalSentence = all terminal

nonTerminal :: Symbol -> Bool
nonTerminal = not . terminal

nonTerminalSentence :: [Symbol] -> Bool
nonTerminalSentence = not . terminalSentence