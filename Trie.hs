module Trie where

data Trie = Trie (Maybe Int) [(Char, Trie)]
    deriving (Show)

empty = Trie Nothing []


example = Trie Nothing [
    ('i', Trie (Just 11) [
        ('n', Trie (Just 5) [('n', Trie (Just 9) [])])
    ]),
    ('t', Trie Nothing [('e', Trie Nothing [
        ('n', Trie (Just 12) []),
        ('d', Trie (Just 4) []),
        ('a', Trie (Just 3) [])
    ]),
    ('o', Trie (Just 7) [])
    ]),
    ('A', Trie (Just 15) [])]

zaza = Trie Nothing [
    ('z', Trie Nothing [
        ('a', Trie (Just 1) [
            ('z', Trie Nothing [
                ('a', Trie (Just 4) [])
            ])
        ]),
        ('o', Trie (Just 2) [
            ('u', Trie (Just 3) [])
        ])
    ])]

trie_char   :: (Char, Trie) -> Char
trie_char   (c, _) = c

trie_value  :: Trie -> Maybe Int
trie_value  (Trie v _) = v

trie_words  :: Trie -> String
trie_words  (Trie Nothing []) = []
trie_words  (Trie _ children) = map trie_char children

trie_first_word :: Trie -> String -> Maybe String
trie_first_word (Trie Nothing []) _ = Nothing
-- If the value is something return the prefix, else, recursievly look depth first.
trie_first_word (Trie v c) p = case v of
    Just value -> Just p
    Nothing -> Nothing
