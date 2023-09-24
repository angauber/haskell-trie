module Trie (trieWords, trieAddWord, trieAddWords) where

import Data.Foldable
import Data.Hashable 

data Trie = Trie Char (Maybe Int) [Trie]
    deriving (Show)

type TrieRoot = [Trie]

empty :: TrieRoot
empty = []

example :: TrieRoot
example = [
    Trie 't' Nothing [
        Trie 'o' (Just 7) []
        , Trie 'e' Nothing [
            Trie 'a' (Just 3) []
            , Trie 'd' (Just 4) []
            , Trie 'n' (Just 12) []
        ]
    ]
    , Trie 'A' (Just 15) []
    , Trie 'i' (Just 11) [ Trie 'n' (Just 5) [ Trie 'n' (Just 9) [] ] ] ]

trieChar   :: Trie -> Char
trieChar   (Trie c _ _) = c

trieValue  :: Trie -> Maybe Int
trieValue  (Trie _ v _) = v

trieLeafs  :: Trie -> [Trie]
trieLeafs  (Trie _ _ []) = []
trieLeafs  (Trie _ _ xs) = xs

trieWords   :: TrieRoot -> [String]
trieWords   [] = []
trieWords   t = trieWordsPrefix "" t

trieWordsPrefix :: String -> TrieRoot -> [String]
trieWordsPrefix _ [] = []
trieWordsPrefix p (lt:xs) = case trieValue lt of
    Just v -> [ltp] ++ trieWordsPrefix ltp (trieLeafs lt) ++ trieWordsPrefix p xs
    Nothing -> trieWordsPrefix ltp (trieLeafs lt) ++ trieWordsPrefix p xs
    where ltp = p ++ [trieChar lt]

trieAddWord     :: TrieRoot -> String -> TrieRoot
trieAddWord     tr "" = tr
trieAddWord     tr s = trieAddHashed tr (s, hash s)

trieAddWords    :: TrieRoot -> [String] -> TrieRoot
trieAddWords    = foldl trieAddWord

trieRootChars   :: TrieRoot -> String
trieRootChars   [] = ""
trieRootChars   tr = map trieChar tr

trieWithValue   :: Trie -> Maybe Int -> Trie
trieWithValue   t v = Trie (trieChar t) v (trieLeafs t)

trieWithLeafs   :: Trie -> [Trie] -> Trie
trieWithLeafs   t = Trie (trieChar t) (trieValue t)

trieAddHashed   :: TrieRoot -> (String, Int) -> TrieRoot
trieAddHashed   tr ("", _) = tr
trieAddHashed   [] (x:xs, h) = [Trie x (if null xs then Just h else Nothing) (trieAddHashed [] (xs, h))]
trieAddHashed   tr (x:xs, h) = if x `elem` trieRootChars tr
    then map (\l -> if trieChar l == x then (if null xs then trieWithValue l (Just h) else trieWithLeafs l (trieAddHashed (trieLeafs l) (xs, h))) else l) tr
    else tr ++ trieAddHashed [] (x:xs, h)

