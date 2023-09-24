module Main where

import Trie (trieAddWords, trieWords)

main :: IO ()
main = do
    let trie = trieAddWords [] ["bazar", "baz", "bazarr", "babar"]

    let words = trieWords trie

    print trie

    print words
