module Main (main) where

import Control.Applicative ((<$>))
import Control.Monad (Functor (fmap))
import Data.Char (Char, toUpper)
import Data.Foldable (Foldable (toList))
import Data.Forest
  ( Forest (..),
    Tree (..),
    foldForest,
    foldTree,
    forest,
    leaf,
    leaves,
    tree,
  )
import Data.Function (($), (&), (.))
import Data.List (intercalate)
import Data.Semigroup (Semigroup ((<>)))
import System.IO (IO)
import Test.Hspec (hspec, shouldBe, specify)

main :: IO ()
main = hspec do
  specify "" do
    let example :: Forest Char
        example =
          forest
            [ tree 'a' $ leaves "bc",
              tree 'd' $
                forest
                  [ leaf 'e',
                    tree 'f' $ leaves "g"
                  ]
            ]
    shouldBe
      (example & foldForest (intercalate ", " . fmap (\(a, b) -> [a] <> " [" <> b <> "]")))
      "a [b [], c []], d [e [], f [g []]]"

  specify "" do
    let example :: Tree Char
        example =
          tree 'a' $
            forest
              [ tree 'b' $ leaves "cd",
                tree 'e' $
                  forest
                    [ leaf 'f',
                      tree 'g' $ leaves "h"
                    ]
              ]
    shouldBe
      (example & foldTree \a bs -> [a] <> " [" <> intercalate ", " bs <> "]")
      "a [b [c [], d []], e [f [], g [h []]]]"

  specify "" do
    let example :: Forest Char
        example =
          forest
            [ tree 'a' $ leaves "bc",
              tree 'd' $
                forest
                  [ leaf 'e',
                    tree 'f' $ leaves "g"
                  ]
            ]
        showCharForest f = intercalate ", " (showCharTree <$> trees f)
          where
            showCharTree t = case trees (subforest t) of
              [] -> [root t]
              [t'] -> [root t] <> ": " <> showCharTree t'
              _ -> [root t] <> ": (" <> showCharForest (subforest t) <> ")"
    shouldBe (example & showCharForest) "a: (b, c), d: (e, f: g)"
    shouldBe (example & fmap toUpper & showCharForest) "A: (B, C), D: (E, F: G)"

  specify "" do
    let example :: Forest Char
        example =
          forest
            [ tree 'a' $ leaves "bc",
              tree 'd' $
                forest
                  [ leaf 'e',
                    tree 'f' $ leaves "g"
                  ]
            ]
    shouldBe (example & toList) "abcdefg"
