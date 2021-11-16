module Main (main) where

import           Control.Applicative (Applicative (pure, (<*>)), (<$>))
import           Control.Monad       (Functor (fmap), Monad (return, (>>=)))
import           Data.Bool           (Bool, (&&))
import           Data.Char           (Char, toUpper)
import           Data.Eq             (Eq ((==)))
import           Data.Foldable       (Foldable (null, toList))
import           Data.Forest         (Forest (..), Tree (..), foldForest,
                                      foldTree, forest, leaf, leaves, tree)
import           Data.Function       (($), (.))
import           Data.List           (intercalate, map)
import           Data.Semigroup      (Semigroup ((<>)))
import           Numeric.Natural     (Natural)
import           System.Exit         (die)
import           System.IO           (IO, putStrLn)
import           Text.Show           (Show (show))

main :: IO ()
main = dieIfFailures $ do
    test 1 $
        let
            example :: Forest Char
            example = forest
                [ tree 'a' $ leaves "bc"
                , tree 'd' $ forest
                    [ leaf 'e'
                    , tree 'f' $ leaves "g"
                    ]
              ]
        in
            foldForest ( intercalate ", " .
                           fmap (\(a, b) -> [a] <> " [" <> b <> "]")
                       )
                       example
              == "a [b [], c []], d [e [], f [g []]]"

    test 2 $
        let
            example :: Tree Char
            example = tree 'a' $ forest
                [ tree 'b' $ leaves "cd"
                , tree 'e' $ forest
                    [ leaf 'f'
                    , tree 'g' $ leaves "h"
                    ]
              ]
        in
            foldTree (\a bs ->
                        [a] <> " [" <> intercalate ", " bs <> "]"
                     )
                     example
              == "a [b [c [], d []], e [f [], g [h []]]]"

    test 3 $
        let
            example :: Forest Char
            example = forest
                [ tree 'a' $ leaves "bc"
                , tree 'd' $ forest
                    [ leaf 'e'
                    , tree 'f' $ leaves "g"
                    ]
              ]
            showCharForest f =
                intercalate ", " (showCharTree <$> trees f)
              where
                showCharTree t = case trees (subforest t) of
                  []   -> [root t]
                  [t'] -> [root t] <> ": " <> showCharTree t'
                  _    -> [root t] <> ": (" <> showCharForest (subforest t) <> ")"
        in
            showCharForest example
              == "a: (b, c), d: (e, f: g)"
            &&
            showCharForest (fmap toUpper example)
              == "A: (B, C), D: (E, F: G)"

    test 4 $
        let
            example :: Forest Char
            example = forest
                [ tree 'a' $ leaves "bc"
                , tree 'd' $ forest
                    [ leaf 'e'
                    , tree 'f' $ leaves "g"
                    ]
              ]
        in
            toList example == "abcdefg"

dieIfFailures :: Failures a -> IO a
dieIfFailures (Failures fs x) =
    if null fs
        then do putStrLn "💯"; return x
        else die $ intercalate " " (map (("🔥" <> ) . show) fs)

type TestNumber = Natural

test :: TestNumber -> Bool -> Failures ()
test n t = Failures (if t then [] else [n]) ()

data Failures a = Failures [TestNumber] a

instance Functor Failures
  where
    fmap f (Failures a x) = Failures a (f x)

instance Applicative Failures
  where
    pure x = Failures [] x
    Failures a f <*> Failures b x = Failures (a <> b) (f x)

instance Monad Failures
  where
    Failures a x >>= f = let Failures b y = f x in Failures (a <> b) y
