{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types.Generalised as G
import Data.GraphViz.Types.Monadic
import Data.Text.Lazy as L
import Data.Word

import System.IO

import STree

stree_graph :: String -> Gr Text Text
stree_graph str = mkGraph v e
  where (v, e) = export_for_graphing . construct $ str

example_graph :: Gr Text Text
example_graph = mkGraph [ (1,"one")
              , (3,"three")
              ]
              [ (1,3,"edge label") ]

params :: GraphvizParams n L.Text L.Text () L.Text
params = nonClusteredParams { globalAttributes = ga
                            , fmtNode          = fn
                            , fmtEdge          = fe
                            }
  where fn (_,l)   = [textLabel l]
        fe (_,_,l) = [textLabel l]

        ga = [ GraphAttrs [ RankDir   FromTop
                          , BgColor   [toWColor White]
                          ]
             , NodeAttrs  [ shape     BoxShape
                          -- , FillColor (some_color 4)
                          -- , style     filled
                          , Width     0.1
                          , Height    0.1
                          ]
             ]

some_color :: Word8 -> ColorList
some_color n | n == 1 = c $ (RGB 127 108 138)
             | n == 2 = c $ (RGB 175 177 112)
             | n == 3 = c $ (RGB 226 206 179)
             | n == 4 = c $ (RGB 172 126 100)
 where c rgb = toColorList [rgb]

my_color :: Word8 -> Attribute
my_color = Color . some_color

main :: IO ()
main = do
  --putStrLn "Enter a string for which to construct a suffix tree."
  str <- getLine
  putStr . unpack . printDotGraph . graphToDot params . stree_graph $ str



