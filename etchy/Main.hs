{-# LANGUAGE OverloadedStrings #-}

module Main where

import Etch.Parser as Parser

main :: IO ()
main = print (Parser.parse "1  = \n1")
