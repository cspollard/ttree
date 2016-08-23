{-# LANGUAGE TemplateHaskell #-}

module Data.TTree where


import Control.Lens
import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)
import Data.Text (Text)


data Branch = BDouble Double
            | BInt Int
            | BChar Char
            | BVDouble (Vector Double)
            | BVInt (Vector Int)
            | BVChar (Vector Char)

makePrisms ''Branch


newtype TTree = TTree { _unTTree :: HashMap Text Branch }

makeLenses ''TTree
