{-# LANGUAGE PartialTypeSignatures #-}
    
module Methodinno
(Methode(..)
,Position(..)
,chgVal
) where

import Data.List as List
data ValeurMethode =  Nom String | Domaine String | Origine String |
                      Pos Position | Description String | Lien String 
                      deriving (Show, Read)
data Position      =  Position (Absc,Ordn) deriving (Show)
instance Read Position where
    readsPrec _ str = let ('(':absc,',':ordn') = break (==',') str
                          (ordn,')':str') = break (==')') ordn'
                      in [(Position (read absc, read ordn), str')]
type Absc = Int
type Ordn = Int
data Methode  =  Methode {nom::String
                         ,domaine::String
                         ,origine::String
                         ,pos::Position
                         ,description::String
                         ,lien::String} 
                       deriving (Show)



chgVal ::  ValeurMethode -> Methode -> Methode
chgVal (Nom nvnom) am                                 =  am {nom  =  nvnom}
chgVal (Domaine nvdomaine) am                         =  am {domaine  =  nvdomaine}
chgVal (Origine nvorigine) am                         =  am {origine  =  nvorigine}
chgVal (Pos nvposition) am                            =  am {pos  =  nvposition}
chgVal (Description nvdescription) am                 =  am {description  =  nvdescription}
chgVal (Lien nvlien) am                               =  am {lien  =  nvlien}
