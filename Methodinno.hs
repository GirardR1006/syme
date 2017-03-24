{-# LANGUAGE PartialTypeSignatures #-}
    
module Methodinno
(Methode(..)
,chgVal
) where

import Data.List

data Methode  =  Methode {nom::String
                       ,domaine::String
                       ,origine::String
                       ,pos::Position
                       ,description::String
                       ,lien::String} 
                       deriving (Show)
data ValeurMethode =  Nom String | Domaine String | Origine String |
                      Pos Position | Description String | Lien String 
                      deriving (Show, Read)
data Position      =  Position (Float,Float) 
                      deriving (Show, Read)




chgVal ::  ValeurMethode -> Methode -> Methode
chgVal (Nom nvnom) am                                 =  am {nom  =  nvnom}
chgVal (Domaine nvdomaine) am                         =  am {domaine  =  nvdomaine}
chgVal (Origine nvorigine) am                         =  am {origine  =  nvorigine}
chgVal (Pos nvposition) am                            =  am {pos  =  nvposition}
chgVal (Description nvdescription) am                 =  am {description  =  nvdescription}
chgVal (Lien nvlien) am                               =  am {lien  =  nvlien}
