module Methodinno
(Methode(..)
) where

data Methode  =  Methode {nom::String
                         ,domaine::String
                         ,origine::String
                         ,description::String
                         ,lien::String
                         ,sourceName::String} 
                         deriving (Show)
