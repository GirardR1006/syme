{-# LANGUAGE PartialTypeSignatures #-}
    
module Methodinno
(Methode(..)
,modMeth
) where

import Data.List

data Methode  =  Methode {nom::String
                       ,domaine::String
                       ,origine::String
                       ,pos::Localisation
                       ,description::String
                       ,lien::String} deriving (Show)

data Localisation  =  Localisation (Float,Float) deriving (Show)

modMeth :: Methode -> [Either String Localisation]  -> [Methode]
modMeth m []        = m 
modMeth m (x:y:xs)  = chgVal x y m :modMeth xs  

chgVal ::  String -> Either String Localisation -> Methode -> Methode
chgVal "nom" v  am          =  Methode {nom  =  v
                                        ,domaine  =  domaine am
                                        ,origine  =  origine am
                                        ,pos  =  pos am
                                        ,description  =  description am
                                        ,lien  =  lien am}
chgVal "domaine" v  am      =  Methode {nom  =  nom am 
                                        ,domaine  =  v
                                        ,origine  =  origine am
                                        ,pos  =  pos am
                                        ,description  =  description am
                                        ,lien  =  lien am}
chgVal "origine" v  am      =  Methode {nom  =  nom am
                                        ,domaine  =  domaine am
                                        ,origine  =  v
                                        ,pos  =  pos am
                                        ,description  =  description am
                                        ,lien  =  lien am}
chgVal "position" v  am     =  Methode {nom  =  nom am
                                        ,domaine = domaine am
                                        ,origine  =  origine am
                                        ,pos  =  v 
                                        ,description  =  description am
                                        ,lien  =  lien am}
chgVal "description" v  am  =  Methode {nom = nom am
                                        ,domaine = domaine am
                                        ,origine  =  origine am
                                        ,pos  =  pos am
                                        ,description  =  v
                                        ,lien  =  lien am}
chgVal "lien" v  am         =  Methode {nom = nom am
                                        ,domaine = domaine am
                                        ,origine  =  origine am
                                        ,pos  =  pos am
                                        ,description  =  description am
                                        ,lien  =  v}
