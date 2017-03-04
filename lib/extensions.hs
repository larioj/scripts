module Lib.Extensions where

if' :: Bool -> a -> a -> a
if' True x _  = x
if' False _ y = y

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (a : _) = Just a

bool :: a -> a -> Bool -> a
bool x _ True  = x
bool _ y False = y

infixl 4 $>>
($>>) :: Functor f => f a -> (a -> b) -> f b
functor $>> function = function <$> functor