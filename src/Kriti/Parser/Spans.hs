{-# LANGUAGE DeriveFunctor #-}
module Kriti.Parser.Spans where

------------------------
--- Source Positions ---
------------------------

data AlexSourcePos = AlexSourcePos { line :: !Int , col :: !Int }
  deriving (Show, Read, Eq, Ord)

overCol :: (Int -> Int) -> AlexSourcePos -> AlexSourcePos
overCol f (AlexSourcePos line col) = AlexSourcePos line (f col)

overLine :: (Int -> Int) -> AlexSourcePos -> AlexSourcePos
overLine f (AlexSourcePos line col) = AlexSourcePos (f line) col

alexStartPos :: AlexSourcePos
alexStartPos = AlexSourcePos 1 1

-------------
--- Spans ---
-------------

data Span = Span { start :: AlexSourcePos, end :: AlexSourcePos }
  deriving (Show, Read, Eq, Ord)

instance Semigroup Span where
  (Span s1 e1) <> (Span s2 e2) = Span (min s1 s2) (max e1 e2)

overStart :: (AlexSourcePos -> AlexSourcePos) -> Span -> Span
overStart f (Span start end) = Span (f start) end

overEnd :: (AlexSourcePos -> AlexSourcePos) -> Span -> Span
overEnd f (Span start end) = Span start (f end)

setStart :: AlexSourcePos -> Span -> Span
setStart sp (Span _ end) = Span sp end

setEnd :: AlexSourcePos -> Span -> Span
setEnd sp (Span start _) = Span start sp

-----------------
--- Locations ---
-----------------

data Loc a = Loc Span a
  deriving (Show, Eq, Ord, Functor)

unlocate :: Loc a -> a
unlocate (Loc _ a) = a

class Located a where
  locate :: a -> Span

instance Located Span where
  {-# INLINE locate #-}
  locate x = x

instance Located (Loc a) where
  {-# INLINE locate #-}
  locate (Loc sp _) = sp

instance (Located a, Located b) => Located (Either a b) where
  {-# INLINE locate #-}
  locate = \case
    Left a -> locate a
    Right b -> locate b
    
