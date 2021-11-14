-- Functor, Foldable and Traversable in Haskell unfortunately work only with
-- the last type parameter. The following boilerplate alternatively defines
-- MTFun, MTFol and MTTra, which can specify fmaps, foldMaps and traversals
-- over any type parameter (or more type parameters at once). As a bonus, we
-- can specify distinct mapping/fold/traversal functions for each data
-- constructor using MFun, MFol, MTra.

-- As for the naming, MFun is an abbreviation of Multifunctor, MTFun is
-- something like Multifunctor for type parameters.


{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic, Generic1)
import Data.Functor.Classes (Eq1, Show1)
import Generic.Data (Generically1(..))
import Generic.Data.Orphans ()
import Data.Composition ((.:))
import System.Random
import Control.Monad
import Data.Functor

----------------------------------------------------------------------------------------
-- Foo and its plate (boilerplate) -----------------------------------------------------

data Foo a b
  = Val
  | OpAA a a
  | OpAB a b
  | OpBs [b]
  deriving (Functor, Foldable, Traversable, Show, Eq, Generic, Generic1)
  deriving (Eq1, Show1) via (Generically1 (Foo a))

-- MFun, MTFun - Functor over any data constructor or type parameter -------------------

data MFun a b a' b' = MFun
  { mfunVal :: forall a' b'. Foo a' b'
  , mfunOpAA :: forall b'. a -> a -> Foo a' b'
  , mfunOpAB :: a -> b -> Foo a' b'
  , mfunOpBs :: forall a'. [b] -> Foo a' b'
  }

{-# INLINE mfun0 #-}
mfun0 :: MFun a b a b
mfun0 = MFun
  { mfunVal = Val
  , mfunOpAA = OpAA
  , mfunOpAB = OpAB
  , mfunOpBs = OpBs
  }

{-# INLINE appMFun #-}
appMFun :: MFun a b a' b' -> Foo a b -> Foo a' b'
appMFun MFun{..} = \case
  Val -> mfunVal
  OpAA a1 a2 -> mfunOpAA a1 a2
  OpAB a b -> mfunOpAB a b
  OpBs bs -> mfunOpBs bs

data MTFun a b a' b' = MTFun
  { mtfunA :: a -> a'
  , mtfunB :: b -> b'
  }

{-# INLINE mtfun0 #-}
mtfun0 :: MTFun a b a b
mtfun0 = MTFun id id

{-# INLINE fromMTFun #-}
fromMTFun :: MTFun a b a' b' -> MFun a b a' b'
fromMTFun MTFun{..} = mfun0
  { mfunOpAA = \a1 a2 -> OpAA (mtfunA a1) (mtfunA a2)
  , mfunOpAB = \a b -> OpAB (mtfunA a) (mtfunB b)
  , mfunOpBs = \bs -> OpBs (map mtfunB bs)
  }

{-# INLINE appMTFun #-}
appMTFun :: MTFun a b a' b' -> Foo a b -> Foo a' b'
appMTFun = appMFun . fromMTFun

-- MFol, MTFol - Foldable over any data constructor or type parameter ------------------

data MFol a b m = MFol
  { mfolVal :: m
  , mfolOpAA :: a -> a -> m
  , mfolOpAB :: a -> b -> m
  , mfolOpBs :: [b] -> m
  }

{-# INLINE mfol0 #-}
mfol0 :: Monoid m => MFol a b m
mfol0 = MFol
  { mfolVal = mempty
  , mfolOpAA = \_ _ -> mempty
  , mfolOpAB = \_ _ -> mempty
  , mfolOpBs = \_ -> mempty
  }

{-# INLINE appMFol #-}
appMFol :: MFol a b m -> Foo a b -> m
appMFol MFol{..} = \case
  Val -> mfolVal
  OpAA a1 a2 -> mfolOpAA a1 a2
  OpAB a b -> mfolOpAB a b
  OpBs bs -> mfolOpBs bs

data MTFol a b m = MTFol
  { mtfolA :: a -> m
  , mtfolB :: b -> m
  }

{-# INLINE mtfol0 #-}
mtfol0 :: Monoid m => MTFol a b m
mtfol0 = MTFol (\_ -> mempty) (\_ -> mempty)

{-# INLINE fromMTFol #-}
fromMTFol :: Monoid m => MTFol a b m -> MFol a b  m
fromMTFol MTFol{..} = mfol0
  { mfolOpAA = \a1 a2 -> mtfolA a1 <> mtfolA a2
  , mfolOpAB = \a b -> mtfolA a <> mtfolB b
  , mfolOpBs = mconcat . map mtfolB
  }

{-# INLINE appMTFol #-}
appMTFol :: Monoid m => MTFol a b m -> Foo a b -> m
appMTFol = appMFol . fromMTFol

-- MTra, MTTra - Traversable over any data constructor or type parameter ---------------

data MTra a b a' b' m = MTra
  { mtraVal :: forall a' b'. m (Foo a' b')
  , mtraOpAA :: forall b'. a -> a -> m (Foo a' b')
  , mtraOpAB :: a -> b -> m (Foo a' b')
  , mtraOpBs :: forall a'. [b] -> m (Foo a' b')
  }

{-# INLINE mtra0 #-}
mtra0 :: Applicative m => MTra a b a b m
mtra0 = MTra
  { mtraVal = pure Val
  , mtraOpAA = pure .: OpAA
  , mtraOpAB = pure .: OpAB
  , mtraOpBs = pure . OpBs
  }

{-# INLINE appMTra #-}
appMTra :: MTra a b a' b' m -> Foo a b -> m (Foo a' b')
appMTra MTra{..} = \case
  Val -> mtraVal
  OpAA a1 a2 -> mtraOpAA a1 a2
  OpAB a b -> mtraOpAB a b
  OpBs bs -> mtraOpBs bs

data MTTra a b a' b' m = MTTra
  { mttraA :: a -> m a'
  , mttraB :: b -> m b'
  }

{-# INLINE mttra0 #-}
mttra0 :: Applicative m => MTTra a b a b m
mttra0 = MTTra pure pure

{-# INLINE fromMTTra #-}
fromMTTra :: Applicative m => MTTra a b a' b' m -> MTra a b a' b' m
fromMTTra MTTra{..} = mtra0
  { mtraOpAA = \a1 a2 -> OpAA <$> mttraA a1 <*> mttraA a2
  , mtraOpAB = \a b -> OpAB <$> mttraA a <*> mttraB b
  , mtraOpBs = fmap OpBs . traverse mttraB
  }

{-# INLINE appMTTra #-}
appMTTra :: forall m a b a' b'. Applicative m
  => MTTra a b a' b' m -> Foo a b -> m (Foo a' b')
appMTTra = appMTra . fromMTTra

----------------------------------------------------------------------------------------
-- Examples ----------------------------------------------------------------------------

foos :: [Foo String Int]
foos = [OpBs [1, 2, 3, 4], Val, OpAB "abc" 5, OpAA "d" "ef"]

main = do
  -- putStrLn the Strings and print the Ints, printed output:
  -- 1
  -- 2
  -- 3
  -- 4
  -- abc
  -- 5
  -- d
  -- ef
  appMTFol MTFol{mtfolA = putStrLn, mtfolB = print} `foldMap` foos

  -- map Strings to their lengths, output:
  -- [OpBs [1,2,3,4],Val,OpAB 3 5,OpAA 1 2]
  print$ appMTFun mtfun0{mtfunA = length} <$> foos

  -- more finegrained control (per data constructor):
  -- map Strings to their head or last elements, depending on the operand position
  -- output: [OpBs [1,2,3,4],Val,OpAB 'a' 5,OpAA 'd' 'f']
  print$
    appMFun mfun0
      { mfunOpAB = \a b -> OpAB (head a) b
      , mfunOpAA = \a1 a2 -> OpAA (head a1) (last a2)
      }
    <$> foos

  -- negate random Ints
  -- output, e.g.: [OpBs [1,2,-3,-4],Val,OpAB "abc" (-5),OpAA "d" "ef"]
  let randomNegate x = getStdRandom (randomR (False, True)) <&> \case True -> -x; _ -> x
  print =<< (appMTTra mttra0{mttraB = randomNegate} `traverse` foos)
