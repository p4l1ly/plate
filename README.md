# plate
haskell traversals over any type parameter

The code shows my approach to be able to map/fold/traverse over any of the data's type parameter. The approach is somehow related to lenses and multiplate but I think it's still quite different. Is there something like this already implemented in some existing generic library?

We have the following artificial data type:

```
data Foo a b
  = Val
  | OpAA a a
  | OpAB a b
  | OpBs [b]
```

And it can be fmapped/folded/traversed over the data type `a` or `b` or both as follows:

```
foos :: [Foo String Int]
foos = [OpBs [1, 2, 3, 4], Val, OpAB "abc" 5, OpAA "d" "ef"]

main = do
  -- putStrLn the Strings and print the Ints
  appMTFol MTFol{mtfolA = putStrLn, mtfolB = print} `foldMap` foos

  -- map Strings to their lengths
  print$ appMTFun mtfun0{mtfunA = length} <$> foos

  -- more finegrained control (per data constructor):
  -- map Strings to their head or last elements, depending on the operand position
  print$
    appMFun mfun0
      { mfunOpAB = \a b -> OpAB (head a) b
      , mfunOpAA = \a1 a2 -> OpAA (head a1) (last a2)
      }
    <$> foos

  -- negate random Ints
  let randomNegate x = getStdRandom (randomR (False, True)) <&> \case True -> -x; _ -> x
  print =<< (appMTTra mttra0{mttraB = randomNegate} `traverse` foos)
```
