module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

data M a = M a | MPure

{--
```
 App
  (Var Data.Functor.Functor)
  (Abs f
    (Abs v
      (Case
        caseExpressions
          [ (Var f)
          , (Var v)
          ]
        caseAlternatives
          [ (Binders
              [ VarBinder f1
              , ConstructorBinder Main.M Main.M
                  [ VarBinder a ]
              ]
             Expression
              (App
                (Var Main.M)
                (App
                  (Var f1)
                  (Var a))))
          , (Binders
              [ VarBinder f1
              , ConstructorBinder Main.M Main.MPure]
             Expression
              (Var Main.MPure))
          ])))
 ```
--}
instance functorM :: Functor M where
  map f (M a) = M (f a)
  map f MPure = MPure

{--
 instance applyM :: Apply M where
   apply (M xy) (M x) = M (xy x)
   apply _ _ = MPure
--}

{--
 ```
 App
  (App
    (Var Control.Apply.Apply IsTypeClassConstructorApp Control.Apply.Apply)
    (Abs __unused
      (Var Main.functorM)))
  (Abs v
    (Abs v1
      (Case
        caseExpressions
          [ Var v
          , Var v1
          ]
        caseAlternatives
          [ (Binders
              [ ConstructorBinder Main.M Main.M
                  [ VarBinder xy ]
              , VarBinder mx
              ]
             Expression
              App
                (App
                  (App        type (ForAll f) (ForAll a (ConstrainedType ...))
                    (Var Data.Functor.map)
                    (Var Main.functorM))
                  (Var xy))
                (Var mx)
            )
          , (Binders
              [ ConstructorBinder Main.M Main.MPure
                  []
              , NullBinder
              ]
             Expression
              (Var Main.MPure))
          ]
        )))
 ```
  --}
instance applyM :: Apply M where
  apply (M xy) mx = map xy mx
  apply MPure _ = MPure

{--
 ```
  App
   (App
     (Var Control.Applicative.Applicative  (:meta IsTypeClassConstructorApp Control.Applicative.Applicative))
     (Abs __unused
       (Main.applyM)))
   (Abs v 
     (Var Main.MPure))
 ```
--}
instance applicativeM :: Applicative M where
  pure _ = MPure

instance bindM :: Bind M where
  bind ma f = MPure

instance monadM :: Monad M

{--
```
Abs dictMonad
  (App (:type ConstrainedType Control.Monad.Monad (TypeApp m Int))
    (App
      (Var Control.Applicative.pure (:type ForAll a (ConstrainedType Control.Applicative.Applicative (TypeApp ..))))
      (App
        (Accessor Applicative0 (Var dictMonad))
        (Var Prim.undefined)))
    (Literal 0)
```
--}
m :: forall m. Monad m => m Int
m = pure 0

{--
  - m :: M Int
  - m = apply (M (\x -> x)) (M 1)
  --}

{--
```
App                 there is no meta or useful type information in this AST
  (Var Main.m)
  (Var Main.monadM)
```
--}
n :: M Int
n = m

{--

```
:type ConstrainedType Control.Monad.Monad
    [ Skolem m
    , TypeApp (Skolem m) (TypeConstructor Prim.Int)] 
Abs dictMonad
  App
    (App
      (App
        (Var Control.Apply.apply (:type ForAll f. (Forall b (Forall a (ConstrainedType Control.Apply.Apply [TypeVar f])))))
        (App
          (Accessor Apply0
            (App
              (Accessor Bind1 (Var Bind dictMonad))
              (Var Prim.undefined))
          (Var Prim.undefined)))
      (App
        (Accessor Apply0
          (App
            (Accessor Bind1 (Var dictMonad))
            (Var Prim.undefined)))
        (Var Prim.undefined)))
    (App
      (App
        (Var Control.Applicative.pure (:type ForAll f (ForAll a (ConstrainedType Control.Applicative.Applicative [TypeVar f])) ))
        (App
          (Accessor Applicative0 (Var dictMonad))
          (Var Prim.undefined)))
      (App
        (App
          (Var Control.Applicative.pure (:type (ForAll f a (ConstrainedType Control.Applicative.Applicative [TypeVar f]))))
          (App
            (Accessor Applicative0 (Var dictMonad))
            (Var Prim.undefined)))
        (Abs a
          (Var a))))
```
I need to deduce from this:
  dictMonad :: Control.Monad.Monad dict
  dictMonad.Apply0.apply + dictMonad.Apply0 is Control.Monad.Apply.Apply dict
  dictMonad.Applicative0.pure + dictMonad.Applicative0 is Control.Applicative.Applicative dict

  dependencies of this function
    Control.Monad.Monad.Apply0.apply
    Control.Monad.Monad.Applicative0.pure
--}
o :: forall m. Monad m => m Int
o = apply (pure (\a -> a)) (pure 0)

p :: M Int
p = o

{--
```
::  ConstrainedType (Data.Show.Show) (ConstrainedType (Control.Monad.Monad (type)
Abs dictShow
   (::  ConstrainedType (Control.Monad.Monad) (type)
    Abs dictMonad
     (Abs a
       (App
         (App
           (Var Control.Applicative.pure)
           (App
             (Accessor Applicative0 (Var dictMonad))
             (Var Prim.undefined)))
         (App
           (App
             (Var Data.Show.show)
             (Var dictShow))
           (Var a)))))
```
--}
x :: forall a m. Show a => Monad m => a -> m String
x a = pure (show a)

{--
```
:: ConstrainedType (Control.Monad.Monad) (type)
Abs dictMonad
   (App
     (App (Var Main.x) (Data.Show.showInt)
     (Var dictMonad))
```
--}
y :: forall m. Monad m => Int -> m String
y = x

{--
```
:: ConstrainedType (Data.Show.Show) (type)
Abs dictShow
 (App
   (App
     (Var Main.x)
     (Var dictShow))
   (Var Main.monadM))
```
--}
z :: forall a. Show a => a -> M String
z = x

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = log "Hello sailor!" *> pure unit
