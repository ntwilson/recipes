module Recipes.Frontend.ExceptVWidget where

import Frontend.Prelude

import Control.Alternative (class Alternative)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT(..))
import Control.MultiAlternative (class MultiAlternative, defaultOrr)
import Control.Plus (class Plus, empty)
import Control.ShiftMap (class ShiftMap, shiftMap)
import Data.Newtype (class Newtype, wrap)

newtype ExceptVWidget r v a = ExceptVWidget (ExceptV r (Widget v) a)

derive instance Newtype (ExceptVWidget r v a) _
derive instance Functor (ExceptVWidget r v)

instance Apply (ExceptVWidget r v) where
  apply (ExceptVWidget f) (ExceptVWidget x) = 
    ExceptVWidget (apply f x)

instance Bind (ExceptVWidget r v) where
  bind (ExceptVWidget a) f =
    wrap (a >>= (f >>> unwrap))

instance Applicative (ExceptVWidget r v) where
  pure x = ExceptVWidget $ pure x

instance Monad (ExceptVWidget r v)

instance Monoid v => Semigroup (ExceptVWidget r v a) where
  append (ExceptVWidget (ExceptT a)) (ExceptVWidget (ExceptT b)) = do
    ExceptVWidget (ExceptT (a <> b))

instance Monoid v => Monoid (ExceptVWidget r v a) where
  mempty = ExceptVWidget $ lift mempty

instance Monoid v => Alt (ExceptVWidget r v) where
  alt = append

instance LiftWidget v (ExceptVWidget r v) where
  liftWidget w = ExceptVWidget $ lift w

instance Monoid v => Plus (ExceptVWidget r v) where
  empty = ExceptVWidget $ lift empty

instance Monoid v => MultiAlternative (ExceptVWidget r v) where
  orr = defaultOrr

instance Monoid v => ShiftMap (Widget v) (ExceptVWidget r v) where
  shiftMap f (ExceptVWidget m) = ExceptVWidget $ shiftMap f m

instance Monoid v => Alternative (ExceptVWidget r v)

instance Monoid v => MonadEffect (ExceptVWidget r v) where
  liftEffect = ExceptVWidget <<< liftEffect

instance Monoid v => MonadAff (ExceptVWidget r v) where
  liftAff = ExceptVWidget <<< liftAff

instance MonadThrow (Variant r) (ExceptVWidget r v) where
  throwError = ExceptVWidget <<< throwError

runExceptVWidget :: forall r v. ExceptVWidget r v ~> ExceptT (Variant r) (Widget v)
runExceptVWidget = un ExceptVWidget

-- class (Monad i, Monad m) <= LiftExceptV r i m where
--   liftExceptV :: ExceptV r i ~> m

-- instance Monad i => LiftExceptV r i (ExceptV r i) where
--   liftExceptV = identity

-- instance LiftExceptV r (Widget v) (ExceptVWidget r v) where
--   liftExceptV = ExceptVWidget
