module Shared.Prelude (module Exports, APPLY, type ($), doubleMap, revDoubleMap, mapCompose, revMapCompose, (<$$>), (<##>), (<$<), (>#>), caseMaybe, STRING_ERROR, stringError, ERROR, err, JSON_DECODE_ERROR, jsonDecodeError, inj) where

import Prelude

import Control.Monad.Error.Class (catchError, throwError, try) as Exports
import Control.Monad.Except.Checked (ExceptV, safe, handleError, handleErrors) as Exports
import Control.Monad.Trans.Class (lift) as Exports
import Data.Argonaut.Core (Json) as Exports
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap) as Exports
import Data.Codec (codec') as Exports
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError(..), printJsonDecodeError, decode, encode) as Exports
import Data.Either (Either(..), choose, either, fromLeft, fromRight, hush, isLeft, isRight, note, note') as Exports
import Data.Function (on) as Exports
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn1, runFn2, runFn3) as Exports
import Data.Interpolate (i) as Exports
import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe', isJust, isNothing, optional) as Exports
import Data.Maybe (maybe)
import Data.Newtype (over, un, under, unwrap) as Exports
import Data.Traversable (class Foldable, class Traversable, Accum, all, and, any, elem, find, fold, foldMap, foldMapDefaultL, foldMapDefaultR, foldl, foldlDefault, foldr, foldrDefault, for, for_, intercalate, mapAccumL, mapAccumR, maximum, maximumBy, minimum, minimumBy, notElem, oneOf, or, scanl, scanr, sequence, sequenceDefault, sequence_, sum, traverse, traverseDefault, traverse_) as Exports
import Data.Tuple.Nested ((/\), type (/\)) as Exports
import Data.Variant (Variant)
import Data.Variant (Variant) as Exports
import Data.Variant as Variant
import Effect (Effect, forE, foreachE, untilE, whileE) as Exports
import Effect.Aff (Aff, BracketConditions, Canceler(..), Error, Fiber, Milliseconds(..), ParAff, attempt, bracket, error, forkAff, launchAff, launchAff_, makeAff, parallel, runAff, runAff_, sequential) as Exports
import Effect.Aff.Class (class MonadAff, liftAff) as Exports
import Effect.Class (class MonadEffect, liftEffect) as Exports
import Effect.Class.Console (log, logShow) as Exports
import Effect.Exception (message) as Exports
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3) as Exports
import Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class Category, class CommutativeRing, class Discard, class DivisionRing, class Eq, class EuclideanRing, class Field, class Functor, class HeytingAlgebra, class Monad, class Monoid, class Ord, class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show, type (~>), Ordering(..), Unit, Void, absurd, add, ap, append, apply, between, bind, bottom, clamp, compare, comparing, compose, conj, const, degree, discard, disj, div, eq, flap, flip, gcd, identity, ifM, join, lcm, liftA1, liftM1, map, max, mempty, min, mod, mul, negate, not, notEq, one, otherwise, pure, recip, show, sub, top, unit, unless, unlessM, void, when, whenM, zero, (#), ($), ($>), (&&), (*), (*>), (+), (-), (/), (/=), (<), (<#>), (<$), (<$>), (<*), (<*>), (<<<), (<=), (<=<), (<>), (<@>), (=<<), (==), (>), (>=), (>=>), (>>=), (>>>), (||)) as Exports
import Prim.Row (class Cons)
import Recipes.ErrorHandling (launchAffWithHandler) as Exports
import Type.Prelude (class IsSymbol, Proxy(..))
import Type.Row (type (+)) as Exports
import Unsafe.Coerce (unsafeCoerce) as Exports

type APPLY :: forall k1 k2. (k1 -> k2) -> k1 -> k2
type APPLY a b = a b
infixr 5 type APPLY as $

doubleMap :: ∀ fOuter fInner a b. Functor fOuter => Functor fInner => (a -> b) -> fOuter (fInner a) -> fOuter (fInner b)
doubleMap = map <<< map 

revDoubleMap :: ∀ fOuter fInner a b. Functor fOuter => Functor fInner => fOuter (fInner a) -> (a -> b) -> fOuter (fInner b)
revDoubleMap = flip doubleMap

mapCompose :: ∀ f a b c. Functor f => (b -> c) -> (a -> f b) -> a -> f c
mapCompose f g = map f <<< g

revMapCompose :: ∀ f a b c. Functor f => (a -> f b) -> (b -> c) -> a -> f c
revMapCompose = flip mapCompose

caseMaybe :: ∀ a b. { just :: a -> b, nothing :: b } -> Exports.Maybe a -> b
caseMaybe { just, nothing } = maybe nothing just


infixr 4 doubleMap as <$$>
infixl 1 revDoubleMap as <##>
infixr 4 mapCompose as <$<
infixl 1 revMapCompose as >#>

inj :: ∀ @sym a r1 r2. Cons sym a r1 r2 => IsSymbol sym => a -> Variant r2
inj = Variant.inj (Proxy @sym)

type STRING_ERROR r = (stringError :: String | r)
stringError :: ∀ r. String -> Exports.Variant (STRING_ERROR r)
stringError = inj @"stringError"

type ERROR r = (error :: Exports.Error | r)
err :: ∀ r. Exports.Error -> Exports.Variant (ERROR r)
err = inj @"error"

type JSON_DECODE_ERROR r = (jsonDecodeError :: Exports.JsonDecodeError | r)
jsonDecodeError :: ∀ r. Exports.JsonDecodeError -> Exports.Variant (JSON_DECODE_ERROR r)
jsonDecodeError = inj @"jsonDecodeError"
