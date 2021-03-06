module Shared.Prelude (module Exports, APPLY, type ($), doubleMap, revDoubleMap, (<$$>), (<##>), equating) where

import Prelude

import Control.Monad.Error.Class (catchError, throwError, try) as Exports
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson) as Exports
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap) as Exports
import Data.Either (Either(..), choose, either, fromLeft, fromRight, hush, isLeft, isRight, note, note') as Exports
import Data.Interpolate (i) as Exports
import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe', optional) as Exports
import Data.Newtype (over, un, under, unwrap) as Exports
import Data.Traversable (class Foldable, class Traversable, Accum, all, and, any, elem, find, fold, foldMap, foldMapDefaultL, foldMapDefaultR, foldl, foldlDefault, foldr, foldrDefault, for, for_, intercalate, mapAccumL, mapAccumR, maximum, maximumBy, minimum, minimumBy, notElem, oneOf, or, scanl, scanr, sequence, sequenceDefault, sequence_, sum, traverse, traverseDefault, traverse_) as Exports
import Data.Tuple.Nested ((/\), type (/\)) as Exports
import Effect (Effect, forE, foreachE, untilE, whileE) as Exports
import Effect.Aff (Aff, BracketConditions, Canceler(..), Error, Fiber, Milliseconds(..), ParAff, attempt, bracket, error, forkAff, launchAff, launchAff_, makeAff, parallel, runAff, runAff_, sequential) as Exports
import Effect.Aff.Class (class MonadAff, liftAff) as Exports
import Effect.Class (class MonadEffect, liftEffect) as Exports
import Effect.Class.Console (log, logShow) as Exports
import Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class Category, class CommutativeRing, class Discard, class DivisionRing, class Eq, class EuclideanRing, class Field, class Functor, class HeytingAlgebra, class Monad, class Monoid, class Ord, class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show, type (~>), Ordering(..), Unit, Void, absurd, add, ap, append, apply, between, bind, bottom, clamp, compare, comparing, compose, conj, const, degree, discard, disj, div, eq, flap, flip, gcd, identity, ifM, join, lcm, liftA1, liftM1, map, max, mempty, min, mod, mul, negate, not, notEq, one, otherwise, pure, recip, show, sub, top, unit, unless, unlessM, void, when, whenM, zero, (#), ($), ($>), (&&), (*), (*>), (+), (-), (/), (/=), (<), (<#>), (<$), (<$>), (<*), (<*>), (<<<), (<=), (<=<), (<>), (<@>), (=<<), (==), (>), (>=), (>=>), (>>=), (>>>), (||)) as Exports
import Recipes.ErrorHandling (class Throwable, class Throws, fromMessage, fromThrowable, launchAffWithHandler, liftError, liftErrorVia, throw) as Exports

type APPLY :: forall k1 k2. (k1 -> k2) -> k1 -> k2
type APPLY a b = a b
infixr 5 type APPLY as $

doubleMap :: ??? fOuter fInner a b. Functor fOuter => Functor fInner => (a -> b) -> fOuter (fInner a) -> fOuter (fInner b)
doubleMap = map <<< map 

revDoubleMap :: ??? fOuter fInner a b. Functor fOuter => Functor fInner => fOuter (fInner a) -> (a -> b) -> fOuter (fInner b)
revDoubleMap = flip doubleMap

equating :: ??? a b. Eq b => (a -> b) -> a -> a -> Boolean
equating projection a b = projection a == projection b

infixr 4 doubleMap as <$$>
infixl 1 revDoubleMap as <##>
