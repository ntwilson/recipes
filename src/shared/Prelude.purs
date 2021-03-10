module Shared.Prelude (module Exports, Dolla, type ($)) where

import Data.Either (Either(..), choose, either, fromLeft, fromRight, hush, isLeft, isRight, note, note') as Exports
import Data.Interpolate (i) as Exports
import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe', optional) as Exports
import Effect (Effect, forE, foreachE, untilE, whileE) as Exports
import Effect.Aff (Aff, BracketConditions, Canceler(..), Error, Fiber, Milliseconds(..), ParAff, attempt, bracket, error, forkAff, launchAff, launchAff_, makeAff, parallel, runAff, runAff_, sequential, try) as Exports
import Effect.Class (liftEffect) as Exports
import Effect.Class.Console (log, logShow) as Exports
import Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class Category, class CommutativeRing, class Discard, class DivisionRing, class Eq, class EuclideanRing, class Field, class Functor, class HeytingAlgebra, class Monad, class Monoid, class Ord, class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show, type (~>), Ordering(..), Unit, Void, absurd, add, ap, append, apply, between, bind, bottom, clamp, compare, comparing, compose, conj, const, degree, discard, disj, div, eq, flap, flip, gcd, identity, ifM, join, lcm, liftA1, liftM1, map, max, mempty, min, mod, mul, negate, not, notEq, one, otherwise, pure, recip, show, sub, top, unit, unless, unlessM, void, when, whenM, zero, (#), ($), ($>), (&&), (*), (*>), (+), (-), (/), (/=), (<), (<#>), (<$), (<$>), (<*), (<*>), (<<<), (<=), (<=<), (<>), (<@>), (=<<), (==), (>), (>=), (>=>), (>>=), (>>>), (||)) as Exports

type Dolla a b = a b
infixr 5 type Dolla as $
