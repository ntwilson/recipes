module Recipes.Frontend.MUI where

import Frontend.Prelude hiding (el, elLeaf)

import Concur.Core.DOM as Concur
import Concur.Core.Props (Props(..))
import Control.MultiAlternative (class MultiAlternative)
import Control.ShiftMap (class ShiftMap)
import Data.Array as Array
import Effect.Uncurried (mkEffectFn1)
import Foreign (Foreign, unsafeToForeign)
import Foreign.Object (Object)
import Foreign.Object as Object
import React (ReactClass, ReactElement, unsafeCreateElement, unsafeCreateLeafElement)
import React.SyntheticEvent (SyntheticMouseEvent)
import Simple.JSON (class WriteForeign, writeImpl)
import Type.Row.Homogeneous (class Homogeneous)
import Unsafe.Coerce (unsafeCoerce)
import Untagged.Castable (class Castable)
import Untagged.Castable as Untagged
import Untagged.Union (UndefinedOr, uorToMaybe)

class (ShiftMap (Widget HTML) m, MultiAlternative m, LiftWidget HTML m) <= ReactWidget m
instance (ShiftMap (Widget HTML) m, MultiAlternative m, LiftWidget HTML m) => ReactWidget m

el :: ∀ f props m a. ReactWidget m => Functor f => (f props -> HTML -> ReactElement) -> f (Props props a) -> Array (m a) -> m a
el reactClass = Concur.el' (\props children -> [reactClass props children])

elLeaf :: ∀ f props m a. ReactWidget m => Functor f => (f props -> ReactElement) -> f (Props props a) -> m a
elLeaf reactClass = Concur.elLeaf (\props -> [reactClass props])

recordToObject :: ∀ r a. Homogeneous r a => Record r -> Object a
recordToObject = unsafeCoerce

-- We're taking a generic props type as input, probably a Record of some sort. As far as Concur is concerned though, it needs
-- some Functor of a prop type, and we need to convert that Record to a homogenous Functor. Typical usage would be to convert 
-- the Record to an Object, and just make each value of the Record into a `Props Foreign a`.
-- The view type (`v`) will be HTML, which is just an alias for an Array ReactElement.

-- Handlers are the tricky part of creating a `Props Foreign a`.  The Handler constructor is `Handler :: (a -> Effect Unit) -> p`.  
-- So in other words, you're proving a function that takes an `a -> Effect Unit` as input, and turns it into whatever your prop type is.
-- Here the prop type is Foreign, and `a` is some sort of SyntheticEvent (e.g., SyntheticMouseEvent)
-- , so `Handler :: (SyntheticMouseEvent -> Effect Unit) -> Foreign`
widget :: ∀ f m usedProps p a. ReactWidget m => Functor f =>
  (f p -> HTML -> ReactElement) -> (usedProps -> f (Props p a)) -> usedProps -> Array (m a) -> m a
widget constructor transform usedProps = el constructor (transform usedProps)


widgetLeaf :: ∀ f m usedProps p a. ReactWidget m => Functor f =>
  (f p -> ReactElement) -> (usedProps -> f (Props p a)) -> usedProps -> m a
widgetLeaf constructor transform usedProps = elLeaf constructor (transform usedProps)

makeHandler :: ∀ a event. (event -> a) -> Props Foreign a
makeHandler transformEvent = Handler (\effFn -> unsafeToForeign $ mkEffectFn1 \event -> effFn (transformEvent event))

data CSS 
css :: ∀ r. { | r } -> CSS
css = unsafeCoerce

type ButtonProps a =
  { className :: UndefinedOr String
  , color :: UndefinedOr String
  , disabled :: UndefinedOr Boolean
  , id :: UndefinedOr String
  , onClick :: UndefinedOr (SyntheticMouseEvent -> a)
  , style :: UndefinedOr CSS
  }
buttonProps :: ∀ a r. Castable a (ButtonProps r) => a -> ButtonProps r
buttonProps = Untagged.cast

foreign import rawButton :: ReactClass Void
rawButtonImpl :: Object Foreign -> Array ReactElement -> ReactElement
rawButtonImpl props = unsafeCreateElement (unsafeCoerce rawButton) $ unsafeCoerce props

button :: ∀ a m. ReactWidget m => ButtonProps a -> Array (m a) -> m a
button = widget rawButtonImpl makeProps
  where 
  makeProps props = Object.fromFoldable $ Array.catMaybes
    [ props.className # uorToMaybe <#> \className -> "className" /\ PrimProp (unsafeToForeign className)
    , props.color # uorToMaybe <#> \color -> "color" /\ PrimProp (unsafeToForeign color)
    , props.disabled # uorToMaybe <#> \disabled -> "disabled" /\ PrimProp (unsafeToForeign disabled)
    , props.id # uorToMaybe <#> \id -> "id" /\ PrimProp (unsafeToForeign id)
    , props.onClick # uorToMaybe <#> \onClick -> "onClick" /\ makeHandler onClick
    , props.style # uorToMaybe <#> \style -> "style" /\ PrimProp (unsafeToForeign style)
    ]

data TextSize = TextSmall | TextMedium
instance WriteForeign TextSize where
  writeImpl TextSmall = writeImpl "small"
  writeImpl TextMedium = writeImpl "medium"

type TextFieldProps a =
  { className :: UndefinedOr String
  , classes :: UndefinedOr { root :: String }
  , disabled :: UndefinedOr Boolean
  , id :: UndefinedOr String
  , label :: UndefinedOr String
  , autoFocus :: UndefinedOr Boolean
  , error :: UndefinedOr Boolean
  , value :: UndefinedOr String
  , size :: UndefinedOr TextSize
  , onChange :: UndefinedOr (String -> a)
  }

textFieldProps :: ∀ a r. Castable a (TextFieldProps r) => a -> TextFieldProps r
textFieldProps = Untagged.cast

foreign import rawTextField :: ReactClass Void
rawTextFieldImpl :: Object Foreign -> Array ReactElement -> ReactElement
rawTextFieldImpl props = unsafeCreateElement (unsafeCoerce rawTextField) $ unsafeCoerce props

textField :: ∀ a m. ReactWidget m => TextFieldProps a -> Array (m a) -> m a
textField = widget rawTextFieldImpl makeProps
  where 
  makeProps props = Object.fromFoldable $ Array.catMaybes
    [ props.className # uorToMaybe <#> \className -> "className" /\ PrimProp (unsafeToForeign className)
    , props.classes # uorToMaybe <#> \classes -> "classes" /\ PrimProp (unsafeToForeign classes)
    , props.size # uorToMaybe <#> \size -> "size" /\ PrimProp (writeImpl size)
    , props.disabled # uorToMaybe <#> \disabled -> "disabled" /\ PrimProp (unsafeToForeign disabled)
    , props.id # uorToMaybe <#> \id -> "id" /\ PrimProp (unsafeToForeign id)
    , props.label # uorToMaybe <#> \label -> "label" /\ PrimProp (unsafeToForeign label)
    , props.autoFocus # uorToMaybe <#> \autoFocus -> "autoFocus" /\ PrimProp (unsafeToForeign autoFocus)
    , props.error # uorToMaybe <#> \error -> "error" /\ PrimProp (unsafeToForeign error)
    , props.value # uorToMaybe <#> \value -> "value" /\ PrimProp (unsafeToForeign value)
    , props.onChange # uorToMaybe <#> \onChange -> "onChange" /\ Handler 
      (\effFn -> unsafeToForeign $ mkEffectFn1 
        \event -> effFn $ onChange $ (unsafeCoerce event).target.value
      )
    ]



foreign import rawCheckbox :: ReactClass Void
rawCheckboxImpl :: Object Foreign -> ReactElement
rawCheckboxImpl props = unsafeCreateLeafElement rawCheckbox $ unsafeCoerce props

type CheckboxProps a =
  { icon :: UndefinedOr ReactElement
  , checkedIcon :: UndefinedOr ReactElement
  , classes :: UndefinedOr { root :: String }
  , disabled :: UndefinedOr Boolean
  , checked :: UndefinedOr Boolean
  , id :: UndefinedOr String
  , defaultChecked :: UndefinedOr Boolean
  , inputProps :: UndefinedOr { "aria-label" :: String }
  , onClick :: UndefinedOr (Boolean -> a)
  }

checkboxProps :: ∀ a @r. Castable a (CheckboxProps r) => a -> CheckboxProps r
checkboxProps = Untagged.cast

checkbox :: ∀ a m. ReactWidget m => CheckboxProps a -> m a
checkbox = widgetLeaf rawCheckboxImpl checkProps
  where 
  checkProps props = Object.fromFoldable $ Array.catMaybes
    [ props.disabled # uorToMaybe <#> \disabled -> "disabled" /\ PrimProp (unsafeToForeign disabled)
    , props.icon # uorToMaybe <#> \icon -> "icon" /\ PrimProp (unsafeToForeign icon)
    , props.checkedIcon # uorToMaybe <#> \checkedIcon -> "checkedIcon" /\ PrimProp (unsafeToForeign checkedIcon)
    , props.classes # uorToMaybe <#> \classes -> "classes" /\ PrimProp (unsafeToForeign classes)
    , props.checked # uorToMaybe <#> \checked -> "checked" /\ PrimProp (unsafeToForeign checked)
    , props.defaultChecked # uorToMaybe <#> \defaultChecked -> "defaultChecked" /\ PrimProp (unsafeToForeign defaultChecked)
    , props.id # uorToMaybe <#> \id -> "id" /\ PrimProp (unsafeToForeign id)
    , props.inputProps # uorToMaybe <#> \inputProps -> "inputProps" /\ PrimProp (unsafeToForeign inputProps)
    , props.onClick # uorToMaybe <#> \onClick -> "onClick" /\ Handler 
      (\effFn -> unsafeToForeign $ mkEffectFn1 
        \event -> effFn $ onClick $ (unsafeCoerce event).target.checked
      ) 
    ]

foreign import rawFormGroup :: ReactClass Void
rawFormGroupImpl :: Array ReactElement -> ReactElement
rawFormGroupImpl = unsafeCreateElement (unsafeCoerce rawFormGroup) {}

formGroup :: ∀ a m. ReactWidget m => Array (m a) -> m a 
formGroup = Concur.el' (\_props children -> [rawFormGroupImpl children]) []


foreign import rawFab :: ReactClass Void
rawFabImpl :: Object Foreign -> Array ReactElement -> ReactElement
rawFabImpl props = unsafeCreateElement (unsafeCoerce rawFab) $ unsafeCoerce props

data FabSize = FabSmall | FabMedium | FabLarge
instance WriteForeign FabSize where
  writeImpl FabSmall = writeImpl "small"
  writeImpl FabMedium = writeImpl "medium"
  writeImpl FabLarge = writeImpl "large"

data FabVariant = Circular | Extended
instance WriteForeign FabVariant where
  writeImpl Circular = writeImpl "circular"
  writeImpl Extended = writeImpl "extended"

data FabColor = Default | Error | Info | Inherit | Primary | Secondary | Success | Warning
instance WriteForeign FabColor where
  writeImpl Default = writeImpl "default"
  writeImpl Error = writeImpl "error"
  writeImpl Info = writeImpl "info"
  writeImpl Inherit = writeImpl "inherit"
  writeImpl Primary = writeImpl "primary"
  writeImpl Secondary = writeImpl "secondary"
  writeImpl Success = writeImpl "success"
  writeImpl Warning = writeImpl "warning"

type FabProps :: Type -> Type
type FabProps a =
  { size :: UndefinedOr FabSize
  , style :: UndefinedOr CSS
  , variant :: UndefinedOr FabVariant
  , disabled :: UndefinedOr Boolean
  , color :: UndefinedOr FabColor
  , classes :: UndefinedOr { root :: String }
  , onClick :: UndefinedOr (SyntheticMouseEvent -> a)
  }

fabProps :: ∀ a @r. Castable a (FabProps r) => a -> FabProps r
fabProps = Untagged.cast

-- widget :: ∀ f m usedProps p a. ReactWidget m => Functor f =>
--   (f p -> HTML -> ReactElement) -> (usedProps -> f (Props p a)) -> usedProps -> Array (m a) -> m a
floatingActionButton :: ∀ a m. ReactWidget m => FabProps a -> Array (m a) -> m a 
floatingActionButton = widget (unsafeCoerce rawFabImpl) makeProps
  where

  makeProps :: FabProps a -> Object (Props Foreign a)
  makeProps props = Object.fromFoldable $ Array.catMaybes
    [ props.size # uorToMaybe <#> \size -> "size" /\ PrimProp (writeImpl size)
    , props.style # uorToMaybe <#> \style -> "style" /\ PrimProp (unsafeToForeign style)
    , props.variant # uorToMaybe <#> \variant -> "variant" /\ PrimProp (writeImpl variant)
    , props.disabled # uorToMaybe <#> \disabled -> "disabled" /\ PrimProp (writeImpl disabled)
    , props.color # uorToMaybe <#> \color -> "color" /\ PrimProp (writeImpl color)
    , props.classes # uorToMaybe <#> \classes -> "classes" /\ PrimProp (writeImpl classes)
    , props.onClick # uorToMaybe <#> \onClick -> "onClick" /\ makeHandler onClick
    ]

foreign import rawAddIcon :: ReactClass Void

addIcon :: ∀ a m. ReactWidget m => m a  
addIcon = elLeaf (unsafeCreateLeafElement (unsafeCoerce rawAddIcon)) []
