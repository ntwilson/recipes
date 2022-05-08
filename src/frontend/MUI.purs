module Recipes.Frontend.MUI where

import Frontend.Prelude

import Concur.Core (class LiftWidget)
import Concur.Core.DOM as Concur
import Concur.Core.Props (Props(..))
import Control.MultiAlternative (class MultiAlternative)
import Control.ShiftMap (class ShiftMap)
import Effect.Uncurried (mkEffectFn1)
import Foreign (Foreign, unsafeToForeign)
import Foreign.Object (Object)
import Option as Option
import React (ReactClass, ReactElement, unsafeCreateElement, unsafeCreateLeafElement)
import React.SyntheticEvent (SyntheticMouseEvent)
import Simple.JSON (class WriteForeign, writeImpl)
import Type.Row.Homogeneous (class Homogeneous)
import Unsafe.Coerce (unsafeCoerce)

class (ShiftMap (Widget HTML) m, MultiAlternative m, LiftWidget HTML m) <= ReactWidget m
instance (ShiftMap (Widget HTML) m, MultiAlternative m, LiftWidget HTML m) => ReactWidget m

el :: ∀ f props m a. ReactWidget m => Functor f => (f props -> HTML -> ReactElement) -> f (Props props a) -> Array (m a) -> m a
el reactClass = Concur.el' (\props children -> [reactClass props children])

elLeaf :: ∀ f props m a. ReactWidget m => Functor f => (f props -> ReactElement) -> f (Props props a) -> m a
elLeaf reactClass = Concur.elLeaf (\props -> [reactClass props])

optionToObject :: ∀ r a. Homogeneous r a => Option.Option r -> Object a
optionToObject = unsafeCoerce

-- We're taking a generic props type as input, probably an Option of some sort. As far as Concur is concerned though, it needs
-- some Functor of a prop type, and we need to convert that Option to a homogenous Functor. Typical usage would be to convert 
-- the Option to an Object, and just make each value of the Option into a `Props Foreign a`.
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

type ButtonProps a = Option.Option
  ( className :: String
  , color :: String
  , disabled :: Boolean
  , id :: String
  , onClick :: SyntheticMouseEvent -> a
  , style :: CSS
  )

foreign import rawButton :: ReactClass Void
rawButtonImpl :: Object Foreign -> Array ReactElement -> ReactElement
rawButtonImpl props = unsafeCreateElement (unsafeCoerce rawButton) $ unsafeCoerce props

button :: ∀ a m. ReactWidget m => ButtonProps a -> Array (m a) -> m a
button = widget rawButtonImpl makeProps
  where 
  makeProps props = optionToObject $ flip Option.modify' props
    { className: PrimProp <<< unsafeToForeign
    , color: PrimProp <<< unsafeToForeign
    , disabled: PrimProp <<< unsafeToForeign
    , id: PrimProp <<< unsafeToForeign
    , onClick: \fn -> makeHandler fn
    , style: PrimProp <<< unsafeToForeign
    }

data TextSize = TextSmall | TextMedium
instance WriteForeign TextSize where
  writeImpl TextSmall = writeImpl "small"
  writeImpl TextMedium = writeImpl "medium"

type TextFieldProps a = Option.Option
  ( className :: String
  , classes :: { root :: String }
  , disabled :: Boolean
  , id :: String
  , label :: String
  , autoFocus :: Boolean
  , error :: Boolean
  , value :: String
  , size :: TextSize
  , onChange :: String -> a
  )

foreign import rawTextField :: ReactClass Void
rawTextFieldImpl :: Object Foreign -> Array ReactElement -> ReactElement
rawTextFieldImpl props = unsafeCreateElement (unsafeCoerce rawTextField) $ unsafeCoerce props

textField :: ∀ a m. ReactWidget m => TextFieldProps a -> Array (m a) -> m a
textField = widget rawTextFieldImpl makeProps
  where 
  makeProps props = optionToObject $ flip Option.modify' props
    { className: PrimProp <<< unsafeToForeign
    , classes: PrimProp <<< unsafeToForeign
    , size: PrimProp <<< writeImpl
    , disabled: PrimProp <<< unsafeToForeign
    , id: PrimProp <<< unsafeToForeign
    , label: PrimProp <<< unsafeToForeign
    , autoFocus: PrimProp <<< unsafeToForeign
    , error: PrimProp <<< unsafeToForeign
    , value: PrimProp <<< unsafeToForeign
    , onChange: \transformValue -> Handler 
      (\effFn -> unsafeToForeign $ mkEffectFn1 
        \event -> effFn $ transformValue $ (unsafeCoerce event).target.value
      )
    }



foreign import rawCheckbox :: ReactClass Void
rawCheckboxImpl :: Object Foreign -> ReactElement
rawCheckboxImpl props = unsafeCreateLeafElement rawCheckbox $ unsafeCoerce props

type CheckboxProps a = Option.Option
  ( icon :: ReactElement
  , checkedIcon :: ReactElement
  , disabled :: Boolean
  , checked :: Boolean
  , id :: String
  , defaultChecked :: Boolean
  , inputProps :: { "aria-label" :: String }
  , onClick :: Boolean -> a
  )

checkbox :: ∀ a m. ReactWidget m => CheckboxProps a -> m a
checkbox = widgetLeaf rawCheckboxImpl checkProps
  where 
  checkProps = optionToObject <<< Option.modify' 
    { disabled: PrimProp <<< unsafeToForeign
    , icon: PrimProp <<< unsafeToForeign
    , checkedIcon: PrimProp <<< unsafeToForeign
    , checked: PrimProp <<< unsafeToForeign
    , defaultChecked: PrimProp <<< unsafeToForeign
    , id: PrimProp <<< unsafeToForeign
    , inputProps: PrimProp <<< unsafeToForeign
    , onClick: 
      \transformValue -> Handler 
        (\effFn -> unsafeToForeign $ mkEffectFn1 
          \event -> effFn $ transformValue $ (unsafeCoerce event).target.checked
        ) 
    }



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
type FabProps a = Option.Option 
  ( size :: FabSize
  , style :: CSS
  , variant :: FabVariant
  , disabled :: Boolean
  , color :: FabColor
  , classes :: { root :: String }
  , onClick :: SyntheticMouseEvent -> a
  )

floatingActionButton :: ∀ a m. ReactWidget m => FabProps a -> Array (m a) -> m a 
floatingActionButton = widget (unsafeCoerce rawFabImpl) makeProps
  where
  makeProps = optionToObject <<< Option.modify'
    { size: PrimProp <<< writeImpl
    , style: PrimProp <<< unsafeToForeign
    , variant: PrimProp <<< writeImpl
    , disabled: PrimProp <<< writeImpl
    , color: PrimProp <<< writeImpl
    , classes: PrimProp <<< writeImpl
    , onClick: \fn -> makeHandler fn
    }

foreign import rawAddIcon :: ReactClass Void

addIcon :: ∀ a m. ReactWidget m => m a  
addIcon = elLeaf (unsafeCreateLeafElement (unsafeCoerce rawAddIcon)) []
