module CSS.Border where

import Prelude

import Data.Tuple.Nested (tuple3, tuple4)

import CSS.Color (Color)
import CSS.Property (class Val, Value)
import CSS.Size (Size)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)

newtype Stroke = Stroke Value

derive instance eqStroke :: Eq Stroke
derive instance ordStroke :: Ord Stroke

instance valStroke :: Val Stroke where
  value (Stroke v) = v

solid :: Stroke
solid = Stroke $ fromString "solid"

dotted :: Stroke
dotted = Stroke $ fromString "dotted"

dashed :: Stroke
dashed = Stroke $ fromString "dashed"

double :: Stroke
double = Stroke $ fromString "double"

wavy :: Stroke
wavy = Stroke $ fromString "wavy"

groove :: Stroke
groove = Stroke $ fromString "groove"

ridge :: Stroke
ridge = Stroke $ fromString "ridge"

inset :: Stroke
inset = Stroke $ fromString "inset"

outset :: Stroke
outset = Stroke $ fromString "outset"

border :: forall a. Stroke -> Size a -> Color -> CSS
border a b c = key (fromString "border") $ tuple3 a b c

borderTop :: forall a. Stroke -> Size a -> Color -> CSS
borderTop a b c = key (fromString "border-top") $ tuple3 a b c

borderBottom :: forall a. Stroke -> Size a -> Color -> CSS
borderBottom a b c = key (fromString "border-bottom") $ tuple3 a b c

borderLeft :: forall a. Stroke -> Size a -> Color -> CSS
borderLeft a b c = key (fromString "border-left") $ tuple3 a b c

borderRight :: forall a. Stroke -> Size a -> Color -> CSS
borderRight a b c = key (fromString "border-right") $ tuple3 a b c

borderColor :: Color -> CSS
borderColor = key $ fromString "border-color"

borderRadius :: forall a. Size a -> Size a -> Size a -> Size a -> CSS
borderRadius a b c d = key (fromString "border-radius") (tuple4 a b c d)

borderSpacing :: forall a. Size a -> CSS
borderSpacing = key $ fromString "border-spacing"
