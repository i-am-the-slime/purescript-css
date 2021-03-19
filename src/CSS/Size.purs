module CSS.Size where

import Prelude

import CSS.Common (class Auto)
import CSS.Property (class Val, Value, value)
import CSS.String (class IsString, fromString)

newtype Size :: forall k. k -> Type
newtype Size a = Size Value

derive instance eqSize :: Eq a => Eq (Size a)
derive instance ordSize :: Ord a => Ord (Size a)

instance isStringSize :: IsString (Size a) where
  fromString = Size <<< fromString

instance valSize :: Val (Size a) where
  value (Size v) = v

instance autoSize :: Auto (Size a) where
  auto = fromString "auto"

data Abs
data Rel

-- | Zero size.
nil :: forall a. Size a
nil = Size $ fromString "0"

-- | Unitless size (as recommended for line-height).
unitless ∷ forall a. Number → Size a
unitless = Size <<< value

-- | Size in pixels.
px :: Number -> Size Abs
px i = Size (value i <> fromString "px")

-- | Size in points (1pt = 1/72 of 1in).
pt :: Number -> Size Abs
pt i = Size (value i <> fromString "pt")

-- | Size in ch's.
ch :: Number -> Size Abs
ch i = Size (value i <> fromString "ch")

-- | Size in em's.
em :: Number -> Size Abs
em i = Size (value i <> fromString "em")

-- | Size in ex'es (x-height of the first avaliable font).
ex :: Number -> Size Abs
ex i = Size (value i <> fromString "ex")

-- | SimpleSize in percents.
pct :: Number -> Size Rel
pct i = Size (value i <> fromString "%")

-- | Size in rem's.
rem :: Number -> Size Rel
rem i = Size (value i <> fromString "rem")

-- | Size in vw's (1vw = 1% of viewport width).
vw :: Number -> Size Rel
vw i = Size (value i <> fromString "vw")

-- | Size in vh's (1vh = 1% of viewport height).
vh :: Number -> Size Rel
vh i = Size (value i <> fromString "vh")

-- | Size in vmin's (the smaller of vw or vh).
vmin :: Number -> Size Rel
vmin i = Size (value i <> fromString "vmin")

-- | Size in vmax's (the larger of vw or vh).
vmax :: Number -> Size Rel
vmax i = Size (value i <> fromString "vmax")

sym :: forall a b. (a -> a -> a -> a -> b) -> a -> b
sym f a = f a a a a

data Deg
data Rad

newtype Angle :: forall k. k -> Type
newtype Angle a = Angle Value

derive instance eqAngle :: Eq a => Eq (Angle a)
derive instance ordAngle :: Ord a => Ord (Angle a)

instance valAngle :: Val (Angle a) where
  value (Angle v) = v

-- | Angle in degrees.
deg :: Number -> Angle Deg
deg i = Angle $ (value i <> fromString "deg")

-- | Angle in radians.
rad :: Number -> Angle Rad
rad i = Angle $ (value i <> fromString "rad")

-- | The calc function in CSS, e.g. calc(100% - 20px)
calc :: forall a b. CalcOp -> Size a -> Size b -> Size Calc
calc operation a b = Size calculated
  where
  calculated = fromString "calc(" <> value a <> op <> value b <> fromString ")"
  op = value case operation of 
    CalcAdd -> " + "
    CalcSubtract -> " - "
    CalcMultiply -> " * "
    CalcDivide -> " / "

data Calc = Calc
data CalcOp = 
  CalcAdd | CalcSubtract | CalcMultiply | CalcDivide

calcAdd :: forall a b. Size a -> Size b -> Size Calc
calcAdd = calc CalcAdd

calcSub :: forall a b. Size a -> Size b -> Size Calc
calcSub = calc CalcSubtract

calcMul :: forall a b. Size a -> Size b -> Size Calc
calcMul = calc CalcMultiply

calcDiv :: forall a b. Size a -> Size b -> Size Calc
calcDiv = calc CalcDivide

infixr 3 calcAdd as !+
infixr 3 calcSub as !-
infixr 4 calcMul as !*
infixr 4 calcDiv as !/