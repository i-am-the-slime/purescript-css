module CSS.Variable (declare, reference, CSSVariable, variable) where

import Prelude

import CSS.Property (class Val, value)
import CSS.String (class IsString, fromString)
import CSS.Stylesheet (CSS, key)
import Data.Maybe (fromMaybe)
import Data.String as S

declare :: forall type_. Val type_ => CSSVariable type_ -> CSS
declare var = key (fromString (renderCSSVariableName var)) (value (extractValue var))
  where
  extractValue (CSSVariable _ value) = value

reference :: forall type_. IsString type_ => CSSVariable type_ -> type_
reference v = fromString ("var(" <> renderCSSVariableName v <> ")")

data CSSVariable type_ = CSSVariable String type_

variable :: forall type_. Val type_ => String -> type_ -> CSSVariable type_
variable name = CSSVariable sanitised
  where
  sanitisedMaybe = S.stripPrefix (S.Pattern "--") name
  sanitised = fromMaybe name sanitisedMaybe

renderCSSVariableName :: forall type_. CSSVariable type_ -> String
renderCSSVariableName (CSSVariable name _) = "--" <> name

derive instance eqCSSVariable  :: Eq type_ => Eq (CSSVariable type_)
derive instance ordCSSVariable :: Ord type_ => Ord (CSSVariable type_)

instance valCSSVariable :: (Val type_) => Val (CSSVariable type_) where
  value v = fromString $ renderCSSVariableName v
