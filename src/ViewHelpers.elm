module ViewHelpers exposing (..)

import Html exposing (div)
import Html.Attributes exposing (class)


row attrs =
    div (class "df-row" :: attrs)


col attrs =
    div (class "df-col" :: attrs)


fg1 =
    class "fg1"


p5 =
    class "p5"


ph5 =
    class "ph5"


ph10 =
    class "ph10"


pv5 =
    class "pv5"


sp10 =
    class "sp10"


sp5 =
    class "sp5"


el attrs child =
    row attrs [ child ]
