module Bootstrap.Form.Textarea
    exposing
        ( textarea
        , id
        , rows
        , value
        , defaultValue
        , disabled
        , onInput
        , attrs
        , success
        , danger
        , Option
        )

{-| This module allows you to create textarea elements.


# Creating
@docs textarea


# Options
@docs id, rows, value, defaultValue, disabled, onInput, attrs, Option

# Validation
@docs success, danger


-}

import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Bootstrap.Form.FormInternal as FormInternal


{-| Opaque type representing a composable input
-}
type Textarea msg
    = Textarea { options : List (Option msg) }


{-| Opaque type representing legal textarea configuration options
-}
type Option msg
    = Id String
    | Rows Int
    | Disabled
    | Value String
    | DefaultValue String
    | OnInput (String -> msg)
    | Validation FormInternal.Validation
    | Attrs (List (Html.Attribute msg))


type alias Options msg =
    { id : Maybe String
    , rows : Maybe Int
    , disabled : Bool
    , value : Maybe String
    , defaultValue : Maybe String
    , onInput : Maybe (String -> msg)
    , validation : Maybe FormInternal.Validation
    , attributes : List (Html.Attribute msg)
    }


{-| Create a textarea element.

    Textarea.textarea
        [ Textarea.id "myarea"
        , Textarea.rows 4
        , Textarea.onInput MyTextareaMsg
        ]

-}
textarea : List (Option msg) -> Html.Html msg
textarea =
    view << create


{-| Options/shorthand for setting the id of a textarea
-}
id : String -> Option msg
id i =
    Id i


{-| Option/shorthand to set the rows attribute of a textarea
-}
rows : Int -> Option msg
rows r =
    Rows r


{-| Use this function to handle any Html.Attribute option you wish for your textarea
-}
attrs : List (Html.Attribute msg) -> Option msg
attrs attributes =
    Attrs attributes


{-| Shorthand for setting the value attribute of a textarea
-}
value : String -> Option msg
value val =
    Value val


{-| Shorthand for setting the defaultValue attribute of a textarea
-}
defaultValue : String -> Option msg
defaultValue val =
    DefaultValue val


{-| Shorthand for assigning an onInput handler for a textarea
-}
onInput : (String -> msg) -> Option msg
onInput toMsg =
    OnInput toMsg


{-| Shorthand for setting the disabled attribute of a textarea
-}
disabled : Option msg
disabled =
    Disabled


{-| Option to add a success marker icon for your textarea.
-}
success : Option msg
success =
    Validation FormInternal.Success


{-| Option to add a danger marker icon for your textarea.
-}
danger : Option msg
danger =
    Validation FormInternal.Danger


view : Textarea msg -> Html.Html msg
view (Textarea { options }) =
    Html.textarea
        (toAttributes options)
        []


create : List (Option msg) -> Textarea msg
create options =
    Textarea { options = options }


toAttributes : List (Option msg) -> List (Html.Attribute msg)
toAttributes modifiers =
    let
        options =
            List.foldl applyModifier defaultOptions modifiers
    in
        [ Attributes.class "form-control"
        , Attributes.disabled options.disabled
        ]
            ++ ([ Maybe.map Attributes.id options.id
                , Maybe.map Attributes.rows options.rows
                , Maybe.map Attributes.value options.value
                , Maybe.map Attributes.value options.defaultValue
                , Maybe.map Events.onInput options.onInput
                , Maybe.map validationAttribute options.validation
                ]
                    |> List.filterMap identity
               )
            ++ options.attributes


defaultOptions : Options msg
defaultOptions =
    { id = Nothing
    , rows = Nothing
    , disabled = False
    , value = Nothing
    , defaultValue = Nothing
    , onInput = Nothing
    , validation = Nothing
    , attributes = []
    }


applyModifier : Option msg -> Options msg -> Options msg
applyModifier modifier options =
    case modifier of
        Id idMod ->
            { options | id = Just idMod }

        Rows rowsMod ->
            { options | rows = Just rowsMod }

        Disabled ->
            { options | disabled = True }

        Value valueMod ->
            { options | value = Just valueMod }

        DefaultValue valueMod ->
            { options | defaultValue = Just valueMod }

        OnInput onInputMod ->
            { options | onInput = Just onInputMod }

        Validation validation ->
            { options | validation = Just validation }

        Attrs attrsMod ->
            { options | attributes = options.attributes ++ attrsMod }


validationAttribute : FormInternal.Validation -> Html.Attribute msg
validationAttribute validation =
    Attributes.class <| FormInternal.validationToString validation
