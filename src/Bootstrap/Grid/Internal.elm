module Bootstrap.Grid.Internal exposing (..)

import Html
import Html.Attributes exposing (class)
import Bootstrap.Text as Text
import Bootstrap.Internal.Text as TextInternal
import Bootstrap.General.Internal exposing (ScreenSize(..), HAlign, HorizontalAlign(..), hAlignClass, screenSizeOption)


type ColOption msg
    = ColWidth Width
    | ColOffset Offset
    | ColPull Pull
    | ColPush Push
    | ColOrder Order
    | ColAlign VAlign
    | ColAttrs (List (Html.Attribute msg))
    | TextAlign Text.HAlign


type RowOption msg
    = RowVAlign VAlign
    | RowHAlign HAlign
    | RowAttrs (List (Html.Attribute msg))


type alias Width =
    { screenSize : ScreenSize
    , columnCount : ColumnCount
    }


type alias Offset =
    { screenSize : ScreenSize
    , offsetCount : OffsetCount
    }


type alias Pull =
    { screenSize : ScreenSize
    , moveCount : MoveCount
    }


type alias Push =
    { screenSize : ScreenSize
    , moveCount : MoveCount
    }


type alias Order =
    { screenSize : ScreenSize
    , moveCount : OrderCol
    }


type alias VAlign =
    { screenSize : ScreenSize
    , align : VerticalAlign
    }


type ColumnCount
    = Col
    | Col1
    | Col2
    | Col3
    | Col4
    | Col5
    | Col6
    | Col7
    | Col8
    | Col9
    | Col10
    | Col11
    | Col12
    | ColAuto


type OffsetCount
    = Offset0
    | Offset1
    | Offset2
    | Offset3
    | Offset4
    | Offset5
    | Offset6
    | Offset7
    | Offset8
    | Offset9
    | Offset10
    | Offset11


type MoveCount
    = Move0
    | Move1
    | Move2
    | Move3
    | Move4
    | Move5
    | Move6
    | Move7
    | Move8
    | Move9
    | Move10
    | Move11
    | Move12


type OrderCol
    = OrderFirst
    | Order1
    | Order2
    | Order3
    | Order4
    | Order5
    | Order6
    | Order7
    | Order8
    | Order9
    | Order10
    | Order11
    | Order12
    | OrderLast


type VerticalAlign
    = Top
    | Middle
    | Bottom


type alias ColOptions msg =
    { attributes : List (Html.Attribute msg)
    , textAlign : Maybe Text.HAlign
    , widthXs : Maybe Width
    , widthSm : Maybe Width
    , widthMd : Maybe Width
    , widthLg : Maybe Width
    , widthXl : Maybe Width
    , offsetXs : Maybe Offset
    , offsetSm : Maybe Offset
    , offsetMd : Maybe Offset
    , offsetLg : Maybe Offset
    , offsetXl : Maybe Offset
    , pullXs : Maybe Pull
    , pullSm : Maybe Pull
    , pullMd : Maybe Pull
    , pullLg : Maybe Pull
    , pullXl : Maybe Pull
    , pushXs : Maybe Push
    , pushSm : Maybe Push
    , pushMd : Maybe Push
    , pushLg : Maybe Push
    , pushXl : Maybe Push
    , orderXs : Maybe Order
    , orderSm : Maybe Order
    , orderMd : Maybe Order
    , orderLg : Maybe Order
    , orderXl : Maybe Order
    , alignXs : Maybe VAlign
    , alignSm : Maybe VAlign
    , alignMd : Maybe VAlign
    , alignLg : Maybe VAlign
    , alignXl : Maybe VAlign
    }


type alias RowOptions msg =
    { attributes : List (Html.Attribute msg)
    , vAlignXs : Maybe VAlign
    , vAlignSm : Maybe VAlign
    , vAlignMd : Maybe VAlign
    , vAlignLg : Maybe VAlign
    , vAlignXl : Maybe VAlign
    , hAlignXs : Maybe HAlign
    , hAlignSm : Maybe HAlign
    , hAlignMd : Maybe HAlign
    , hAlignLg : Maybe HAlign
    , hAlignXl : Maybe HAlign
    }


width : ScreenSize -> ColumnCount -> ColOption msg
width size count =
    ColWidth <| Width size count


colVAlign : ScreenSize -> VerticalAlign -> ColOption msg
colVAlign size align =
    ColAlign <| VAlign size align


offset : ScreenSize -> OffsetCount -> ColOption msg
offset size count =
    ColOffset <| Offset size count


pull : ScreenSize -> MoveCount -> ColOption msg
pull size count =
    ColPull <| Pull size count


push : ScreenSize -> MoveCount -> ColOption msg
push size count =
    ColPush <| Push size count


order : ScreenSize -> OrderCol -> ColOption msg
order size count =
    ColOrder <| Order size count


rowVAlign : ScreenSize -> VerticalAlign -> RowOption msg
rowVAlign size align =
    RowVAlign <| VAlign size align


rowHAlign : ScreenSize -> HorizontalAlign -> RowOption msg
rowHAlign size align =
    RowHAlign <| HAlign size align


colAttributes : List (ColOption msg) -> List (Html.Attribute msg)
colAttributes modifiers =
    let
        options =
            List.foldl applyColOption defaultColOptions modifiers

        shouldAddDefaultXs =
            (List.filterMap identity
                [ options.widthXs
                , options.widthSm
                , options.widthMd
                , options.widthLg
                , options.widthXl
                ]
                |> List.length
            )
                == 0
    in
        colWidthsToAttributes
            [ if shouldAddDefaultXs then
                Just <| Width XS Col
              else
                options.widthXs
            , options.widthSm
            , options.widthMd
            , options.widthLg
            , options.widthXl
            ]
            ++ offsetsToAttributes
                [ options.offsetXs
                , options.offsetSm
                , options.offsetMd
                , options.offsetLg
                , options.offsetXl
                ]
            ++ pullsToAttributes
                [ options.pullXs
                , options.pullSm
                , options.pullMd
                , options.pullLg
                , options.pullXl
                ]
            ++ pushesToAttributes
                [ options.pushXs
                , options.pushSm
                , options.pushMd
                , options.pushLg
                , options.pushXl
                ]
            ++ orderToAttributes
                [ options.orderXs
                , options.orderSm
                , options.orderMd
                , options.orderLg
                , options.orderXl
                ]
            ++ vAlignsToAttributes "align-self-"
                [ options.alignXs
                , options.alignSm
                , options.alignMd
                , options.alignLg
                , options.alignXl
                ]
            ++ (case options.textAlign of
                Just a ->
                    [TextInternal.textAlignClass a]
                Nothing ->
                    [])
            ++ options.attributes


rowAttributes : List (RowOption msg) -> List (Html.Attribute msg)
rowAttributes modifiers =
    let
        options =
            List.foldl applyRowOption defaultRowOptions modifiers
    in
        [ class "row" ]
            ++ vAlignsToAttributes "align-items-"
                [ options.vAlignXs
                , options.vAlignSm
                , options.vAlignMd
                , options.vAlignLg
                , options.vAlignXl
                ]
            ++ hAlignsToAttributes
                [ options.hAlignXs
                , options.hAlignSm
                , options.hAlignMd
                , options.hAlignLg
                , options.hAlignXl
                ]
            ++ options.attributes


applyColOption : ColOption msg -> ColOptions msg -> ColOptions msg
applyColOption modifier options =
    case modifier of
        ColAttrs attrs ->
            { options | attributes = options.attributes ++ attrs }

        ColWidth widthMod ->
            applyColWidth widthMod options

        ColOffset offsetMod ->
            applyColOffset offsetMod options

        ColPull pullMod ->
            applyColPull pullMod options

        ColPush pushMod ->
            applyColPush pushMod options

        ColOrder orderMod ->
            applyColOrder orderMod options

        ColAlign alignMod ->
            applyColAlign alignMod options

        TextAlign alignMod ->
            { options | textAlign = Just alignMod }


applyColWidth : Width -> ColOptions msg -> ColOptions msg
applyColWidth widthInput options =
    case widthInput.screenSize of
        XS ->
            { options | widthXs = Just widthInput }

        SM ->
            { options | widthSm = Just widthInput }

        MD ->
            { options | widthMd = Just widthInput }

        LG ->
            { options | widthLg = Just widthInput }

        XL ->
            { options | widthXl = Just widthInput }


applyColOffset : Offset -> ColOptions msg -> ColOptions msg
applyColOffset offsetInput options =
    case offsetInput.screenSize of
        XS ->
            { options | offsetXs = Just offsetInput }

        SM ->
            { options | offsetSm = Just offsetInput }

        MD ->
            { options | offsetMd = Just offsetInput }

        LG ->
            { options | offsetLg = Just offsetInput }

        XL ->
            { options | offsetXl = Just offsetInput }


applyColPull : Pull -> ColOptions msg -> ColOptions msg
applyColPull pullInput options =
    case pullInput.screenSize of
        XS ->
            { options | pullXs = Just pullInput }

        SM ->
            { options | pullSm = Just pullInput }

        MD ->
            { options | pullMd = Just pullInput }

        LG ->
            { options | pullLg = Just pullInput }

        XL ->
            { options | pullXl = Just pullInput }


applyColPush : Push -> ColOptions msg -> ColOptions msg
applyColPush pushInput options =
    case pushInput.screenSize of
        XS ->
            { options | pushXs = Just pushInput }

        SM ->
            { options | pushSm = Just pushInput }

        MD ->
            { options | pushMd = Just pushInput }

        LG ->
            { options | pushLg = Just pushInput }

        XL ->
            { options | pushXl = Just pushInput }


applyColOrder : Order -> ColOptions msg -> ColOptions msg
applyColOrder orderInput options =
    case orderInput.screenSize of
        XS ->
            { options | orderXs = Just orderInput }

        SM ->
            { options | orderSm = Just orderInput }

        MD ->
            { options | orderMd = Just orderInput }

        LG ->
            { options | orderLg = Just orderInput }

        XL ->
            { options | orderXl = Just orderInput }


applyColAlign : VAlign -> ColOptions msg -> ColOptions msg
applyColAlign align options =
    case align.screenSize of
        XS ->
            { options | alignXs = Just align }

        SM ->
            { options | alignSm = Just align }

        MD ->
            { options | alignMd = Just align }

        LG ->
            { options | alignLg = Just align }

        XL ->
            { options | alignXl = Just align }


applyRowOption : RowOption msg -> RowOptions msg -> RowOptions msg
applyRowOption modifier options =
    case modifier of
        RowAttrs attrs ->
            { options | attributes = options.attributes ++ attrs }

        RowVAlign align ->
            applyRowVAlign align options

        RowHAlign align ->
            applyRowHAlign align options


applyRowVAlign : VAlign -> RowOptions msg -> RowOptions msg
applyRowVAlign align options =
    case align.screenSize of
        XS ->
            { options | vAlignXs = Just align }

        SM ->
            { options | vAlignSm = Just align }

        MD ->
            { options | vAlignMd = Just align }

        LG ->
            { options | vAlignLg = Just align }

        XL ->
            { options | vAlignXl = Just align }


applyRowHAlign : HAlign -> RowOptions msg -> RowOptions msg
applyRowHAlign align options =
    case align.screenSize of
        XS ->
            { options | hAlignXs = Just align }

        SM ->
            { options | hAlignSm = Just align }

        MD ->
            { options | hAlignMd = Just align }

        LG ->
            { options | hAlignLg = Just align }

        XL ->
            { options | hAlignXl = Just align }


defaultColOptions : ColOptions msg
defaultColOptions =
    { attributes = []
    , textAlign = Nothing
    , widthXs = Nothing
    , widthSm = Nothing
    , widthMd = Nothing
    , widthLg = Nothing
    , widthXl = Nothing
    , offsetXs = Nothing
    , offsetSm = Nothing
    , offsetMd = Nothing
    , offsetLg = Nothing
    , offsetXl = Nothing
    , pullXs = Nothing
    , pullSm = Nothing
    , pullMd = Nothing
    , pullLg = Nothing
    , pullXl = Nothing
    , pushXs = Nothing
    , pushSm = Nothing
    , pushMd = Nothing
    , pushLg = Nothing
    , pushXl = Nothing
    , orderXs = Nothing
    , orderSm = Nothing
    , orderMd = Nothing
    , orderLg = Nothing
    , orderXl = Nothing
    , alignXs = Nothing
    , alignSm = Nothing
    , alignMd = Nothing
    , alignLg = Nothing
    , alignXl = Nothing
    }


defaultRowOptions : RowOptions msg
defaultRowOptions =
    { attributes = []
    , vAlignXs = Nothing
    , vAlignSm = Nothing
    , vAlignMd = Nothing
    , vAlignLg = Nothing
    , vAlignXl = Nothing
    , hAlignXs = Nothing
    , hAlignSm = Nothing
    , hAlignMd = Nothing
    , hAlignLg = Nothing
    , hAlignXl = Nothing
    }


colWidthsToAttributes : List (Maybe Width) -> List (Html.Attribute msg)
colWidthsToAttributes widths =
    let
        widthVar w =
            Maybe.map colWidthClass w
    in
        List.map widthVar widths
            |> List.filterMap identity


colWidthClass : Width -> Html.Attribute msg
colWidthClass { screenSize, columnCount } =
    "col"
        ++ (Maybe.map (\v -> "-" ++ v) (screenSizeOption screenSize)
                |> Maybe.withDefault ""
           )
        ++ (Maybe.map (\v -> "-" ++ v) (columnCountOption columnCount)
                |> Maybe.withDefault ""
           )
        |> class


offsetsToAttributes : List (Maybe Offset) -> List (Html.Attribute msg)
offsetsToAttributes offsets =
    let
        offsetVar m =
            Maybe.map offsetClass m
    in
        List.map offsetVar offsets
            |> List.filterMap identity


offsetClass : Offset -> Html.Attribute msg
offsetClass { screenSize, offsetCount } =
    class <| "offset" ++ screenSizeToPartialString screenSize ++ offsetCountOption offsetCount


pullsToAttributes : List (Maybe Pull) -> List (Html.Attribute msg)
pullsToAttributes pulls =
    let
        pullVar m =
            case m of
                Just { screenSize, moveCount } ->
                    Just <| class <| "pull" ++ screenSizeToPartialString screenSize ++ moveCountOption moveCount

                Nothing ->
                    Nothing
    in
        List.map pullVar pulls
            |> List.filterMap identity


pushesToAttributes : List (Maybe Pull) -> List (Html.Attribute msg)
pushesToAttributes pushes =
    let
        pushVar m =
            case m of
                Just { screenSize, moveCount } ->
                    Just <| class <| "push" ++ screenSizeToPartialString screenSize ++ moveCountOption moveCount

                Nothing ->
                    Nothing
    in
        List.map pushVar pushes
            |> List.filterMap identity


orderToAttributes : List (Maybe Order) -> List (Html.Attribute msg)
orderToAttributes orders =
    let
        orderVar m =
            case m of
                Just { screenSize, moveCount } ->
                    Just <| class <| "order" ++ screenSizeToPartialString screenSize ++ orderColOption moveCount

                Nothing ->
                    Nothing
    in
        List.map orderVar orders
            |> List.filterMap identity


vAlignsToAttributes : String -> List (Maybe VAlign) -> List (Html.Attribute msg)
vAlignsToAttributes prefix aligns =
    let
        align a =
            Maybe.map (vAlignClass prefix) a
    in
        List.map align aligns
            |> List.filterMap identity


vAlignClass : String -> VAlign -> Html.Attribute msg
vAlignClass prefix { align, screenSize } =
    class <|
        (prefix
            ++ (Maybe.map (\v -> v ++ "-") (screenSizeOption screenSize)
                    |> Maybe.withDefault ""
               )
            ++ verticalAlignOption align
        )


hAlignsToAttributes : List (Maybe HAlign) -> List (Html.Attribute msg)
hAlignsToAttributes aligns =
    let
        align a =
            Maybe.map hAlignClass a
    in
        List.map align aligns
            |> List.filterMap identity


screenSizeToPartialString : ScreenSize -> String
screenSizeToPartialString screenSize =
    case screenSizeOption screenSize of
        Just s ->
            "-" ++ s ++ "-"

        Nothing ->
            "-"


columnCountOption : ColumnCount -> Maybe String
columnCountOption size =
    case size of
        Col ->
            Nothing

        Col1 ->
            Just "1"

        Col2 ->
            Just "2"

        Col3 ->
            Just "3"

        Col4 ->
            Just "4"

        Col5 ->
            Just "5"

        Col6 ->
            Just "6"

        Col7 ->
            Just "7"

        Col8 ->
            Just "8"

        Col9 ->
            Just "9"

        Col10 ->
            Just "10"

        Col11 ->
            Just "11"

        Col12 ->
            Just "12"

        ColAuto ->
            Just "auto"


offsetCountOption : OffsetCount -> String
offsetCountOption size =
    case size of
        Offset0 ->
            "0"

        Offset1 ->
            "1"

        Offset2 ->
            "2"

        Offset3 ->
            "3"

        Offset4 ->
            "4"

        Offset5 ->
            "5"

        Offset6 ->
            "6"

        Offset7 ->
            "7"

        Offset8 ->
            "8"

        Offset9 ->
            "9"

        Offset10 ->
            "10"

        Offset11 ->
            "11"


moveCountOption : MoveCount -> String
moveCountOption size =
    case size of
        Move0 ->
            "0"

        Move1 ->
            "1"

        Move2 ->
            "2"

        Move3 ->
            "3"

        Move4 ->
            "4"

        Move5 ->
            "5"

        Move6 ->
            "6"

        Move7 ->
            "7"

        Move8 ->
            "8"

        Move9 ->
            "9"

        Move10 ->
            "10"

        Move11 ->
            "11"

        Move12 ->
            "12"


orderColOption : OrderCol -> String
orderColOption size =
    case size of
        OrderFirst ->
            "first"

        Order1 ->
            "1"

        Order2 ->
            "2"

        Order3 ->
            "3"

        Order4 ->
            "4"

        Order5 ->
            "5"

        Order6 ->
            "6"

        Order7 ->
            "7"

        Order8 ->
            "8"

        Order9 ->
            "9"

        Order10 ->
            "10"

        Order11 ->
            "11"

        Order12 ->
            "12"

        OrderLast ->
            "last"


verticalAlignOption : VerticalAlign -> String
verticalAlignOption align =
    case align of
        Top ->
            "start"

        Middle ->
            "center"

        Bottom ->
            "end"
