module Reorderable
    exposing
        ( Config
        , div
        , Event(..)
        , fullConfig
        , HtmlWrapper
        , initialState
        , Msg
        , ol
        , simpleConfig
        , State
        , subscriptions
        , ul
        , update
        )

{-| This library helps you create drag and drop re-orderable html lists

Check out the [examples][] to see how it works
[examples]: https://github.com/rohanorton/elm-reorderable-list/tree/master/examples

# View
@docs ul, ol, div

# Config
@docs Config, HtmlWrapper, simpleConfig, fullConfig

# State
@docs State, initialState

# Subscriptions
@docs subscriptions

# Updates
@docs Msg, update, Event

-}

import Html exposing (text, Html, Attribute)
import Html.Keyed as Keyed
import Html.Attributes exposing (class, style)
import Html.Events exposing (on, onWithOptions)
import Json.Decode as Json
import Reorderable.Helpers as Helpers
import Reorderable.Mouse as Mouse exposing (Position, MouseEvent)


-- MODEL


{-| Internal state of the re-orderable component.

Tracks which element is being dragged and whether mouse is over an ignored
element.
-}
type State
    = State
        { dragging : Maybe DraggedItem
        , mouseOverIgnored : Bool
        }


type alias DraggedItem =
    { id : String
    , clientPos : Position
    , offset : Position
    }


{-| Create the inital state for your re-orderable component.
-}
initialState : State
initialState =
    State
        { dragging = Nothing
        , mouseOverIgnored = False
        }



-- UPDATE


{-| Messages sent to internal update command used for updating the internal
component state.
-}
type Msg
    = MouseOverIgnored Bool
    | InternalDragStart String MouseEvent
    | InternalDragMove MouseEvent
    | InternalDragEnd


{-|
-}
type Event
    = DragEnd
    | DragStart String
    | DragMove


{-| Update function for updating the state of the component.
-}
update : Msg -> State -> ( State, Maybe Event )
update msg (State state) =
    case Debug.log "msg" msg of
        MouseOverIgnored mouseOverIgnored ->
            ( State { state | mouseOverIgnored = mouseOverIgnored }
            , Nothing
            )

        InternalDragStart id ev ->
            let
                dragging =
                    DraggedItem id ev.clientPos ev.offset
            in
                ( State { state | dragging = Just dragging }
                , Just <| DragStart id
                )

        InternalDragMove ev ->
            let
                dragging =
                    state.dragging
                        |> Maybe.map
                            (\dragged ->
                                { dragged
                                    | clientPos = ev.clientPos
                                }
                            )
            in
                ( State { state | dragging = dragging }
                , Nothing
                )

        InternalDragEnd ->
            ( State { state | dragging = Nothing }
            , Just <| DragEnd
            )



-- SUBSCRIPTIONS


{-| Subscriptions function
-}
subscriptions : (Msg -> msg) -> State -> Sub msg
subscriptions toMsg (State state) =
    case state.dragging of
        Nothing ->
            Sub.batch []

        Just _ ->
            Sub.batch
                [ Mouse.moves <| toMsg << InternalDragMove
                , Mouse.ups <| always <| toMsg InternalDragEnd
                ]



-- VIEW


{-| Takes a list and turn it into an html, drag and drop re-orderable
ordered-list. `Config` is configuration for the component, describing how the
data should be displayed and how to handle events.

**Note:** `State` and `List data` belong in your `Model`. `Config` belongs in
your view.
-}
ol : Config data msg -> State -> List data -> Html msg
ol ((Config { listClass }) as config) state list =
    Keyed.ol [ class listClass ] <| List.concatMap (childView Html.li config list state) list


{-| Takes a list and turn it into an html, drag and drop re-orderable
unordered-list. `Config` is configuration for the component, describing how the
data should be displayed and how to handle events.

**Note:** `State` and `List data` belong in your `Model`. `Config` belongs in
your view.
-}
ul : Config data msg -> State -> List data -> Html msg
ul ((Config { listClass }) as config) state list =
    Keyed.ul [ class listClass ] <| List.concatMap (childView Html.li config list state) list


{-| Takes a list and turn it into an html, drag and drop re-orderable
divs. `Config` is configuration for the component, describing how the
data should be displayed and how to handle events.

**Note:** `State` and `List data` belong in your `Model`. `Config` belongs in
your view.
-}
div : Config data msg -> State -> List data -> Html msg
div ((Config { listClass }) as config) state list =
    Keyed.node "div" [ class listClass ] <| (List.concatMap (childView Html.div config list state) list)


includeDragged : HtmlElement msg -> Config data msg -> State -> data -> List ( String, Html msg )
includeDragged element (Config config) (State state) data =
    case state.dragging of
        Nothing ->
            []

        Just dragged ->
            if dragged.id == (config.toId data) then
                [ draggingView element (Config config) dragged data ]
            else
                []


type alias HtmlElement msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


childView : HtmlElement msg -> Config data msg -> List data -> State -> data -> List ( String, Html msg )
childView element (Config config) list (State state) data =
    let
        id =
            config.toId data

        ( childView, childClass ) =
            if isDragging then
                ( config.placeholderView data, config.placeholderClass )
            else
                ( config.itemView (ignoreDrag config.toMsg) data, config.itemClass )

        isDragging =
            state.dragging
                |> Maybe.map .id
                |> Maybe.map ((==) id)
                |> Maybe.withDefault False

        dragged =
            includeDragged element (Config config) (State state) data
    in
        ( id
        , element
            [ onDragStart state.mouseOverIgnored <| config.toMsg << InternalDragStart id
            , onDragOver config.updateList
                (\() ->
                    Helpers.updateList config.toId id (Maybe.map .id state.dragging) list
                )
                (state.dragging /= Nothing)
            , class childClass
            ]
            [ childView ]
        )
            :: dragged


draggingView : HtmlElement msg -> Config data msg -> DraggedItem -> data -> ( String, Html msg )
draggingView element (Config config) draggedItem data =
    let
        { x, y } =
            getPosition draggedItem
    in
        ( "__draggedItem"
        , element
            [ style
                [ "position" => "absolute"
                , "display" => "block"
                , "left" => px x
                , "top" => px y
                , "pointer-events" => "none"
                , "cursor" => "pointer"
                ]
            , class config.itemClass
            ]
            [ config.itemView (ignoreDrag config.toMsg) data ]
        )


getPosition : DraggedItem -> Position
getPosition { clientPos, offset } =
    { x = clientPos.x - offset.x
    , y = clientPos.y - offset.y
    }


onDragStart : Bool -> (MouseEvent -> msg) -> Attribute msg
onDragStart ignored tagger =
    Mouse.mouseEventDecoder
        |> Json.andThen (decodeWhen <| not ignored)
        |> Json.map tagger
        |> on "mousedown"


decodeWhen : Bool -> a -> Json.Decoder a
decodeWhen condition x =
    if condition then
        Json.succeed x
    else
        Json.fail "Not this time"


onDragEnd : msg -> Attribute msg
onDragEnd msg =
    on "mouseup" <| Json.succeed msg


onDragOver : (List a -> msg) -> (() -> List a) -> Bool -> Attribute msg
onDragOver updateList listThunk isDragging =
    Json.succeed ()
        |> Json.andThen (decodeWhen isDragging)
        |> Json.andThen (Json.succeed << updateList << listThunk)
        |> on "mouseover"


ignoreDrag : (Msg -> msg) -> HtmlWrapper msg
ignoreDrag toMsg elem attr children =
    elem
        (attr
            ++ [ on "mouseenter" <| Json.succeed <| toMsg <| MouseOverIgnored True
               , on "mouseleave" <| Json.succeed <| toMsg <| MouseOverIgnored False
               ]
        )
        children



-- CONFIG


{-| Configuration for your re-orderable list.

**Note:** Your `Config` should *never* be held in your model.
It should only appear in `view` code.
-}
type Config data msg
    = Config
        { toId : data -> String
        , toMsg : Msg -> msg
        , itemView : HtmlWrapper msg -> data -> Html msg
        , placeholderView : data -> Html msg
        , listClass : String
        , itemClass : String
        , placeholderClass : String
        , draggable : Bool
        , updateList : List data -> msg
        }


{-| This type alias is to simplify the definition of a function that takes a
standard html function and its arguments to return a Html msg

This doesn't make much sense in the abstract, check out the ignoreDrag function
for an example
-}
type alias HtmlWrapper msg =
    (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg


{-| A really really simple re-orderable list.

For creating a basic reorderable ul or ol from a list of strings. It's
painfully simple and probably a bit useless!

-}
simpleConfig : { toMsg : Msg -> msg, updateList : List String -> msg } -> Config String msg
simpleConfig { toMsg, updateList } =
    Config
        { toId = identity
        , toMsg = toMsg
        , itemView = always text
        , listClass = ""
        , itemClass = ""
        , placeholderClass = ""
        , placeholderView = text
        , draggable = True
        , updateList = updateList
        }


{-| Provides all the bells and whistles that this library has to offer at this
time.

- toId: Converts your data into an ID string. This *must* be a unique ID for
    the component to work effectively!

-}
fullConfig :
    { toId : data -> String
    , toMsg : Msg -> msg
    , itemView : HtmlWrapper msg -> data -> Html msg
    , placeholderView : data -> Html msg
    , listClass : String
    , itemClass : String
    , placeholderClass : String
    , draggable : Bool
    , updateList : List data -> msg
    }
    -> Config data msg
fullConfig =
    Config



-- HELPERS


(=>) : String -> String -> ( String, String )
(=>) =
    (,)


px : number -> String
px num =
    (toString num) ++ "px"
