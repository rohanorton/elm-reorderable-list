module Reorderable
    exposing
        ( ul
        , ol
        , State
        , initialState
        , Msg
        , update
        , subscriptions
        , Config
        , HtmlWrapper
        , simpleConfig
        , fullConfig
        , Event(..)
        )

{-| This library helps you create drag and drop re-orderable html lists

Check out the [examples][] to see how it works
[examples]: https://github.com/rohanorton/elm-reorderable-list/tree/master/examples

# View
@docs ul, ol

# Config
@docs Config, HtmlWrapper, simpleConfig, fullConfig

# State
@docs State, initialState

# Updates
@docs Msg, update, Event

-}

import Html exposing (li, text, Html, Attribute)
import Html.Lazy as Lazy
import Html.Keyed as Keyed
import Html.Attributes exposing (class, style)
import Html.Events exposing (on, onWithOptions)
import Json.Decode as Json
import Reorderable.Helpers as Helpers
import Mouse exposing (Position)


-- MODEL


{-| Internal state of the re-orderable component.

Tracks which element is being dragged and whether mouse is over an ignored
element.
-}
type State
    = State
        { dragging : Maybe Draggable
        , mouseOverIgnored : Bool
        }


type alias Draggable =
    { id : String
    , start : Position
    , current : Position
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
    = InternalDragStart String Position
    | InternalDragMove Position
    | InternalDragEnd


{-|
-}
type Event
    = DragStart String
    | DragEnd


{-| Update function for updating the state of the component.
-}
update : Msg -> State -> ( State, Maybe Event )
update msg (State state) =
    case msg of
        InternalDragStart id xy ->
            let
                dragging =
                    Draggable id xy xy
            in
                ( State { state | dragging = Just dragging }
                , Just <| DragStart id
                )

        InternalDragMove xy ->
            let
                dragging =
                    state.dragging
                        |> Maybe.map (\draggable -> { draggable | current = xy })
            in
                ( State { state | dragging = dragging }
                , Nothing
                )

        InternalDragEnd ->
            ( State { state | dragging = Nothing }
            , Just <| DragEnd
            )



-- VIEW


{-| Takes a list and turn it into an html, drag and drop re-orderable
ordered-list. `Config` is configuration for the component, describing how the
data should be displayed and how to handle events.

**Note:** `State` and `List data` belong in your `Model`. `Config` belongs in
your view.
-}
ol : Config data msg -> State -> List data -> Html msg
ol ((Config { listClass }) as config) state list =
    Keyed.ol [ class listClass ] <| List.map (liView config list state) list


{-| Takes a list and turn it into an html, drag and drop re-orderable
unordered-list. `Config` is configuration for the component, describing how the
data should be displayed and how to handle events.

**Note:** `State` and `List data` belong in your `Model`. `Config` belongs in
your view.
-}
ul : Config data msg -> State -> List data -> Html msg
ul ((Config { listClass }) as config) state list =
    Keyed.ul [ class listClass ] <| List.map (liView config list state) list


liView : Config data msg -> List data -> State -> data -> ( String, Html msg )
liView ((Config { toId }) as config) list (State state) data =
    let
        id =
            toId data

        currentView =
            case state.dragging of
                Just draggable ->
                    if draggable.id == id then
                        Lazy.lazy3 draggingView config data draggable
                    else
                        Lazy.lazy3 nonDraggingView config data id

                Nothing ->
                    Lazy.lazy3 nonDraggingView config data id
    in
        ( id, currentView )


nonDraggingView : Config data msg -> data -> String -> Html msg
nonDraggingView (Config config) data id =
    li
        ([]
            ++ (onDragStart <| Json.map (config.toMsg << InternalDragStart id) positionDecoder)
        )
        [ config.itemView data ]


draggingView : Config data msg -> data -> Draggable -> Html msg
draggingView (Config config) data draggable =
    let
        { x, y } =
            getPosition draggable
    in
        li
            [ style
                [ "position" => "relative"
                , "display" => "block"
                , "left" => px x
                , "top" => px y
                ]
            ]
            [ config.itemView data ]


subscriptions : (Msg -> msg) -> State -> Sub msg
subscriptions toMsg (State state) =
    case state.dragging of
        Nothing ->
            Sub.batch []

        Just draggable ->
            Sub.batch
                [ Mouse.moves (\xy -> toMsg <| InternalDragMove xy)
                , Mouse.ups (\xy -> toMsg InternalDragEnd)
                ]


getPosition : Draggable -> Position
getPosition { start, current } =
    { x = current.x - start.x
    , y = current.y - start.y
    }


onMulti : List String -> Json.Decoder msg -> List (Attribute msg)
onMulti events decoder =
    let
        handler event =
            on event decoder
    in
        List.map handler events


onDragStart : Json.Decoder msg -> List (Attribute msg)
onDragStart =
    onMulti [ "mousedown" ]


onDragMove : Json.Decoder msg -> List (Attribute msg)
onDragMove =
    onMulti [ "mousemove" ]


onDragEnd : Json.Decoder msg -> List (Attribute msg)
onDragEnd =
    onMulti [ "mouseup", "mouseleave", "mousecancel" ]


positionDecoder : Json.Decoder Position
positionDecoder =
    Mouse.position



-- CONFIG


{-| Configuration for your re-orderable list.

**Note:** Your `Config` should *never* be held in your model.
It should only appear in `view` code.
-}
type Config data msg
    = Config
        { toId : data -> String
        , toMsg : Msg -> msg
        , itemView : data -> Html msg
        , placeholderView : data -> Html msg
        , listClass : String
        , itemClass : String
        , placeholderClass : String
        , draggable : Bool
        , updateList : (() -> List data) -> msg
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
simpleConfig : { toMsg : Msg -> msg, updateList : (() -> List String) -> msg } -> Config String msg
simpleConfig { toMsg, updateList } =
    Config
        { toId = identity
        , toMsg = toMsg
        , itemView = text
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
    , itemView : data -> Html msg
    , placeholderView : data -> Html msg
    , listClass : String
    , itemClass : String
    , placeholderClass : String
    , draggable : Bool
    , updateList : (() -> List data) -> msg
    }
    -> Config data msg
fullConfig =
    Config



-- CSS Helpers


(=>) : String -> String -> ( String, String )
(=>) =
    (,)


px : number -> String
px num =
    (toString num) ++ "px"
