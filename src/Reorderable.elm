module Reorderable exposing (Msg, State, init, update, ul, ol)

import Html exposing (li, Html, Attribute)
import Html.Keyed as Keyed
import Html.Attributes exposing (draggable)
import Html.Events exposing (on, onWithOptions)
import Json.Decode as Json
import Reorderable.Helpers as Helpers


-- MODEL


type State
    = State
        { dragging : Maybe String
        , mouseOver : Bool
        }


init : State
init =
    State
        { dragging = Nothing
        , mouseOver = False
        }



-- UPDATE


type Msg
    = MouseOver Bool
    | StartDragging String
    | StopDragging


type alias UpdateConfig data =
    { toId : data -> String
    }


update : Msg -> State -> State
update msg (State state) =
    case msg of
        MouseOver mouseOver ->
            State { state | mouseOver = mouseOver }

        StartDragging id ->
            let
                dragging =
                    if state.mouseOver then
                        Nothing
                    else
                        Just id
            in
                State { state | dragging = dragging }

        StopDragging ->
            State { state | dragging = Nothing }



-- VIEW


type alias Config data msg =
    { toId : data -> String
    , toMsg : Msg -> msg
    , itemView : data -> Html msg
    , draggable : Bool
    , updateList : (() -> List data) -> msg
    }


ol : Config data msg -> State -> List data -> Html msg
ol config state list =
    Keyed.ol [] <| List.map (liView config list state) list


ul : Config data msg -> State -> List data -> Html msg
ul config state list =
    Keyed.ul [] <| List.map (liView config list state) list


liView : Config data msg -> List data -> State -> data -> ( String, Html msg )
liView config list (State state) data =
    let
        id =
            config.toId data
    in
        ( id
        , li
            [ draggable <| toString config.draggable
            , onWithOptions "dragstart"
                { stopPropagation = state.mouseOver
                , preventDefault = state.mouseOver
                }
                <| Json.succeed
                <| config.toMsg (StartDragging id)
            , on "dragend" <| Json.succeed <| config.toMsg <| StopDragging
            , on "dragenter"
                <| Json.succeed
                <| config.updateList
                <| (\() -> Helpers.updateList config.toId id state.dragging list)
            ]
            [ config.itemView data ]
        )


ignore : (Msg -> msg) -> List (Attribute msg) -> List (Attribute msg)
ignore toMsg attributes =
    attributes
        ++ [ on "mouseenter" <| Json.succeed <| toMsg <| MouseOver True
           , on "mouseleave" <| Json.succeed <| toMsg <| MouseOver False
           ]
