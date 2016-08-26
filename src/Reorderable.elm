module Reorderable
    exposing
        ( ul
        , ol
        , State
        , initialState
        , Msg
        , update
        , Config
        , HtmlWrapper
        , simpleConfig
        , fullConfig
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
@docs Msg, update

-}

import Html exposing (li, text, Html, Attribute)
import Html.Keyed as Keyed
import Html.Attributes exposing (draggable, class)
import Html.Events exposing (on, onWithOptions)
import Json.Decode as Json
import Reorderable.Helpers as Helpers


-- MODEL


{-| Internal state of the re-orderable component.

Tracks which element is being dragged and whether mouse is over an ignored
element.
-}
type State
    = State
        { dragging : Maybe String
        , mouseOverIgnored : Bool
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
    | StartDragging String
    | StopDragging


{-| Update function for updating the state of the component.
-}
update : Msg -> State -> State
update msg (State state) =
    case msg of
        MouseOverIgnored mouseOverIgnored ->
            State { state | mouseOverIgnored = mouseOverIgnored }

        StartDragging id ->
            let
                dragging =
                    if state.mouseOverIgnored then
                        Nothing
                    else
                        Just id
            in
                State { state | dragging = dragging }

        StopDragging ->
            State { state | dragging = Nothing }



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
liView (Config config) list (State state) data =
    let
        id =
            config.toId data

        itemClass =
            if state.dragging == Just id then
                config.placeholderClass
            else
                config.itemClass
    in
        ( id
        , li
            [ draggable <| toString config.draggable
            , onWithOptions "dragstart"
                { stopPropagation = state.mouseOverIgnored
                , preventDefault = state.mouseOverIgnored
                }
                <| Json.succeed
                <| config.toMsg (StartDragging id)
            , on "dragend" <| Json.succeed <| config.toMsg <| StopDragging
            , on "dragenter"
                <| Json.succeed
                <| config.updateList
                <| (\() -> Helpers.updateList config.toId id state.dragging list)
            , class itemClass
            ]
            [ config.itemView (ignoreDrag config.toMsg) data ]
        )


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
    (List (Attribute msg)
     -> List (Html msg)
     -> Html msg
    )
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
        , itemView = always text
        , listClass = ""
        , itemClass = ""
        , placeholderClass = ""
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
    , listClass : String
    , itemClass : String
    , placeholderClass : String
    , draggable : Bool
    , updateList : (() -> List data) -> msg
    }
    -> Config data msg
fullConfig =
    Config
