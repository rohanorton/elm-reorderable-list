effect module Reorderable.Mouse
    where { subscription = MySub }
    exposing
        ( Position
        , mouseEvent
        , mouseEventDecoder
        , MouseEvent
        , clicks
        , moves
        , downs
        , ups
        )

import Dict
import Dom.LowLevel as Dom
import Json.Decode as Json
import Process
import Task exposing (Task)


-- POSITIONS


type alias Position =
    { x : Int
    , y : Int
    }


type alias MouseEvent =
    { mousePosition : Position
    , offset : Position
    }


mouseEvent : Int -> Int -> Int -> Int -> MouseEvent
mouseEvent pageX pageY offsetX offsetY =
    { mousePosition = Position pageX pageY
    , offset = Position offsetX offsetY
    }


mouseEventDecoder : Json.Decoder MouseEvent
mouseEventDecoder =
    Json.map4 mouseEvent
        (Json.field "pageX" Json.int)
        (Json.field "pageY" Json.int)
        (Json.field "offsetX" Json.int)
        (Json.field "offsetY" Json.int)



-- MOUSE EVENTS


clicks : (MouseEvent -> msg) -> Sub msg
clicks tagger =
    subscription (MySub "click" tagger)


moves : (MouseEvent -> msg) -> Sub msg
moves tagger =
    subscription (MySub "mousemove" tagger)


downs : (MouseEvent -> msg) -> Sub msg
downs tagger =
    subscription (MySub "mousedown" tagger)


{-| Get a position whenever the user *releases* the mouse button.
-}
ups : (MouseEvent -> msg) -> Sub msg
ups tagger =
    subscription (MySub "mouseup" tagger)



-- SUBSCRIPTIONS


type MySub msg
    = MySub String (MouseEvent -> msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func (MySub category tagger) =
    MySub category (tagger >> func)



-- EFFECT MANAGER STATE


type alias State msg =
    Dict.Dict String (Watcher msg)


type alias Watcher msg =
    { taggers : List (MouseEvent -> msg)
    , pid : Process.Id
    }



-- CATEGORIZE SUBSCRIPTIONS


type alias SubDict msg =
    Dict.Dict String (List (MouseEvent -> msg))


categorize : List (MySub msg) -> SubDict msg
categorize subs =
    categorizeHelp subs Dict.empty


categorizeHelp : List (MySub msg) -> SubDict msg -> SubDict msg
categorizeHelp subs subDict =
    case subs of
        [] ->
            subDict

        (MySub category tagger) :: rest ->
            categorizeHelp rest <|
                Dict.update category (categorizeHelpHelp tagger) subDict


categorizeHelpHelp : a -> Maybe (List a) -> Maybe (List a)
categorizeHelpHelp value maybeValues =
    case maybeValues of
        Nothing ->
            Just [ value ]

        Just values ->
            Just (value :: values)



-- EFFECT MANAGER


init : Task Never (State msg)
init =
    Task.succeed Dict.empty


type alias Msg =
    { category : String
    , mouseEvent : MouseEvent
    }


(&>) t1 t2 =
    t1
        |> Task.andThen (\_ -> t2)


onEffects : Platform.Router msg Msg -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router newSubs oldState =
    let
        leftStep category { pid } task =
            Process.kill pid &> task

        bothStep category { pid } taggers task =
            task
                |> Task.andThen (\state -> Task.succeed (Dict.insert category (Watcher taggers pid) state))

        rightStep category taggers task =
            let
                tracker =
                    Dom.onDocument category mouseEventDecoder (Platform.sendToSelf router << Msg category)
            in
                task
                    |> Task.andThen
                        (\state ->
                            Process.spawn tracker
                                |> Task.andThen (\pid -> Task.succeed (Dict.insert category (Watcher taggers pid) state))
                        )
    in
        Dict.merge
            leftStep
            bothStep
            rightStep
            oldState
            (categorize newSubs)
            (Task.succeed Dict.empty)


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router { category, mouseEvent } state =
    case Dict.get category state of
        Nothing ->
            Task.succeed state

        Just { taggers } ->
            let
                send tagger =
                    Platform.sendToApp router (tagger mouseEvent)
            in
                Task.sequence (List.map send taggers)
                    &> Task.succeed state
