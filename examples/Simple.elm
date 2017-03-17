module Simple exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick)
import Reorderable


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { list : List String
    , reorderableState : Reorderable.State
    }


init : ( Model, Cmd Msg )
init =
    { list = [ "apples", "pears", "oranges", "lemons", "peaches", "satsumas" ]
    , reorderableState = Reorderable.initialState
    }
        ! []



-- UPDATE


type Msg
    = ReorderableMsg Reorderable.Msg
    | UpdateList (List String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "Current Message" msg of
        ReorderableMsg childMsg ->
            let
                newReordableState =
                    Reorderable.update childMsg model.reorderableState
            in
                { model | reorderableState = newReordableState } ! []

        UpdateList newList ->
            { model | list = newList } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Reorderable.subscriptions ReorderableMsg model.reorderableState



-- VIEW


reorderableConfig : Reorderable.Config String Msg
reorderableConfig =
    Reorderable.simpleConfig
        { toMsg = ReorderableMsg
        , updateList = UpdateList
        }


view : Model -> Html Msg
view { list, reorderableState } =
    Reorderable.ul reorderableConfig reorderableState list
