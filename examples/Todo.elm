module Todo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, type', value)
import Html.Events exposing (onClick, onInput)
import Html.App
import Reorderable


main : Program Never
main =
    Html.App.beginnerProgram
        { model = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { todos : Todos
    , reorderableState : Reorderable.State
    }


type alias Todos =
    List Todo


type alias Todo =
    { id : String
    , action : String
    , done : Bool
    }


defaultTodo : Todo
defaultTodo =
    { id = ""
    , action = ""
    , done = False
    }


initialTodos : Todos
initialTodos =
    [ { defaultTodo | id = "1", action = "Buy train ticket" }
    , { defaultTodo | id = "2", action = "Buy sandwiches" }
    , { defaultTodo | id = "3", action = "Lookup directions" }
    ]


init : Model
init =
    { todos = initialTodos
    , reorderableState = Reorderable.initialState
    }



-- UPDATE


type Msg
    = UpdateDone String Bool
    | UpdateEntry String String
    | ReorderableMsg Reorderable.Msg
    | ReorderList (() -> List Todo)


update : Msg -> Model -> Model
update msg model =
    case Debug.log "Current Message" msg of
        UpdateDone id done ->
            let
                newTodos =
                    updateDone id done model.todos
            in
                { model | todos = newTodos }

        UpdateEntry id newAction ->
            let
                newTodos =
                    updateAction id newAction model.todos
            in
                { model | todos = newTodos }

        ReorderableMsg childMsg ->
            let
                newReordableState =
                    Reorderable.update childMsg model.reorderableState
            in
                { model | reorderableState = newReordableState }

        ReorderList newTodosThunk ->
            { model | todos = newTodosThunk () }


updateDone : String -> Bool -> Todos -> Todos
updateDone id done todos =
    List.map
        (\todo ->
            if todo.id == id then
                { todo | done = done }
            else
                todo
        )
        todos


updateAction : String -> String -> Todos -> Todos
updateAction id action todos =
    List.map
        (\todo ->
            if todo.id == id then
                { todo | action = action }
            else
                todo
        )
        todos



-- VIEW


view : Model -> Html Msg
view { todos, reorderableState } =
    Reorderable.ul
        (Reorderable.fullConfig
            { toId = .id
            , toMsg = ReorderableMsg
            , draggable = True
            , updateList = ReorderList
            , itemView = todoView
            , itemClass = ""
            , listClass = ""
            , placeholderClass = ""
            }
        )
        reorderableState
        todos


todoView : Reorderable.HtmlWrapper Msg -> Todo -> Html Msg
todoView ignoreDrag { id, action, done } =
    div []
        [ input
            [ type' "checkbox"
            , onClick <| UpdateDone id (not done)
            , value <| toString done
            ]
            []
          {- Selecting text inside input shouldn't begin drag, so using the
             ignoreDrag function to prevent:
          -}
        , ignoreDrag input
            [ type' "text"
            , onInput <| UpdateEntry id
            , value action
            ]
            []
        ]
