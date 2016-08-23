module Todo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, type', value)
import Html.Events exposing (onClick)
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
    , reorderableState = Reorderable.init
    }



-- UPDATE


type Msg
    = Toggle String
    | ReorderableMsg Reorderable.Msg
    | UpdateList (() -> List Todo)


update : Msg -> Model -> Model
update msg model =
    case Debug.log "Current Message" msg of
        Toggle id ->
            let
                newTodos =
                    toggleDone id model.todos
            in
                { model | todos = newTodos }

        ReorderableMsg childMsg ->
            let
                newReordableState =
                    Reorderable.update childMsg model.reorderableState
            in
                { model | reorderableState = newReordableState }

        UpdateList newTodosThunk ->
            { model | todos = newTodosThunk () }


toggleDone : String -> Todos -> Todos
toggleDone id todos =
    List.map
        (\todo ->
            if todo.id == id then
                { todo | done = not todo.done }
            else
                todo
        )
        todos



-- VIEW


view : Model -> Html Msg
view { todos, reorderableState } =
    Reorderable.ul
        { toId = .id
        , toMsg = ReorderableMsg
        , draggable = True
        , updateList = UpdateList
        , itemView = todoView
        }
        reorderableState
        todos


todoView : Todo -> Html Msg
todoView { id, action, done } =
    div []
        [ input [ type' "checkbox", onClick <| Toggle id, value <| toString done ] []
        , Reorderable.ignore ReorderableMsg <| input [ type' "text", value action ] []
        ]
