module Todo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, type', value, checked)
import Html.Events exposing (on, onClick, onInput, keyCode)
import Html.App
import Json.Decode as Json
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
    , newTask : String
    , nextId : Int
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
    , newTask = ""
    , nextId = 4
    }



-- UPDATE


type Msg
    = UpdateNewTaskInput String
    | AddTask
    | UpdateDone String Bool
    | UpdateEntry String String
    | ReorderableMsg Reorderable.Msg
    | ReorderList (() -> List Todo)
    | NoOp


update : Msg -> Model -> Model
update msg model =
    case Debug.log "Current Message" msg of
        UpdateNewTaskInput newTask ->
            { model | newTask = newTask }

        AddTask ->
            let
                newTodo =
                    { defaultTodo
                        | id = toString model.nextId
                        , action = model.newTask
                    }
            in
                { model
                    | newTask = ""
                    , nextId = model.nextId + 1
                    , todos = model.todos ++ [ newTodo ]
                }

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
                ( newReordableState, _ ) =
                    Reorderable.update childMsg model.reorderableState
            in
                { model | reorderableState = newReordableState }

        ReorderList newTodosThunk ->
            { model | todos = newTodosThunk () }

        NoOp ->
            model


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
view { newTask, todos, reorderableState } =
    div []
        [ h1 [] [ text "Re-orderable Todo List" ]
        , div [ class "container" ]
            [ newTaskView newTask
            , Reorderable.ul
                (Reorderable.fullConfig
                    { toId = .id
                    , toMsg = ReorderableMsg
                    , draggable = True
                    , updateList = ReorderList
                    , itemView = taskView
                    , placeholderView = Just placeholderView
                    , listClass = "todo-list"
                    , itemClass = "todo-list__item"
                    , placeholderClass = "todo-list__placeholder"
                    }
                )
                reorderableState
                todos
            ]
        ]


newTaskView : String -> Html Msg
newTaskView newTask =
    input
        [ type' "text"
        , class "todo-list__new-input"
        , onInput UpdateNewTaskInput
        , onEnter AddTask
        , value newTask
        ]
        []


taskView : Reorderable.HtmlWrapper Msg -> Todo -> Html Msg
taskView ignoreDrag { id, action, done } =
    div []
        [ div [ class "todo-list__item__handle" ] []
        , input
            [ type' "checkbox"
            , onClick <| UpdateDone id (not done)
            , checked done
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


placeholderView : a -> Html Msg
placeholderView _ =
    div []
        [ text "dragging!" ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        tagger code =
            if code == 13 then
                msg
            else
                NoOp
    in
        on "keydown" (Json.map tagger keyCode)


(=>) : String -> String -> ( String, String )
(=>) =
    (,)
