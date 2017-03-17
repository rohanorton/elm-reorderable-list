module Todo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, type_, value, checked, readonly)
import Html.Events exposing (on, onClick, onInput, keyCode)
import Json.Decode as Json
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


init : ( Model, Cmd Msg )
init =
    { todos = initialTodos
    , reorderableState = Reorderable.initialState
    , newTask = ""
    , nextId = 4
    }
        ! []



-- UPDATE


type Msg
    = UpdateNewTaskInput String
    | AddTask
    | UpdateDone String Bool
    | UpdateEntry String String
    | ReorderableMsg Reorderable.Msg
    | ReorderList (List Todo)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "Current Message" msg of
        UpdateNewTaskInput newTask ->
            { model | newTask = newTask } ! []

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
                    ! []

        UpdateDone id done ->
            let
                newTodos =
                    updateDone id done model.todos
            in
                { model | todos = newTodos } ! []

        UpdateEntry id newAction ->
            let
                newTodos =
                    updateAction id newAction model.todos
            in
                { model | todos = newTodos } ! []

        ReorderableMsg childMsg ->
            let
                newReordableState =
                    Reorderable.update childMsg model.reorderableState
            in
                { model | reorderableState = newReordableState } ! []

        ReorderList newTodos ->
            { model | todos = newTodos } ! []


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Reorderable.subscriptions ReorderableMsg model.reorderableState



-- VIEW


view : Model -> Html Msg
view { newTask, todos, reorderableState } =
    div []
        [ h1 [] [ text "Re-orderable Todo List" ]
        , div [ class "todo-list__container" ]
            [ newTaskView newTask
            , Reorderable.ul
                (Reorderable.fullConfig
                    { toId = .id
                    , toMsg = ReorderableMsg
                    , draggable = True
                    , updateList = ReorderList
                    , itemView = taskView
                    , placeholderView = placeholderView
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
        [ type_ "text"
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
        , ignoreDrag input
            [ type_ "checkbox"
            , onClick <| UpdateDone id (not done)
            , checked done
            ]
            []
          {- Selecting text inside input shouldn't begin drag, so using the
             ignoreDrag function to prevent:
          -}
        , ignoreDrag input
            [ type_ "text"
            , onInput <| UpdateEntry id
            , value action
            , readonly done
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
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "Is not enter"
    in
        keyCode
            |> Json.andThen isEnter
            |> on "keydown"


(=>) : String -> String -> ( String, String )
(=>) =
    (,)
