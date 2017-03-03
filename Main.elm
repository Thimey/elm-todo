module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, target, href, property, defaultValue, value)
import Html.Events exposing (..)
import Uuid


--import Html.Attributes exposing (..)
--import Html.Events exposing (onClick)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }


type alias Model =
    { todos : List Todo
    , input : String
    , lastId : Int
    , filter : Filter
    }


type alias Todo =
    { id : Int
    , label : String
    , status : TodoStatus
    }


type TodoStatus
    = Complete
    | Incomplete


type Filter
    = All
    | By TodoStatus


initialModel : Model
initialModel =
    { todos = []
    , input = "hello"
    , lastId = 1
    , filter = All
    }


toggleTodo : Todo -> Todo
toggleTodo todo =
    case todo.status of
        Complete ->
            { todo | status = Incomplete }

        Incomplete ->
            { todo | status = Complete }


type Msg
    = SetInput String
    | AddTodo Todo
    | RemoveTodo Int
    | ToggleTodo Int
    | FilterTodos Filter


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetInput input ->
            { model | input = input }

        AddTodo todo ->
            { model | todos = model.todos ++ [ todo ], lastId = model.lastId + 1, input = "" }

        RemoveTodo id ->
            { model | todos = List.filter (\todo -> todo.id /= id) model.todos }

        ToggleTodo id ->
            { model
                | todos =
                    List.map
                        (\todo ->
                            if todo.id == id then
                                toggleTodo todo
                            else
                                todo
                        )
                        model.todos
            }

        FilterTodos filter ->
            { model | filter = filter }


renderTodo : Todo -> Html Msg
renderTodo todo =
    li []
        [ span
            [ class
                (if todo.status == Complete then
                    "completedTodo"
                 else
                    "incompletTodo"
                )
            , onClick (ToggleTodo todo.id)
            ]
            [ text todo.label
            , span [ onClick (RemoveTodo todo.id) ] [ text "X" ]
            ]
        ]


filterTodos : Filter -> List Todo -> List Todo
filterTodos filter todos =
    case filter of
        All ->
            todos

        By what ->
            List.filter (\todo -> todo.status == what) todos


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text ("asd's Todo app") ]
        , p [] [ text "Start adding a todo!" ]
        , button [ onClick (FilterTodos (By Complete)) ] [ text "Complete" ]
        , button [ onClick (FilterTodos (By Incomplete)) ] [ text "Incomplete" ]
        , button [ onClick (FilterTodos All) ] [ text "All" ]
        , input [ onInput SetInput, value model.input ] []
        , button [ onClick (AddTodo (Todo (model.lastId + 1) model.input Incomplete)) ] [ text "Add" ]
        , ul []
            (model.todos
                |> (filterTodos model.filter)
                |> List.map renderTodo
            )
        ]
