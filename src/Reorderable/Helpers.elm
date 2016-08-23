module Reorderable.Helpers exposing (moveTo, updateList)

import List.Extra as List


moveTo : Int -> Int -> List a -> List a
moveTo startIndex destIndex list =
    let
        maybeElem =
            list
                |> List.getAt startIndex

        setElemToNewPosition xs =
            maybeElem
                |> Maybe.map (\x -> insertAt destIndex x xs)
                |> Maybe.withDefault xs
    in
        if destIndex >= List.length list then
            list
        else
            list
                |> List.removeAt startIndex
                |> setElemToNewPosition


insertAt : Int -> a -> List a -> List a
insertAt index elem list =
    if index == 0 then
        elem :: list
    else
        case list of
            [] ->
                []

            head :: tail ->
                head :: insertAt (index - 1) elem tail


updateList : (data -> String) -> String -> Maybe String -> List data -> List data
updateList toId overId dragging list =
    let
        draggedId =
            dragging
                |> Maybe.withDefault ""

        indexEqualTo id =
            list
                |> List.findIndex (\a -> toId a == id)
                |> Maybe.withDefault 0

        overIndex =
            indexEqualTo overId

        draggedIndex =
            indexEqualTo draggedId
    in
        list
            |> moveTo draggedIndex overIndex
