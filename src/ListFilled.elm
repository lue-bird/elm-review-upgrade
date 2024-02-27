module ListFilled exposing (ListFilled, attach, head, tail, toList)


type alias ListFilled element =
    ( element, List element )


head : ListFilled a -> a
head =
    \( head_, _ ) -> head_


tail : ListFilled a -> List a
tail =
    \( _, tail_ ) -> tail_


attach : List a -> (ListFilled a -> ListFilled a)
attach attachment =
    \baseListFilled ->
        ( baseListFilled |> head
        , (baseListFilled |> tail) ++ attachment
        )


toList : ListFilled a -> List a
toList =
    \listFilled -> (listFilled |> head) :: (listFilled |> tail)
