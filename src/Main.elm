module Main exposing (Model(..), Msg(..), getPostBody, init, main, subscriptions, update, view)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Failure
    | Loading
    | Success (Dict.Dict String PostBody)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getPostBody )



-- UPDATE


type Msg
    = MorePlease
    | GotPostBody (Result Http.Error (Dict.Dict String PostBody))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( Loading, getPostBody )

        GotPostBody result ->
            case result of
                Ok bodies ->
                    ( Success bodies, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Firebase Realtime Database Viewer" ]
        , viewPostBody model
        ]


viewPostBody : Model -> Html Msg
viewPostBody model =
    case model of
        Failure ->
            div []
                [ text "Could not get json. "
                , button [ onClick MorePlease ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading..."

        Success bodies ->
            div []
                [ button [ onClick MorePlease, style "display" "block" ] [ text "More Please!" ]
                , table [ style "border" "solid thin" ]
                    (tr []
                        [ th [ style "border" "solid thin" ] [ text "foo" ]
                        , th [ style "border" "solid thin" ] [ text "bar" ]
                        , th [ style "border" "solid thin" ] [ text "maybeFoo" ]
                        , th [ style "border" "solid thin" ] [ text "maybeBar" ]
                        ]
                        :: List.map toTableRow (Dict.values bodies)
                    )
                ]


toTableRow : PostBody -> Html Msg
toTableRow body =
    tr []
        [ td [ style "border" "solid thin" ] [ text body.foo ]
        , td [ style "border" "solid thin" ] [ text <| String.fromInt body.bar ]
        , td [ style "border" "solid thin" ] [ text <| Maybe.withDefault "" body.maybeFoo ]
        , td [ style "border" "solid thin" ] [ text <| String.fromInt (Maybe.withDefault 0 body.maybeBar) ]
        ]



-- HTTP


type alias PostBody =
    { foo : String
    , bar : Int
    , maybeFoo : Maybe String
    , maybeBar : Maybe Int
    }


getPostBody : Cmd Msg
getPostBody =
    Http.get
        { url = "replace your firebase realtime database end point with secret"
        , expect = Http.expectJson GotPostBody postBodyDecoder
        }


postBodyDecoder : Decoder (Dict.Dict String PostBody)
postBodyDecoder =
    Json.Decode.map (Dict.map dbRecordToPostBody) (dict bodyDecoder)


bodyDecoder : Decoder PostBody
bodyDecoder =
    map4 PostBody
        (field "foo" string)
        (field "bar" int)
        (maybe (field "maybeFoo" string))
        (maybe (field "maybeBar" int))


dbRecordToPostBody : String -> PostBody -> PostBody
dbRecordToPostBody _ body =
    body
