module PhotoGroove exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser
import Random
import Http
import Json.Decode exposing (Decoder, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)

view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model.chosenSize

            Loading -> []

            Errored errorMessage -> [ text ("Error: " ++ errorMessage )]

viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize =
    [ h1 [] [ text "Photo Groove" ]
    , button
        [ onClick ClickedSurpriseMe ]
        [ text "Surprise Me!" ]
    , h3 [] [ text "Thumbnail Size:" ]
    , div [ id "choose-size" ]
        (List.map viewSizeChooser [ Small, Medium, Large ])
    , div [ id "thumbnails", class (sizeToString chosenSize) ]
        (List.map (renderThumbnail selectedUrl) photos)
    , img
        [ class "large"
        , src (urlPrefix ++ "large/" ++ selectedUrl)
        ]
        []
    ]

type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }

type alias Model =
    { status : Status
    , chosenSize : ThumbnailSize
    }


photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
      |> Json.Decode.Pipeline.required "url" string
      |> Json.Decode.Pipeline.required "size" int
      |> optional "title" string "(untitled)"

    
urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


renderThumbnail : String -> Photo -> Html Msg
renderThumbnail selectedUrl thumb =
    img
      [ src (urlPrefix ++ thumb.url)
      , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]")
      , classList [ ( "selected", selectedUrl == thumb.url ) ]
      , onClick (ClickedPhoto thumb.url)
      ] []

viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
      [ input [ type_ "radio", name "size", onClick (ClickedSize size)] []
      , text (sizeToString size)
      ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small  -> "small"
        Medium -> "med"
        Large  -> "large"

    
type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))

type ThumbnailSize
    = Small
    | Medium
    | Large

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ClickedPhoto url       -> ( { model | status = selectUrl url model.status }, Cmd.none )
        ClickedSurpriseMe      ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                      |> Random.generate GotRandomPhoto
                      |> Tuple.pair model
                      
                Loading -> ( model, Cmd.none )
                
                Loaded [] _ -> (model, Cmd.none)
                
                Errored errorMessage -> (model, Cmd.none)
                
        ClickedSize size       -> ( { model | chosenSize = size}, Cmd.none )
        GotRandomPhoto photo -> ( { model | status = selectUrl photo.url model.status }
                                  , Cmd.none )

        GotPhotos (Ok photos) ->
            case photos of
                first :: rest  ->
                    ({ model | status = Loaded photos first.url }, Cmd.none)
                []             -> ({ model | status = Errored "0 photos found" }, Cmd.none)
        GotPhotos (Err _)     -> ( {model | status = Errored "Unexpected Error" }, Cmd.none)

selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _      -> Loaded photos url
        Loading              -> status 
        Errored errorMessage -> status


initialCmd : Cmd Msg
initialCmd =
    Http.get
         { url = "http://elm-in-action.com/photos/list.json"
         , expect = Http.expectJson GotPhotos (Json.Decode.list photoDecoder)
         }
             
        
initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Small
    }

main : Program () Model Msg
main =
    Browser.element
      { init = \_ -> (initialModel, initialCmd)
      , update = update
      , view = view
      , subscriptions = \model -> Sub.none
      }
    
