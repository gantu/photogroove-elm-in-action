module PhotoGroove exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser
import Array exposing (Array)
import Random

view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [text "Photo Groove" ]
        , button
          [ onClick ClickedSurpriseMe ]
          [ text "Surprise Me!" ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
          (List.map
            (renderThumbnail model.selectedUrl)
            model.photos
          )
        , img
          [ class "large"
          , src (urlPrefix ++ "large/" ++ model.selectedUrl)
          ]
          []
        ]

type alias Photo =
    { url: String }

type alias Model =
    { photos: List Photo
    , selectedUrl: String
    , chosenSize : ThumbnailSize
    }

urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"

photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos


renderThumbnail : String -> Photo -> Html Msg
renderThumbnail selectedUrl thumb =
    img
      [ src (urlPrefix ++ thumb.url)
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


getPhotoUrl : Int -> String
getPhotoUrl index =
    case Array.get index photoArray of
        Just photo -> photo.url
        Nothing    -> ""

randomPhotoPicker : Random.Generator Int
randomPhotoPicker =
    Random.int 0 (Array.length photoArray - 1)

    
type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotSelectexIndex Int

type ThumbnailSize
    = Small
    | Medium
    | Large

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ClickedPhoto url       -> ( { model | selectedUrl = url }, Cmd.none )
        ClickedSurpriseMe      -> ( model , Random.generate GotSelectexIndex randomPhotoPicker )
        ClickedSize size       -> ( { model | chosenSize = size}, Cmd.none )
        GotSelectexIndex index -> ( { model | selectedUrl = getPhotoUrl index }, Cmd.none )
        
initialModel : Model
initialModel =
    {
        photos =
            [ { url = "1.jpeg" }
            , { url = "2.jpeg" }
            , { url = "3.jpeg" }
            ]
    , selectedUrl = "1.jpeg"
    , chosenSize = Small
    }

main : Program () Model Msg
main =
    Browser.element
      { init = \flags -> (initialModel, Cmd.none)
      , update = update
      , view = view
      , subscriptions = \model -> Sub.none
      }
    
