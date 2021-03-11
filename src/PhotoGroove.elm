module PhotoGroove exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser
import Array exposing (Array)


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [text "Photo Groove" ]
        , button
          [ onClick { description = "ClickedSurpriseMe", data = "" } ]
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
      , onClick { description = "ClickedPhoto", data = thumb.url }
      ] []

viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
      [ input [ type_ "radio", name "size" ] []
      , text (sizeToString size)
      ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small  -> "small"
        Medium -> "med"
        Large  -> "large"

type alias Msg =
    { description: String
    , data: String
    }

type ThumbnailSize
    = Small
    | Medium
    | Large
    
update msg model =
    case msg.description of
        "ClickedPhoto"      -> { model | selectedUrl = msg.data }
        "ClickedSurpriseMe" -> { model | selectedUrl = "2.jpeg" }
        _                   -> model

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

main =
    Browser.sandbox
      { init = initialModel
      , view = view
      , update = update
      }
    
