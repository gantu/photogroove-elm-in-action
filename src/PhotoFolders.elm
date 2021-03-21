module PhotoFolders exposing (Model, Msg, init, update, view)

import Http
import Json.Decode as Decoder
  exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Browser
import Html exposing (..)
import Html.Attributes exposing (class, src, href)
import Html.Events exposing (onClick)
import Dict exposing (Dict)

type Folder =
    Folder
      { name : String
      , photoUrls : List String
      , subFolders : List Folder
      , expanded : Bool
      }

type alias Model =
    { selectedPhotoUrl : Maybe String
    , photos : Dict String Photo
    , root : Folder
    }


initialModel : Model
initialModel =
    { selectedPhotoUrl = Nothing
    , photos = Dict.empty
    , root = Folder
        { name= "Loading..."
        , photoUrls = []
        , subFolders = []
        , expanded = True
        }
    }


init : Maybe String -> ( Model, Cmd Msg )
init selectedFilename =
    ( { initialModel | selectedPhotoUrl = selectedFilename }
    , Http.get
        { url = "http://elm-in-action.com/folders/list"
        , expect =
            Http.expectJson GotInitialModel modelDecoder }
     )


modelDecoder : Decoder Model
modelDecoder =
    Decoder.map2
        (\photos root ->
            { photos = photos
            , root = root
            , selectedPhotoUrl = Nothing
            }
         )
         modelPhotosDecoder
         folderDecoder
     
type Msg
    = ClickedPhoto String
    | GotInitialModel (Result Http.Error Model)
    | ClickedFolder FolderPath


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | selectedPhotoUrl = Just url }
            , Cmd.none
            )

        GotInitialModel (Ok newModel) ->
            ({ newModel | selectedPhotoUrl = model.selectedPhotoUrl }, Cmd.none)

        GotInitialModel (Err _) ->
            ( model, Cmd.none)

        ClickedFolder path ->
            ( { model | root = toggleExpanded path model.root }, Cmd.none)


type alias Photo =
    { title : String
    , size : Int
    , relatedUrls : List String
    , url : String
    }

viewPhoto : String -> Html Msg
viewPhoto url =
    a [ href ("/photos/" ++ url), class "photo", onClick (ClickedPhoto url) ]
        [text url]
    
viewFolder : FolderPath -> Folder -> Html Msg
viewFolder path (Folder folder) =
    let
        viewSubFolder : Int -> Folder -> Html Msg
        viewSubFolder index subfolder =
            viewFolder (appendIndex index path) subfolder

        folderLabel =
            label [ onClick (ClickedFolder path) ] [ text folder.name ]
    in
    if folder.expanded then
        let
            contents =
                List.append
                (List.indexedMap viewSubFolder folder.subFolders)
                (List.map viewPhoto folder.photoUrls)
        in
        div [ class "folder expanded" ]
            [ folderLabel
            , div [ class "contents" ] contents
            ]
     else
         div [ class "folder collapsed" ] [ folderLabel ]


appendIndex : Int -> FolderPath -> FolderPath
appendIndex index path =
    case path of
        End -> Subfolder index End
        Subfolder subFolderIndex remainingPath ->
            Subfolder subFolderIndex (appendIndex index remainingPath)


viewSelectedPhoto : Photo -> Html Msg
viewSelectedPhoto photo =
    div
      [ class "selected-photo" ]
      [ h2 [] [ text photo.title ]
      , img [ src (urlPrefix ++ "photos/" ++ photo.url ++
          "/full" ) ] []
      , span []
          [ text (String.fromInt photo.size ++ "KB") ]
      , h3 [] [ text "Related" ]
      , div [ class "related-photos" ]
          (List.map viewRelatedPhoto photo.relatedUrls)
      ]


viewRelatedPhoto : String -> Html Msg
viewRelatedPhoto url =
    img
      [ class "related-photo"
      , onClick (ClickedPhoto url)
      , src (urlPrefix ++ "photos/" ++ url ++ "/thumb")
      ]
      []

urlPrefix : String
urlPrefix = "http://elm-in-action.com/"

type FolderPath
    = End
    | Subfolder Int FolderPath


toggleExpanded : FolderPath -> Folder -> Folder
toggleExpanded path (Folder folder) =
    case path of
        End ->
            Folder { folder | expanded = not folder.expanded}

        Subfolder targetIndex remainingPath ->
            let
                subfolders : List Folder
                subfolders =
                    List.indexedMap transform folder.subFolders

                transform : Int -> Folder -> Folder
                transform currentIndex currentSubFolder =
                    if currentIndex == targetIndex then
                        toggleExpanded remainingPath currentSubFolder
                    else
                        currentSubFolder
             in
             Folder { folder | subFolders = subfolders}
                
type alias JsonPhoto =
    { title : String
    , size : Int
    , relatedUrls : List String
    }


jsonPhotoDecoder : Decoder JsonPhoto
jsonPhotoDecoder =
    Decoder.succeed JsonPhoto
      |> required "title" string
      |> required "size" int
      |> required "related_photos" (list string)


finishPhoto : ( String, JsonPhoto ) -> ( String, Photo )
finishPhoto ( url, json ) =
    ( url
    , { url = url
      , size = json.size
      , title = json.title
      , relatedUrls = json.relatedUrls
      }
    )


fromPairs : List ( String, JsonPhoto) -> Dict String Photo
fromPairs pairs =
    pairs
      |> List.map finishPhoto
      |> Dict.fromList


photosDecoder : Decoder (Dict String Photo)
photosDecoder =
    Decoder.keyValuePairs jsonPhotoDecoder
        |> Decoder.map fromPairs


modelPhotosDecoder : Decoder (Dict String Photo)
modelPhotosDecoder =
    Decoder.succeed modelPhotosFromJson
        |> required "photos" photosDecoder
        |> required "subfolders" (Decoder.lazy (\_ ->
            list modelPhotosDecoder))


modelPhotosFromJson :
    Dict String Photo
    -> List (Dict String Photo)
    -> Dict String Photo
modelPhotosFromJson folderPhotos subFolderPhotos =
    List.foldl Dict.union folderPhotos subFolderPhotos
    

folderDecoder : Decoder Folder
folderDecoder =
    Decoder.succeed folderFromJson
        |> required "name" string
        |> required "photos" photosDecoder
        |> required "subfolders" (Decoder.lazy (\_ ->
            list folderDecoder))


folderFromJson :
    String -> Dict String Photo -> List Folder -> Folder
folderFromJson name photos subfolders =
    Folder
        { name = name
        , expanded = True
        , subFolders = subfolders
        , photoUrls = Dict.keys photos
        }
        
view : Model -> Html Msg
view model =
    let
        photoByUrl : String -> Maybe Photo
        photoByUrl url = Dict.get url model.photos

        selectedPhoto : Html Msg
        selectedPhoto =
            case Maybe.andThen photoByUrl model.selectedPhotoUrl of
                Just photo -> viewSelectedPhoto photo
                Nothing -> text ""
     in
     div [ class "content"]
         [ div [ class "folders" ]
           [ viewFolder End model.root
           ]
         , div [ class "selected-photo" ] [ selectedPhoto]
         ]
