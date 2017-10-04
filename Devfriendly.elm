port module Devfriendly exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder, field, list)
import Json.Decode.Pipeline as Pipe exposing (decode, required, optional, optionalAt)
import Http
import Navigation


-- PORTS


port moveMap : Town -> Cmd msg


port addPlaces : List Place -> Cmd msg



-- MODEL


type alias Model =
    { towns : List Town
    , places : List Place
    , selectedTown : TownSlug
    }


type alias Place =
    { name : String
    , latitude : Float
    , longitude : Float
    , address : String
    , url : String
    , openHours : String
    , comment : String
    , wifi : Bool
    , power : Bool
    }



-- UPDATE


type Msg
    = TownOnChange String
    | GetTowns (Result Http.Error (List Town))
    | GetPlaces (Result Http.Error (List Place))
    | UrlChange Navigation.Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            let
                townSlug =
                    urlToTownSlug location

                town =
                    findTown townSlug model.towns
            in
                case town of
                    Just town ->
                        ( { model | selectedTown = townSlug }
                        , moveMap town
                        )

                    Nothing ->
                        let
                            _ =
                                Debug.log "Not a town" townSlug
                        in
                            ( model, Cmd.none )

        TownOnChange townName ->
            let
                hash =
                    "#" ++ slugifyTownName townName
            in
                ( model, Navigation.newUrl hash )

        GetTowns (Ok towns) ->
            let
                hash =
                    "#" ++ model.selectedTown
            in
                ( { model | towns = towns }, Navigation.newUrl hash )

        GetTowns (Err error) ->
            let
                _ =
                    Debug.log "Get Towns Failed" error
            in
                ( { model | towns = [] }, Cmd.none )

        GetPlaces (Ok places) ->
            ( { model
                | places = places ++ model.places
              }
            , addPlaces places
            )

        GetPlaces (Err error) ->
            let
                _ =
                    Debug.log "Get Towns Failed" error
            in
                ( { model | places = [] }, Cmd.none )



-- VIEW


onChange : (String -> msg) -> Html.Attribute msg
onChange tagger =
    on "change" (Decode.map tagger Html.Events.targetValue)


viewMenu : Model -> Html Msg
viewMenu model =
    let
        townsOption =
            List.map
                (\town ->
                    option
                        [ selected (model.selectedTown == (slugifyTownName town.name)) ]
                        [ text town.name ]
                )
                (List.sortBy .name model.towns)
    in
        select [ id "towns", onChange TownOnChange ] townsOption


view : Model -> Html Msg
view model =
    viewMenu model



-- Commands


loadTowns : String -> Cmd Msg
loadTowns url =
    Decode.list townDecoder
        |> Http.get url
        |> Http.send GetTowns


loadPlaces : String -> Cmd Msg
loadPlaces url =
    Decode.list placeDecoder
        |> Http.get url
        |> Http.send GetPlaces


cmdsDisplayTown : Town -> List (Cmd Msg)
cmdsDisplayTown town =
    [ moveMap town ]



-- Decoder


townDecoder : Decoder Town
townDecoder =
    Pipe.decode Town
        |> Pipe.required "name" Decode.string
        |> Pipe.required "lat" Decode.float
        |> Pipe.required "lon" Decode.float
        |> Pipe.required "defaultZoom" Decode.int


placeDecoder : Decoder Place
placeDecoder =
    Pipe.decode Place
        |> Pipe.required "name" Decode.string
        |> Pipe.required "lat" Decode.float
        |> Pipe.required "lon" Decode.float
        |> Pipe.optional "address" Decode.string ("")
        |> Pipe.optional "url" Decode.string ("")
        |> Pipe.optional "openHours" Decode.string ("")
        |> Pipe.optional "comment" Decode.string ("")
        |> Pipe.optionalAt [ "wifi", "available" ] Decode.bool False
        |> Pipe.optionalAt [ "power", "available" ] Decode.bool False


placesDecode : String -> List Place
placesDecode jsonPlaces =
    let
        result =
            Decode.decodeString (Decode.list placeDecoder) jsonPlaces
    in
        case result of
            Ok places ->
                places

            Err error ->
                []



-- Towns


type alias TownSlug =
    String


type alias Town =
    { name : String
    , latitude : Float
    , longitude : Float
    , defaultZoom : Int
    }


slugifyTownName : String -> TownSlug
slugifyTownName town =
    town
        |> String.toLower
        |> String.map
            (\c ->
                case c of
                    ' ' ->
                        '-'

                    'é' ->
                        'e'

                    'è' ->
                        'e'

                    'à' ->
                        'a'

                    _ ->
                        c
            )


urlToTownSlug : Navigation.Location -> TownSlug
urlToTownSlug location =
    case location.hash of
        "" ->
            defaultTown

        hash ->
            String.dropLeft 1 hash


findTown : TownSlug -> List Town -> Maybe Town
findTown townSlug towns =
    towns
        |> List.filter (\t -> (slugifyTownName t.name) == townSlug)
        |> List.head



-- MAIN


baseUrl : String
baseUrl =
    "https://raw.githubusercontent.com/devfriendlyplaces/data/master/locations/"


defaultTown : TownSlug
defaultTown =
    "toulouse"


townsUrl : String
townsUrl =
    baseUrl ++ "locations.json"


placesUrl : String
placesUrl =
    "/" ++ "all_places.json"


placesUrlFor : TownSlug -> String
placesUrlFor townSlug =
    baseUrl ++ townSlug ++ ".json"


main : Program Never Model Msg
main =
    let
        initialModel location =
            let
                townSlug =
                    urlToTownSlug location
            in
                ( { towns = []
                  , places = []
                  , selectedTown = townSlug
                  }
                , Cmd.batch [ loadTowns townsUrl, loadPlaces placesUrl ]
                )
    in
        Navigation.program UrlChange
            { init = initialModel
            , view = view
            , update = update
            , subscriptions = \_ -> Sub.none
            }
