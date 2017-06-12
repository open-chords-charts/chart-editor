module Music.Chart exposing (..)

import List.Extra as List
import Music.Chord as Chord exposing (Chord)
import Music.Note as Note exposing (Note(..), Interval)


-- TYPES


type alias Chart =
    { title : String
    , key : Note
    , parts : List Part
    }


type Part
    = Part PartName (List Bar)
    | PartRepeat PartName


mapPart : (List Bar -> List Bar) -> Part -> Part
mapPart f part =
    case part of
        Part partName bars ->
            Part partName (f bars)

        PartRepeat _ ->
            part


mapPartBars : (Bar -> Bar) -> Part -> Part
mapPartBars f =
    mapPart (List.map f)


type alias PartIndex =
    Int


type alias PartName =
    String


isPartRepeat : Part -> Bool
isPartRepeat part =
    case part of
        Part _ _ ->
            False

        PartRepeat _ ->
            True


getBarsOfPartByIndex : PartIndex -> Chart -> List Bar
getBarsOfPartByIndex partIndex chart =
    case List.getAt partIndex chart.parts of
        Nothing ->
            []

        Just part ->
            case part of
                Part _ bars ->
                    bars

                PartRepeat _ ->
                    []


getPartByName : PartName -> Chart -> Maybe Part
getPartByName partName chart =
    chart.parts
        |> List.find
            (\part ->
                case part of
                    Part partName1 _ ->
                        partName == partName1

                    PartRepeat _ ->
                        False
            )


getPartName : Part -> PartName
getPartName part =
    case part of
        Part partName _ ->
            partName

        PartRepeat partName ->
            partName


type Bar
    = Bar (List Chord)
    | BarRepeat


mapBar : (List Chord -> List Chord) -> Bar -> Bar
mapBar f bar =
    case bar of
        Bar chords ->
            Bar (f chords)

        BarRepeat ->
            bar


mapBarChords : (Chord -> Chord) -> Bar -> Bar
mapBarChords f =
    mapBar (List.map f)



-- TRANSPOSITION FUNCTIONS


transpose : Note -> Chart -> Chart
transpose key chart =
    let
        interval =
            Note.interval chart.key key

        newParts =
            chart.parts |> List.map (transposePart interval)
    in
        { chart
            | key = Note.transpose interval key
            , parts = newParts
        }


transposePart : Interval -> Part -> Part
transposePart interval part =
    part |> mapPartBars (transposeBar interval)


transposeBar : Interval -> Bar -> Bar
transposeBar interval bar =
    bar |> mapBarChords (Chord.transpose interval)



-- TO STRING FUNCTIONS


barToString : Bar -> String
barToString bar =
    case bar of
        Bar chords ->
            chords
                |> List.map Chord.toString
                |> String.join "/"

        BarRepeat ->
            "-"


partToString : Part -> String
partToString part =
    let
        partNameToString partName =
            "= " ++ partName

        partAsString =
            case part of
                Part partName bars ->
                    [ partNameToString partName
                    , bars
                        |> List.map barToString
                        |> String.join " "
                    ]
                        |> String.join "\n"

                PartRepeat partName ->
                    partNameToString partName
    in
        partAsString ++ "\n"


toString : Chart -> String
toString chart =
    let
        dashes =
            "---"

        metadata =
            [ "title: " ++ chart.title
            , "key: " ++ Note.toString chart.key
            ]
                |> String.join "\n"

        header =
            [ dashes, metadata, dashes ]
                |> String.join "\n"
    in
        [ header
        , ""
        , chart.parts
            |> List.map partToString
            |> String.join "\n"
        ]
            |> String.join "\n"
