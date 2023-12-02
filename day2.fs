module aoc_2023.day2

open System.IO
open FParsec
open aoc_2022

type Color =
    | Red
    | Green
    | Blue

let pColor =
    (pstring "red" >>. preturn Red)
    <|>
    (pstring "green" >>. preturn Green)
    <|>
    (pstring "blue" >>. preturn Blue)

type ColorCounts = Map<Color, int64>

let pColorCount =
    (tuple3 pint64 ParsingUtils.space pColor)
    |>> fun (count, _, color) -> Map.add color count Map.empty

let mergeColorCounts (maps: ColorCounts list) =
    let tuplesToSum t =
        Seq.map snd t
        |> Seq.sum
    maps
    |> Seq.collect Map.toSeq
    |> Seq.groupBy fst
    |> Seq.map (fun (color, vals) -> (color, tuplesToSum vals))
    |> Map.ofSeq

let pColorCounts =
    let pSeparator = pstring ", "

    sepBy pColorCount pSeparator |>> mergeColorCounts

let pGameHeader = pstring "Game " >>. pint32 .>> pstring ": "

type Game = {
    ID: int
    Subsets: ColorCounts list
}
let pGame =
    let pSeparator = pstring "; "

    let pRounds = sepBy pColorCounts pSeparator

    tuple2 pGameHeader pRounds |>> fun (gameId, rounds) -> { ID = gameId; Subsets = rounds }

let parseGame line =
    ParsingUtils.runParser pGame line

let isValidPart1Round round =
    let getOrZero key =
        Map.tryFind key round
        |> Option.defaultValue 0L
    let redCount = getOrZero Red
    let greenCount = getOrZero Green
    let blueCount = getOrZero Blue

    redCount <= 12l && greenCount <= 13l && blueCount <= 14l

let isValidPart1Game game =
    let isValid =
        game.Subsets
        |> Seq.forall isValidPart1Round

    isValid

let part1core fileLines =

    let validGames =
        fileLines
        |> Seq.map parseGame
        |> Seq.filter isValidPart1Game

    validGames
    |> Seq.map (fun x -> x.ID)
    |> Seq.sum
    |> sprintf "%i"

let part1 =
    let fileLines = File.ReadAllLines "./day2.txt"

    part1core fileLines

let gamePower game =
    let groupMax (_, tuples) =
        Seq.map snd tuples
        |> Seq.max

    game.Subsets
    |> Seq.collect Map.toSeq
    |> Seq.groupBy fst
    |> Seq.map groupMax
    |> Seq.reduce (fun x y -> x * y)

let part2 =
    let fileLines = File.ReadAllLines "./day2.txt"

    fileLines
    |> Seq.map parseGame
    |> Seq.map gamePower
    |> Seq.sum
    |> sprintf "%i"
