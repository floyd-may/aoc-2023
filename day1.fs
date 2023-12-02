module aoc_2023.day1

open System.IO
open FParsec
open aoc_2022

let firstlast digits =
    [Seq.head digits; Seq.last digits]
    |> Array.ofList

let part1CalibrationValueFromLine line =
    line
    :> char seq
    |> Seq.filter System.Char.IsDigit
    |> firstlast
    |> (fun x -> new string(x))
    |> System.Int32.Parse

let stringsToChars = [
    ("one", '1');
    ("two", '2');
    ("three", '3');
    ("four", '4');
    ("five", '5');
    ("six", '6');
    ("seven", '7');
    ("eight", '8');
    ("nine", '9');
]

let reverseString str =
    Seq.rev str
    |> Array.ofSeq
    |> (fun x -> new string(x))

let reverseStringsToChars =
    stringsToChars
    |> List.map (fun (str, c) -> (reverseString str, c))

let parseFirstDigit digitMap line =
    let pDigitOption =
        digitMap
        |> List.map (fun pair -> pstring (fst pair) >>. preturn (snd pair))
        |> choice
        <|> digit
        |>> Some

    let lineParser = many (pDigitOption <|> (anyChar >>. preturn None))

    ParsingUtils.runParser lineParser line
    |> Seq.filter Option.isSome
    |> Seq.head
    |> Option.get

let parsePartTwoLine line =
    let reverseLine = reverseString line

    let result =
        [|
            parseFirstDigit stringsToChars line
            parseFirstDigit reverseStringsToChars reverseLine;
        |]
        |> (fun x -> new string(x))
        |> System.Int64.Parse

    result

let part1 =
    let fileLines = File.ReadAllLines "./day1.txt"

    let calibrationValues =
        fileLines
        |> Seq.filter (fun x -> System.String.IsNullOrEmpty(x) |> not)
        |> Seq.map part1CalibrationValueFromLine

    calibrationValues
    |> Seq.sum
    |> sprintf "%i"
let part2 =
    let fileLines = File.ReadAllLines "./day1.txt"

    let calibrationValues =
        fileLines
        |> Seq.filter (fun x -> System.String.IsNullOrEmpty(x) |> not)
        |> Seq.map parsePartTwoLine

    calibrationValues
    |> Seq.sum
    |> sprintf "%i"
