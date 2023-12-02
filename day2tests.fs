module aoc_2023.day2tests

open Xunit
open FsUnit.Xunit

let sampleInput =
    [
        "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green";
        "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue";
        "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red";
        "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red";
        "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green";
    ]

[<Fact>]
let ``part1 example``() =
    day2.part1core sampleInput |> should equal "8"

[<Fact>]
let ``game 81 validity``() =
    let game81Text = "Game 81: 11 green, 5 red; 7 green, 14 blue, 4 red; 7 red, 8 blue, 2 green; 10 red, 3 green, 18 blue; 3 red, 1 green"

    let game81 = day2.parseGame game81Text

    day2.isValidPart1Game game81 |> should equal false
