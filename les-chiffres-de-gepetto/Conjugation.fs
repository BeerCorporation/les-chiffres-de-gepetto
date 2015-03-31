namespace Lcdg

open System
open FSharp.Data

module Conjugation =
    let getInfinitive verb =
        try
            HtmlDocument.Load("http://www.conjugaison.com/verbe/" + verb + ".html")
            |> HtmlDocument.descendantsNamed false ["h1"]
            |> Seq.map (fun item -> item.InnerText())
            |> Seq.head
        with
        | :? ArgumentException ->
            "Le mot " + verb + " n'existe pas tdb !"