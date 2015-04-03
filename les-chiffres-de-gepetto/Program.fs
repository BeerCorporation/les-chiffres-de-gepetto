namespace Lcdg

module Main =
    let trashWords = [
        "combien"
        "quel"
        "quelle"
        "est"
        "t'il"
        "t-il"
        "a-t-il"
        "aux"
        "qui"
        "des"
        "une"
        "les"
    ]

    let filterQuestion (question: string) =
        question.Split ' '
        |> Array.filter (fun word -> word.Length > 2 || word.Length > 1 && word.ToUpper().Equals word)
        |> Array.filter (fun word -> not (List.exists (fun item -> word.ToLower().Equals item) trashWords))
        |> Array.toList

    let getScore ids =
        ids |> Seq.toList
            |> List.map (fun item -> 75 + item % 11)
            |> List.averageBy (fun item -> float item)
            |> int

    [<EntryPoint>]
    let main _ = 
        while true do
            printfn "Posez votre question !"
            System.Console.ReadLine ()
            |> filterQuestion
            |> Seq.distinct
            |> Seq.map (fun item -> item |> Synonym.FindWord)
            |> Async.Parallel
            |> Async.RunSynchronously
            |> Seq.distinct
            |> Seq.map (fun item -> item |> Synonym.FindSynonyms)             
            |> Async.Parallel
            |> Async.RunSynchronously
            |> Seq.map (fun item -> item |> Seq.min |> fst)
            |> getScore
            |> printfn "%d%%\n"
        0