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
        question.Split(' ')
        |> Array.filter (fun word -> word.Length > 2 || word.Length > 1 && word.ToUpper().Equals word)
        |> Array.filter (fun word -> not (List.exists (fun item -> word.ToLower().Equals item) trashWords))

    let main _ = 
        [
            "Combien y a t'il de gens qui votent FN aux départementales en Ile de France ?"
            "Quel est le pourcentage de gens qui regrettent de ne pas s'être foutu des caisses au collège ?"
            "Quel est le nombre de gens riches qui mangent de la merde pour se sentir prolétaire ?"
            "Quelle est proportion de gens qui sont plus stressés parce qu'ils utilisent Linux ?"
        ]
        |> List.map filterQuestion
        |> List.iter (printfn "%A") 
        System.Console.ReadKey() |> ignore
        0

    [<EntryPoint>]
    let testHttp _ =
        while true do
            printfn "Mot ?"
            System.Console.ReadLine() 
            |> Conjugation.getInfinitive
            |> printfn "%s"
            printfn ""
        0