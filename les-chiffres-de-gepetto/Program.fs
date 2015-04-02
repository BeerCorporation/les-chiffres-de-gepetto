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
        ids |> List.map (fun item -> 75 + item % 11)
            |> List.averageBy (fun item -> float item)
            |> int
    
    open FSharp.Data
    open System.Xml.Linq
    type Word = XmlProvider<"http://dictionnaire.cordial-enligne.fr/DictionnaireXml/manger.xml">

    [<EntryPoint>]
    let main _ = 
        let resp = Http.RequestString("http://dictionnaire.cordial-enligne.fr/php/search.php", body = FormValues ["mot", "mangent"])
        let xml = Http.RequestString("http://dictionnaire.cordial-enligne.fr/DictionnaireXml/" + resp)
                  |> Word.Parse
        printfn "%s" xml.Definition.Titre.Entree
        while true do
            System.Console.ReadLine ()
            |> filterQuestion
            |> List.map (fun item -> item |> Conjugation.getInfinitive
                                          |> Synonym.findSynonyms
                                          |> Seq.min
                                          |> fst)
            |> getScore
            |> printfn "%d%%\n"
        0