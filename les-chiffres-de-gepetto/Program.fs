namespace Lcdg

open System
open System.Data
open System.Data.Linq
open System.Net
open System.Text.RegularExpressions
open Microsoft.FSharp.Data.TypeProviders
open Microsoft.FSharp.Linq
open FSharp.Data

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
    
    type dbSchema = SqlDataConnection<Config.dbString>
    let db = dbSchema.GetDataContext()

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
        Console.ReadKey() |> ignore
        0

    let rec combinations acc size set = seq {
        match size, set with 
        | n, x::xs -> 
            if n > 0 then yield! combinations (x::acc) (n - 1) xs
            if n >= 0 then yield! combinations acc n xs 
        | 0, [] -> yield acc 
        | _, [] -> () 
    }

    let combinations2 = combinations [] 2

    // Returns all synonyms for a given word, using the exisiting database and Wiktionary
    let findSynonyms word =
        printfn ""
        // First, let's gather all words from the database
        let previousWordsInDb = query {
            for row in db.Words do
                select row
        }

        // Then, let's try to find our word in it
        let previousWordInDb = previousWordsInDb |> Seq.tryFind (fun item -> item.Word = word)
    
        // The actual word object of the database
        let wordInDbOption =
            match previousWordInDb with
            | None ->
                // If the word doesn't exist in the database, let's check if it exists
                try
                    Http.RequestString("http://fr.wiktionary.org/w/index.php", query = [("action", "raw"); ("title", WebUtility.UrlEncode(word))]) |> ignore
                    // If we reached that point, the word is valid, so let's go and add it !
                    let newId = 
                        if Seq.isEmpty previousWordsInDb then
                            1
                        else
                            (Seq.last previousWordsInDb).Id + 1
                    let newWordInDb = new dbSchema.ServiceTypes.Words(Id = newId, Word = word, HasBeenChecked = false)
                    db.Words.InsertOnSubmit(newWordInDb)
                    db.DataContext.SubmitChanges()
                    Some(newWordInDb)
                with
                | :? WebException as webEx when (webEx.Response :? HttpWebResponse) ->
                    printfn "Le mot %s n'existe pas tdb !" word
                    None
            | Some x -> previousWordInDb // Or else it's simply the one we found earlier

        match wordInDbOption with
        | None          -> Seq.empty
        | Some wordInDb ->
            // The complete collection of words in the database
            let wordsInDb = 
                match previousWordInDb with
                | None   -> previousWordsInDb |> Seq.append [wordInDb]
                | Some x -> previousWordsInDb :> seq<dbSchema.ServiceTypes.Words>
   
            // Okay, let's find the synonyms of our word now
            let existingSynonyms =
                // Let's find our synonyms in the database 
                Seq.concat [ 
                    (query {
                        for relation in db.Relations do
                            where (relation.Word1 = wordInDb.Id)
                            join word in db.Words on (relation.Word2 = word.Id)
                            select word
                    })
                    (query {
                        for relation in db.Relations do
                            where (relation.Word2 = wordInDb.Id)
                            join word in db.Words on (relation.Word1 = word.Id)
                            select word
                    })
                ] |> Seq.map (fun item -> item.Word)

            // Then, we check if we already checked its synonyms online
            if wordInDb.HasBeenChecked then
                existingSynonyms 
                |> Seq.append [word]
                |> Seq.sort
            else
                query {
                    for row in db.Words do
                        where (row.Word = word)
                        select row
                } |> Seq.iter (fun row -> row.HasBeenChecked <- true)
                // If not, let's search online for new synonyms
                let raw = Http.RequestString("http://fr.wiktionary.org/w/index.php", query = [("action", "raw"); ("title", WebUtility.UrlEncode(word))]).Split('\n')
                          |> Array.toSeq
                let synStart = Seq.tryFindIndex (fun elem -> elem.Equals "==== {{S|synonymes}} ====") raw
                match synStart with
                | Some synStart ->
                    let rawSkipped = raw |> Seq.skip (synStart + 1)
                    let synEnd = rawSkipped |> Seq.findIndex (fun elem -> elem.Equals "") 
                    let newWords = rawSkipped |> Seq.take synEnd
                                              |> Seq.filter (fun item -> item.[2] = '[')
                                              |> Seq.map (fun item -> Regex.Match(item, "(?<=\[\[)(.*?)(?=\])").Value)
                                              |> Seq.distinct
                                              |> Seq.filter (fun item -> not (wordsInDb |> Seq.exists (fun elem -> elem.Word = item)))
                    if Seq.isEmpty newWords then
                        existingSynonyms 
                        |> Seq.append [word]
                        |> Seq.sort
                    else
                        let wordId = (Seq.last wordsInDb).Id + 1
                        newWords |> Seq.map (fun item -> new dbSchema.ServiceTypes.Words(Id = wordId + (newWords |> Seq.findIndex ((=) item)), Word = item))
                                 |> db.Words.InsertAllOnSubmit
                        newWords |> Seq.map (fun item -> new dbSchema.ServiceTypes.Relations(Word1 = wordInDb.Id, Word2 = wordId + (newWords |> Seq.findIndex ((=) item))))
                                 |> db.Relations.InsertAllOnSubmit

                        db.DataContext.SubmitChanges()

                        existingSynonyms
                        |> Seq.append [word]
                        |> Seq.append newWords
                        |> Seq.sort
                | None -> existingSynonyms 
                          |> Seq.append [word]
                          |> Seq.sort

    [<EntryPoint>]
    let testHttp _ =
        while true do
            printfn "Mot ?"
            Console.ReadLine() 
            |> findSynonyms 
            |> Seq.iter (fun item -> printfn "%s" item)
            printfn ""
        0