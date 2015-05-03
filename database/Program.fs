namespace Lcdg.Database

open Microsoft.FSharp.Data.TypeProviders
open System.Data.Linq
open FSharp.Data

module Main =
    type Words = SqlDataConnection<Config.dbString>
    type Data = JsonProvider<"data/template.json">

    [<EntryPoint>]
    let main _ =
        let db = Words.GetDataContext ()
        let data = Data.Load "../../data/data.json"
        let value = ref 0
        
        let mutable (wordList: Words.ServiceTypes.Words list) = []

        for word in data.Words do
            let newEntries = Array.concat [
                                              [|word.Word|]
                                              word.Forms
                                              word.Synonyms |> Array.filter (fun item -> item.Split ' ' |> Array.length = 1)
                                          ] 
                                          |> set
            
            let existingEntries = wordList |> List.filter (fun item -> item.Type = word.Type)
                                           |> List.map (fun item -> item.Word)
                                           |> set
            
            let intersection = Set.intersect newEntries existingEntries

            let newValue = match Set.count intersection with
                           | 0 -> value := !value + 1
                                  !value
                           | _ -> (wordList |> (List.find (fun item -> item.Word = Set.minElement intersection && item.Type = word.Type))).Value
            
            let entries = newEntries - intersection
                          |> Set.toList
                          |> List.map (fun item -> new Words.ServiceTypes.Words (Type = word.Type, Word = item, Value = newValue))
                          
            wordList <- List.concat [wordList; entries]
            printfn "%s" word.Word
        
        wordList |> db.Words.InsertAllOnSubmit
        db.DataContext.SubmitChanges ()
        0
