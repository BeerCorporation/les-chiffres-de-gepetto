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
        let id = ref 0

        data.Words |> Array.map (fun item ->
                         id := !id + 1
                         let newWord = [new Words.ServiceTypes.Words (Id = !id, Type = item.Type, Word = item.Word, Value = !id)]
                         let value = !id
                         let forms = [
                            for form in item.Forms do
                                id := !id + 1
                                yield new Words.ServiceTypes.Words (Id = !id, Type = item.Type, Word = form, Value = value)
                            ]
                         printfn "%s" item.Word
                         List.concat [newWord; forms]
                      )
                   |> List.concat
                   |> db.Words.InsertAllOnSubmit
                   
        db.DataContext.SubmitChanges ()
        0
