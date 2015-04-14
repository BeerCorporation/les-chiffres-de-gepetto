open FSharp.Data

type XmlData = XmlProvider<"Template.xml">

let outFile = new System.IO.StreamWriter "data.txt"

[<EntryPoint>]
let main _ = 
    printfn "Parsing data..."
    let rawXmlData = XmlData.Load "../../Morphalou.xml"
    
    printfn "Printing results..."
    rawXmlData.LexicalEntries 
    |> Seq.map (fun item -> 
        List.concat [
                [item.FormSet.LemmatizedForm.GrammaticalCategory]
                [item.FormSet.LemmatizedForm.Orthography]
                [
                    for inflected in item.FormSet.InflectedForms ->
                        inflected.Orthography
                ]
            ]
        )
    |> Seq.map (fun item -> item |> Seq.distinct)
    |> Seq.map (fun item -> String.concat " | " item)
    |> Seq.iter (fun item -> item |> outFile.WriteLine)
    outFile.Close ()
    0
