open FSharp.Data
open System.IO
open System.Text.RegularExpressions

// type XmlData = XmlProvider<"Template.xml">
// type XmlDefinition = XmlProvider<"Definition.xml">
type JsonData = JsonProvider<"template.json">

let readLines (filePath: string) = [
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
    ]

[<EntryPoint>]
let main _ = 
    use output = new StreamWriter "data.json"
    let formLines = readLines "../../data.txt"
    let formData = formLines |> List.map (fun item -> Regex.Split(item, " \| "))

    let jsonData = JsonData.Load "../../data.json"
    let data = jsonData.Words |> Array.map (fun item ->
        let forms = match formData |> List.tryFind (fun word -> word.[0] = item.Type && word.[1] = item.Word) with
                    | Some x -> x
                    | None -> formData |> List.find (fun word -> word.[1] = item.Word && word.[0] <> "verbe")
                     
        printfn "%s" item.Word
        new JsonData.Word (word = item.Word, ``type`` = item.Type, forms = forms.[2..], synonyms = item.Synonyms)
    )
    
    let newData = new JsonData.Root(words = data)
    output.Write (sprintf "%A" newData)

    
    
    (*
    let xmlData = new DirectoryInfo "../../data"
    let files = xmlData.GetFiles ()
    
    let (outputList: JsonData.Root list) = [
        for file in files do
            let definition = XmlDefinition.Load (file.OpenText ())
            printfn "%s" definition.Definition.Titre.Entree
            let synonyms = seq {
                for sens in definition.Synonymes.Sens do
                    for synonym in sens.Synonymes do
                        yield synonym.Value
            }
            yield new JsonData.Root(definition.Definition.Titre.Entree, definition.Definition.Titre.Categ, [||], synonyms |> Seq.distinct |> Seq.toArray)
    ]

    use output = new StreamWriter "../../data.json"
    output.WriteLine "["
    outputList 
    |> List.sortBy (fun item -> item.Word)  
    |> List.iter (fun item -> 
        output.Write item
        output.WriteLine ",")
    output.Write "]"
    *)
    (*
    use output = new StreamWriter "data.txt"
    let original = readLines "../../data.txt"
    let notfound = readLines "../../notfound.txt"

    original
    |> Seq.filter (fun item -> not <| Seq.exists ((=) (Regex.Split(item, " \| ")).[1]) notfound)
    |> Seq.iter (fun item -> output.WriteLine item)
    *)
    
    // let notFound = new System.IO.StreamWriter "notfound.txt"
    // printfn "Parsing data..."
    // let rawXmlData = XmlData.Load "../../Morphalou.xml"
    // let data = readLines "../../data.txt"
    // for word in rawXmlData.LexicalEntries do
    // for str in data do
        // let word = word.FormSet.LemmatizedForm.Orthography
        (*
        let word = Regex.Split(str, " \| ").[1]
        try
            let rawDefinition = Http.RequestString("http://dictionnaire.cordial-enligne.fr/DictionnaireXml/" + word + "-verbe.xml")
            use outFile = new StreamWriter ("..\\..\\data\\" + word + "-verb.xml")
            printfn "%s verb found!!!!" word
            outFile.Write rawDefinition
        with
        | :? System.Net.WebException -> printfn "%s not found" word
        *)
    //notFound.Close ()
       
    (*
    let outFile = new StreamWriter "data.txt"
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
    outFile.Close () *)
    0
