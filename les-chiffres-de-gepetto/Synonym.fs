namespace Lcdg

open System
open System.Data
open System.Data.Linq
open Microsoft.FSharp.Data.TypeProviders
open Microsoft.FSharp.Linq
open FSharp.Data

module Synonym =
    type private XmlWord = XmlProvider<"http://dictionnaire.cordial-enligne.fr/DictionnaireXml/manger.xml">
    type private SynonymDb = SqlDataConnection<Config.dbString>

    // DB reads to add new rows to the Words table needs to be protected, or else we can have duplicate keys
    let private dbMutex = new System.Threading.Mutex ()

    ///////////////
    // UTILITIES //
    ///////////////

    // Outputs the unique combinations of size size from the given collection
    let rec private combinations acc size set = seq {
        match size, set with 
        | n, x::xs -> 
            if n > 0 then yield! combinations (x::acc) (n - 1) xs
            if n >= 0 then yield! combinations acc n xs 
        | 0, [] -> yield acc 
        | _, [] -> () 
    }
    let private combinations2 = combinations [] 2

    // Removes the 4 final letters of a string
    type String with
        member this.RemoveExt =
            this.Remove (this.Length - 4, 4)

    // Active pattern to replace "adjectif" by "-adjectif" at the end of a string
    let private (|Adjective|_|) (word: string) =
        if (word.EndsWith("adjectif") && word.Length > 8) then
            Some <| word.Substring (0, word.Length - 8) + "-adjectif"
        else
            None

    //////////////////////
    // HELPER FUNCTIONS //
    //////////////////////

    // Check for the synonyms of the given word in the DB
    let private getSynonymsInDb (db: SynonymDb.ServiceTypes.SimpleDataContextTypes.Synonymdb) (wordInDb: SynonymDb.ServiceTypes.Words) =
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
        ] |> Seq.map (fun item -> (item.Id, item.Word))
    
    // Gets the current synonyms of the given word and add it to the seq
    let private getSynonymsAndWordInDb (db: SynonymDb.ServiceTypes.SimpleDataContextTypes.Synonymdb) (wordInDb: SynonymDb.ServiceTypes.Words) =
        wordInDb |> getSynonymsInDb db |> Seq.append [(wordInDb.Id, wordInDb.Word)]
    
    //////////////////////
    // PUBLIC INTERFACE //
    //////////////////////

    // Query the Cordial dictionary to find the nominal form of the given word
    // Output is a string * bool, second equals true if the word exists in the dictionary
    let FindWord wordToCheck = async {
        let word = wordToCheck |> Seq.filter (fun char -> Char.IsLetterOrDigit char)
                               |> String.Concat

        let! wordInDict = Http.AsyncRequestString("http://dictionnaire.cordial-enligne.fr/php/search.php", body = FormValues ["mot", word])
        match wordInDict with
        | "0" -> return (word, false) // Doesn't exists, so we return the original word
        | wd  -> match wd.RemoveExt with // Response word has a ".xml" suffix
                 | Adjective wda -> return (wda, true) // Adjective can have a "adjectif" suffix that isn't properly written
                 | wda           -> return (wda, true)
    }   
           
    // Returns all synonyms for a given word, using the exisiting database and the online Cordial dictionary
    let FindSynonyms word = async {
        let db = SynonymDb.GetDataContext () // Can't be shared between threads

        dbMutex.WaitOne () |> ignore

        // Gather all exisiting words from the database
        let previousWordsInDb = query {
            for row in db.Words do
                select row
        }
        
        // Then, let's try to find our word in it
        let previousWordInDb = previousWordsInDb |> Seq.tryFind (fun item -> item.Word = fst word)

        // The actual word object of the database
        let wordInDb =
            match previousWordInDb with
            | None -> // Doesn't exists in the database, so let's add it
                let newId = 
                    if Seq.isEmpty previousWordsInDb then
                        1 // In case DB is empty
                    else
                        (Seq.last previousWordsInDb).Id + 1
                let newWordInDb = new SynonymDb.ServiceTypes.Words(Id = newId, Word = fst word, HasBeenChecked = false)
                db.Words.InsertOnSubmit newWordInDb
                db.DataContext.SubmitChanges ()
                newWordInDb
            | Some word -> word // Or else it's simply the one we found earlier

        dbMutex.ReleaseMutex ()

        match word with
        | (_, false) -> return Seq.singleton (wordInDb.Id, wordInDb.Word) // Word doesn't exists so no synonyms
        | (_, true) ->
            // If the word hasn't been checked already, we can jump directly to the end and return the synonyms from the DB
            if not wordInDb.HasBeenChecked then
                query {
                    for row in db.Words do
                        where (row.Word = fst word)
                        select row
                } |> Seq.iter (fun row -> row.HasBeenChecked <- true)

                // Request the xml definition of the word 
                let xml = Http.AsyncRequestString("http://dictionnaire.cordial-enligne.fr/DictionnaireXml/" + fst word + ".xml")
                          |> Async.RunSynchronously
                          |> XmlWord.Parse

                // Find all synonyms in the parsed XML
                let newWords =
                    seq {
                        for sens in xml.Synonymes.Sens do
                           for synonym in sens.Synonymes do
                                yield synonym.Value 
                    }
                    |> Seq.distinct
                    |> Seq.filter (fun item -> (item.Split ' ' |> Array.length = 1)) // Only one word synonyms are kept

                if not <| Seq.isEmpty newWords then
                    dbMutex.WaitOne () |> ignore
                    
                    let wordsInDb = query {
                        for row in db.Words do
                            select row
                    }

                    let wordId = (Seq.last wordsInDb).Id + 1
                    let wordMap = newWords // Creates a Map int * string with ID and Word
                                  |> Seq.filter (fun item -> not (wordsInDb |> Seq.exists (fun elem -> elem.Word = item))) // No duplicates with DB
                                  |> Seq.map (fun item -> (wordId + (newWords |> Seq.findIndex ((=) item)), item))
                    // Creates new word entries for the database
                    wordMap |> Seq.map (fun item -> new SynonymDb.ServiceTypes.Words(Id = fst item, Word = snd item))
                            |> db.Words.InsertAllOnSubmit
                    // Creates new relation entries for the database
                    wordMap |> Seq.append [(wordInDb.Id, wordInDb.Word)]
                            |> Seq.toList
                            |> combinations2 // Get all relations with all elements of the sequence
                            |> Seq.map (fun item -> new SynonymDb.ServiceTypes.Relations(Word1 = (item.[1] |> fst), Word2 = (item.[0] |> fst)))
                            |> db.Relations.InsertAllOnSubmit
                    
                    db.DataContext.SubmitChanges ()
                    dbMutex.ReleaseMutex ()

            return getSynonymsAndWordInDb db wordInDb
    }