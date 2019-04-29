module AnkiImportTests

open CardOverflow.Api
open CardOverflow.Entity
open CardOverflow.Test
open Microsoft.EntityFrameworkCore
open Microsoft.FSharp.Quotations
open System.IO
open System.IO.Compression
open System.Linq
open Xunit

let nameof (q: Expr<_>) = // https://stackoverflow.com/a/48311816
    match q with
    | Patterns.Let(_, _, DerivedPatterns.Lambdas(_, Patterns.Call(_, mi, _))) -> mi.Name
    | Patterns.PropertyGet(_, mi, _) -> mi.Name
    | DerivedPatterns.Lambdas(_, Patterns.Call(_, mi, _)) -> mi.Name
    | _ -> failwith "Unexpected format"
let any<'R> : 'R = failwith "!"

let unzipAndGetAnkiDbService collection ankiFileName =
    let baseDir = @"..\netcoreapp3.0\AnkiExports\"
    let tempDir = baseDir + @"Temp\" + ankiFileName + @"\" // Need to isolate ankiDb otherwise tests run in parallel fail
    if Directory.Exists tempDir
    then Directory.Delete(tempDir, true)
    ZipFile.Open(baseDir + ankiFileName, ZipArchiveMode.Read).ExtractToDirectory tempDir
    tempDir + collection |> AnkiDbFactory |> AnkiDbService

let getAnki2 =
    unzipAndGetAnkiDbService "collection.anki2"

let getAnki21 =
    unzipAndGetAnkiDbService "collection.anki21"

let assertHasBasicInfo ankiService dbService =
    let userId = 3
    AnkiImporter(ankiService, dbService, userId).run()
    |> Result.isOk
    |> Assert.True
    Assert.Equal(7, dbService.Query(fun x -> x.Concepts.Count()))
    Assert.Single(dbService.Query(fun x -> x.CardOptions.Where(fun x -> x.UserId = userId).ToList())) |> ignore
    Assert.Equal<string>(
        [ "Basic"; "OtherTag"; "Tag" ],
        dbService.Query(fun x -> x.PrivateTags.ToList()).Select(fun x -> x.Name) |> Seq.sortBy id)
    Assert.Equal<string>(
        [ "OtherTag" ],
        dbService.Query(fun db ->
            db.Concepts
                .Include(nameof <@ any<ConceptEntity>.PrivateTagConcepts @> + "." + nameof <@ any<PrivateTagConceptEntity>.PrivateTag @>)
                .Single(fun c -> c.Fields.Contains("mp3"))
                .PrivateTagConcepts.Select(fun t -> t.PrivateTag.Name)))

let assertNotEmpty (ankiDb: SimpleAnkiDb) =
    ankiDb.Cols |> Assert.NotEmpty
    ankiDb.Cards |> Assert.NotEmpty
    ankiDb.Notes |> Assert.NotEmpty

[<Fact>]
let ``AnkiDbService can read from AllDefaultTemplatesAndImageAndMp3.apkg``() =
    AnkiImportTestData.allDefaultTemplatesAndImageAndMp3_apkg |> assertNotEmpty

[<Fact>]
let ``AnkiImporter can import AllDefaultTemplatesAndImageAndMp3.apkg``() =
    use tempDbService = new TempDbService()
    AnkiImportTestData.allDefaultTemplatesAndImageAndMp3_apkg |> assertHasBasicInfo <| tempDbService.DbService

[<Fact>]
let ``AnkiDbService can read from AllDefaultTemplatesAndImageAndMp3.colpkg``() =
    AnkiImportTestData.allDefaultTemplatesAndImageAndMp3_colpkg |> assertNotEmpty

[<Fact>]
let ``AnkiImporter can import AllDefaultTemplatesAndImageAndMp3.colpkg``() =
    use tempDbService = new TempDbService()
    AnkiImportTestData.allDefaultTemplatesAndImageAndMp3_colpkg |> assertHasBasicInfo <| tempDbService.DbService
