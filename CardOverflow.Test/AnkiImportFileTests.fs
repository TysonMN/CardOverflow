module AnkiImportFileTests

open CardOverflow.Api
open ContainerExtensions
open LoadersAndCopiers
open Helpers
open CardOverflow.Debug
open CardOverflow.Entity
open CardOverflow.Pure
open CardOverflow.Test
open Microsoft.EntityFrameworkCore
open Microsoft.FSharp.Quotations
open System.Linq
open Xunit
open System
open AnkiImportTestData
open SimpleInjector
open SimpleInjector.Lifestyles
open CardOverflow.Sanitation
open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Security.Cryptography
open System.Collections.Generic

[<Theory>]
[<ClassData(typeof<AllDefaultTemplatesAndImageAndMp3>)>]
let ``AnkiImporter.save saves three files`` ankiFileName ankiDb: Task<unit> = task {
    let userId = 3
    use c = new TestContainer(ankiFileName)
    
    do!
        SanitizeAnki.ankiExportsDir +/ ankiFileName
        |> AnkiImporter.loadFiles (fun _ -> None)
        |> Result.bind (AnkiImporter.save c.Db ankiDb userId)
        |> Result.getOk

    Assert.Equal(3, c.Db.File_CardInstance.Count())
    Assert.Equal(3, c.Db.File.Count())
    Assert.NotEmpty(c.Db.CardInstance.Where(fun x -> x.AnkiNoteOrd = Nullable 1uy))
    }

[<Theory>]
[<ClassData(typeof<AllDefaultTemplatesAndImageAndMp3>)>]
let ``Running AnkiImporter.save 3x only imports 3 files`` ankiFileName ankiDb: Task<unit> = task {
    let userId = 3
    use c = new TestContainer(ankiFileName)

    for _ in [1..3] do
        do!
            SanitizeAnki.ankiExportsDir +/ ankiFileName
            |> AnkiImporter.loadFiles (fun sha256 -> c.Db.File.FirstOrDefault(fun f -> f.Sha256 = sha256) |> Option.ofObj)
            |> Result.bind (AnkiImporter.save c.Db ankiDb userId)
            |> Result.getOk

    Assert.Equal(3, c.Db.File_CardInstance.Count())
    Assert.Equal(3, c.Db.File.Count())
    Assert.NotEmpty(c.Db.CardInstance.Where(fun x -> x.AnkiNoteOrd = Nullable 1uy))
    }

[<Fact>]
let ``Anki.replaceAnkiFilenames transforms anki filenames into our filenames`` () =
    let expected = [
        "Basic FrontBasic Back"
        "Basic (and reversed card) frontBasic (and reversed card) back"
        "Basic (optional reversed card) frontBasic (optional reversed card) backBasic (optional reversed card) reverse"
        "Basic (type in the answer) frontBasic (type in the answer) back"
        "Cloze text.&nbsp;Canberra was founded in {{c1::1913}}.Cloze extra"
        """Basic with image&nbsp;<img src="/image/AAEECRAZJDFAUWR5kKnE4QAhRGmQueQRQHGk2RBJhME">Basic back, no image"""
        """Basic front with mp3
<audio controls autoplay>
    <source src="AAIGDBQeKjhIWm6EnLbS8BAyVnykzvooWIq+9CxmouA=" type="audio/mpeg">
    Your browser does not support the audio element.
</audio>
Basic back, no mp3"""
        """<img src="/image/AAIEBggKDA4QEhQWGBocHiAiJCYoKiwuMDI0Njg6PD4"><img src="/image/AAIEBggKDA4QEhQWGBocHiAiJCYoKiwuMDI0Njg6PD4">"""]
    let fields = AnkiImportTestData.allDefaultTemplatesAndImageAndMp3_colpkg.Notes |> List.map(fun x -> x.Flds)
    let map =
        [ ("png1.png", FileEntity(
            Sha256 = Array.init 32 (fun index -> index + index |> byte)
          ))
          ("png2.png", FileEntity(
            Sha256 = Array.init 32 (fun index -> index + index |> byte)
          ))
          ("favicon.ico", FileEntity(
            Sha256 = Array.init 32 (fun index -> index * index |> byte)
          ))
          ("bloop.wav", FileEntity(
            Sha256 = Array.init 32 (fun index -> index * index + index |> byte)
          ))
        ] |> Map.ofList
    
    let actual = fields |> List.map(fun x -> Anki.replaceAnkiFilenames x map |> snd)

    Assert.Equal (expected.Length, actual.Length)
    Seq.zip expected actual
    |> Seq.iter Assert.Equal
    Assert.Equal<string list> (expected, actual)

[<Fact>]
let ``AnkiImporter import cards that have the same acquireHash as distinct cards`` (): Task<unit> = task { // lowTODO, perhaps they should be the same card
    let userId = 3
    use c = new TestContainer()
    do!
        AnkiImporter.save c.Db duplicatesFromLightyear userId Map.empty
        |> Result.getOk
    Assert.Equal<string seq>(
        ["bab::endocrinology::thyroid::thyroidcancer"; "bab::gastroenterology::clinical::livertumors"; "Deck:duplicate cards"; "DifferentCaseRepeatedTag"; "Pathoma::Neoplasia::Tumor_Progression"; "repeatedTag"],
        c.Db.Tag.Select(fun x -> x.Name).OrderBy(fun x -> x))
    Assert.Equal(3, c.Db.Card.Count())
    Assert.Equal(3, c.Db.CardInstance.Count())
    }

let testCommunalFields (c: TestContainer) userId cardId expected = task {
    let! acquired = CardRepository.GetAcquired c.Db userId cardId
    let acquired = Result.getOk acquired
    Assert.Equal<string seq>(
        expected |> List.map MappingTools.stripHtmlTags |> List.sort,
        acquired.CardInstanceMeta.CommunalFields.Select(fun x -> x.Value |> MappingTools.stripHtmlTags) |> Seq.sort)}

[<Fact>]
let ``Multiple cloze indexes works and missing image => <img src="missingImage.jpg">`` (): Task<unit> = task {
    let userId = 3
    use c = new TestContainer()
    let testCommunalFields = testCommunalFields c userId
    do!
        AnkiImporter.save c.Db multipleClozeAndSingleClozeAndNoClozeWithMissingImage userId Map.empty
        |> Result.getOk
    Assert.Equal(
        5,
        c.Db.CardInstance
            .Count(fun x -> x.FieldValues.Contains("may be remembered with the mnemonic"))
        )
    Assert.SingleI
        <| c.Db.CardInstance
            .Where(fun x -> x.FieldValues.Contains("Fibrosis"))
    Assert.SingleI
        <| c.Db.CardInstance
            .Where(fun x -> x.FieldValues.Contains("acute"))
    Assert.True(c.Db.CardInstance.Select(fun x -> x.FieldValues).Single(fun x -> x.Contains "Prerenal").Contains """<img src="/missingImage.jpg">""")
    let longThing = """Drugs that act on microtubules may be remembered with the mnemonic "Microtubules Get Constructed Very Poorly":M: {{c1::Mebendazole (antihelminthic)}}G: {{c2::Griseofulvin (antifungal)}} C: {{c3::Colchicine (antigout)}} V: {{c4::Vincristine/Vinblastine (anticancer)}}P: {{c5::Palcitaxel (anticancer)}}"""
    Assert.Equal<string seq>(
        [   longThing
            ""
            "↑ {{c1::Cl−}} concentration (> 60 mEq/L) in sweat is diagnostic for Cystic Fibrosis"
            "Image here"],
        c.Db.CommunalFieldInstance.Select(fun x -> MappingTools.stripHtmlTags x.Value))
    let! card = CardRepository.Get c.Db userId 1
    Assert.Equal<string seq>(
        [longThing; ""],
        card.LatestMeta.CommunalFields.Select(fun x -> x.Value |> MappingTools.stripHtmlTags))
    Assert.Equal(
        """Drugs that act on microtubules may be remembered with the mnemonic "Microtubules Get Constructed Very Poorly":M: [ ... ] G: Griseofulvin (antifungal) C: Colchicine (antigout) V: Vincristine/Vinblastine (anticancer)P: Palcitaxel (anticancer)""",
        card.LatestMeta.StrippedFront)
    let! card = CardRepository.Get c.Db userId 1
    Assert.Empty card.Relationships
    Assert.Empty c.Db.Relationship

    let! clozes = c.Db.CardInstance.Where(fun x -> x.FieldValues.Contains "mnemonic").ToListAsync()
    let initialInstance = clozes.First()
    for instance in clozes do
        do! testCommunalFields instance.CardId [longThing; ""]

    let! editCommand = SanitizeCardRepository.getEdit c.Db initialInstance.Id
    let editCommand = editCommand |> Result.getOk
    Assert.Empty(editCommand.FieldValues.Where(fun x -> not <| x.IsCommunal))
    let communalFields = editCommand.FieldValues.Where(fun x -> x.IsCommunal) |> List.ofSeq
    let updatedCommunalField = communalFields.[0]
    Assert.Contains("microtubules", updatedCommunalField.Value)
    let updatedCommunalField = { updatedCommunalField with Value = Guid.NewGuid().ToString() + updatedCommunalField.Value }
    let updatedCommand = { editCommand with FieldValues = [updatedCommunalField; communalFields.[1]].ToList() }
    let! acquired = CardRepository.GetAcquired c.Db userId initialInstance.CardId
    let! x = SanitizeCardRepository.Update c.Db userId (Result.getOk acquired) updatedCommand
    Result.getOk x
    for instance in clozes do
        do! testCommunalFields instance.CardId [updatedCommunalField.Value; ""]

    let! card = CardRepository.Get c.Db userId <| clozes.First().CardId
    let! editCommand = SanitizeCardRepository.getEdit c.Db card.LatestMeta.Id
    let editCommand = editCommand |> Result.getOk
    Assert.Empty(editCommand.FieldValues.Where(fun x -> not <| x.IsCommunal))
    let communalFields = editCommand.FieldValues.Where(fun x -> x.IsCommunal) |> List.ofSeq
    let updatedCommunalField0 = communalFields.[0]
    let updatedCommunalField1 = communalFields.[1]
    Assert.Contains("microtubules", updatedCommunalField0.Value)
    Assert.Equal("<b><br /></b>", updatedCommunalField1.Value)
    let updatedCommunalField0 = { updatedCommunalField0 with Value = Guid.NewGuid().ToString() + updatedCommunalField0.Value }
    let updatedCommunalField1 = { updatedCommunalField1 with Value = Guid.NewGuid().ToString() + updatedCommunalField1.Value }
    let updatedCommand = { editCommand with FieldValues = [updatedCommunalField0; updatedCommunalField1].ToList() }
    let! acquired = CardRepository.GetAcquired c.Db userId initialInstance.CardId
    let! x = SanitizeCardRepository.Update c.Db userId (Result.getOk acquired) updatedCommand
    Result.getOk x
    for instance in clozes do
        do! testCommunalFields instance.CardId [updatedCommunalField0.Value; updatedCommunalField1.Value] }

[<Fact>]
let ``Create cloze card works`` (): Task<unit> = task {
    let userId = 3
    use c = new TestContainer()
    let testCommunalFields = testCommunalFields c userId
    let! templates = SanitizeCardTemplate.Search c.Db "Cloze"
    let clozeTemplate = templates.Single(fun x -> x.Name = "Cloze")

    let test clozeMaxIndex clozeText clozeExtra otherTest = task {
        let updateCommand = {
            EditSummary = "Initial creation"
            FieldValues =
                clozeTemplate.Fields.Select(fun f -> {
                    EditField = ViewField.copyTo f
                    Value =
                        if f.Name = "Text" then
                            clozeText
                        else
                            clozeExtra
                    Communal = None
                }).ToList()
            TemplateInstance = clozeTemplate }
        let! card = CardRepository.getNew c.Db userId
        let! x = SanitizeCardRepository.Update c.Db userId card updateCommand
        Result.getOk x
        for i in [1 .. clozeMaxIndex] |> List.map byte do
            let clozeText = AnkiImportLogic.multipleClozeToSingleCloze i [clozeText] |> Seq.exactlyOne
            let cardId = c.Db.CardInstance.Single(fun x -> x.FieldValues.Contains(clozeText)).CardId
            do! testCommunalFields cardId [clozeText; clozeExtra]
        otherTest clozeText }
    do! test 1 "Canberra was founded in {{c1::1913}}." "extra"
        <| fun clozeText -> Assert.SingleI(c.Db.CardInstance.Where(fun x -> x.FieldValues.Contains clozeText))
    do! test 1 "{{c1::Canberra::city}} was founded in {{c1::1913}}." "extra"
        <| fun clozeText -> Assert.SingleI(c.Db.CardInstance.Where(fun x -> x.FieldValues.Contains clozeText))
    do! test 2 "{{c1::Portland::city}} was founded in {{c2::1845}}." "extra"
        <| fun clozeText -> 
            Assert.Equal(0, c.Db.CardInstance.Count(fun x -> x.FieldValues.Contains clozeText))
            Assert.Equal(1, c.Db.CardInstance.Count(fun x -> x.FieldValues.Contains "Portland was founded in {{c2::1845}}."))
            Assert.Equal(1, c.Db.CardInstance.Count(fun x -> x.FieldValues.Contains "{{c1::Portland::city}} was founded in 1845."))
    }

[<Fact>]
let ``Creating card with shared "Back" field works twice`` (): Task<unit> = task {
    let userId = 3
    use c = new TestContainer()
    let! template =
        SanitizeCardTemplate.Search c.Db "Basic"
        |> TaskX.map (fun x -> x.Single(fun x -> x.Name = "Basic"))
    
    let test communalValue id editSummary = task {
        let! acquired = CardRepository.getNew c.Db userId
        do! SanitizeCardRepository.Update
                c.Db
                userId
                acquired
                {   EditSummary = editSummary
                    FieldValues =
                        template
                            .Fields
                            .Select(fun f ->
                                let value, communal =
                                    if f.Name = "Front" then
                                        "Front", None
                                    else
                                        communalValue, Some { CommunalCardInstanceIds = [0].ToList() }
                                {   EditField = ViewField.copyTo f
                                    Value = value
                                    Communal = communal
                                })
                            .ToList()
                    TemplateInstance = template }
            |> TaskX.map Result.getOk
    
        let! field = c.Db.CommunalField.SingleAsync(fun x -> x.Id = id)
        Assert.Equal(id, field.Id)
        Assert.Equal(3, field.AuthorId)
        let! instance = c.Db.CommunalFieldInstance.Include(fun x -> x.CommunalFieldInstance_CardInstances).SingleAsync(fun x -> x.Value = communalValue)
        let communalFieldInstanceId = id
        Assert.Equal(communalFieldInstanceId, instance.Id)
        Assert.Equal(id, instance.CommunalFieldId)
        Assert.Equal("Back", instance.FieldName)
        Assert.Equal(communalValue, instance.Value)
        Assert.Null instance.Modified
        Assert.Equal(editSummary, instance.EditSummary)
        Assert.Equal(id, instance.CommunalFieldInstance_CardInstances.Single().CardInstanceId)
        Assert.Equal(id, instance.CommunalFieldInstance_CardInstances.Single().CommunalFieldInstanceId) }
    do! test "a" 1 <| Guid.NewGuid().ToString()
    do! test "b" 2 <| Guid.NewGuid().ToString() }

[<Fact>]
let ``EditCardCommand's back works with cloze`` () =
    let test text expected questionTemplate =
        {   EditSummary = ""
            FieldValues =
                CardTemplateInstance.initialize.Fields.Select(fun f -> {
                    EditField = f
                    Value =
                        if f.Name = "Front" then
                            text
                        else
                            f.Name
                    Communal = None
                }).ToList()
            TemplateInstance =
                { CardTemplateInstance.initialize with
                    QuestionTemplate = questionTemplate
                } |> ViewCardTemplateInstance.load
        }.Backs
        |> Result.getOk
        |> Seq.map MappingTools.stripHtmlTags
        |> fun x -> Assert.Equal<string seq>(expected, x)
    let testOrdinary text expected =
        test text expected "{{Front}}"
    testOrdinary
        "The front"
        [ "The front Back" ]
    let testCloze text expected =
        test text expected "{{cloze:Front}}"
    testCloze
        "{{c1::Canberra::city}} was founded in {{c1::1913}}."
        [   "[ Canberra ] was founded in [ 1913 ] . Back" ]
    testCloze
        "{{c2::Canberra::city}} was founded in {{c1::1913}}."
        [   "Canberra was founded in [ 1913 ] . Back"
            "[ Canberra ] was founded in 1913. Back" ]

    let testMultiCloze front back expectedBack = // https://eshapard.github.io/anki/the-power-of-making-new-cards-on-the-fly-in-anki.html
        {   EditSummary = ""
            FieldValues =
                CardTemplateInstance.initialize.Fields.Select(fun f -> {
                    EditField = f
                    Value =
                        match f.Name with
                        | "Front" -> front
                        | "Back" -> back
                        | _ -> "Source goes here"
                    Communal = None
                }).ToList()
            TemplateInstance =
                { CardTemplateInstance.initialize with
                    QuestionTemplate = "{{cloze:Front}}{{cloze:Back}}"
                    AnswerTemplate = "{{cloze:Front}}{{cloze:Back}}{{Source}}"
                } |> ViewCardTemplateInstance.load
        }.Backs
        |> Result.getOk
        |> Seq.map MappingTools.stripHtmlTags
        |> fun x -> Assert.Equal<string seq>(expectedBack, x)
    testMultiCloze
        "Columbus first crossed the Atlantic in {{c1::1492}}"
        ""
        ["Columbus first crossed the Atlantic in [ 1492 ] Source goes here"]
    testMultiCloze
        "Columbus first crossed the Atlantic in {{c1::1492}}"
        "In {{c2::1492}}, Columbus sailed the ocean {{c3::blue}}."
        [   "Columbus first crossed the Atlantic in [ 1492 ] Source goes here"
            "In [ 1492 ] , Columbus sailed the ocean blue.Source goes here"
            "In 1492, Columbus sailed the ocean [ blue ] .Source goes here" ]

[<Fact>]
let ``AnkiDefaults.cardTemplateIdByHash is same as initial db`` () =
    let c = new TestContainer()
    let userId = 1
    let toEntity (cardTemplate: AnkiCardTemplateInstance) =
        cardTemplate.CopyToNew userId null
    use hasher = SHA256.Create()
    let dbidByHash =
        Anki.parseModels
            userId
            InitializeDatabase.ankiModels
        |> Result.getOk
        |> List.collect (snd >> List.map toEntity)
        |> List.mapi (fun i entity ->
            CardTemplateInstanceEntity.acquireHash hasher entity, i + 1
        ) |> Map.ofList
    let actualDbIdByHash =
        c.Db.CardTemplate
            .Include(fun x -> x.CardTemplateInstances)
            .AsEnumerable()
            .Select(fun x -> x.CardTemplateInstances.Single())
            .Select(fun x -> CardTemplateInstanceEntity.acquireHash hasher x, x.Id)
            |> Map.ofSeq

    dbidByHash |> Map.iter(fun hash expectedId ->
        Assert.Equal(
            expectedId,
            actualDbIdByHash.[hash]))
    Assert.Equal<Map<string, int>>(
        dbidByHash,
        AnkiDefaults.cardTemplateIdByHash)

//[<Fact>]
let ``Manual Anki import`` (): Task<unit> = task {
    let userId = 3
    let pathToCollection = @""
    
    use c = new TestContainer()
    let db = c.Db
    
    //use c = new Container()
    //c.RegisterStuff
    //c.RegisterStandardConnectionString
    //use __ = AsyncScopedLifestyle.BeginScope c
    //let db = c.GetInstance<CardOverflowDb>()

    do!    
        let ankiDb =
            AnkiImporter.getSimpleAnkiDb
            |> using(SanitizeAnki.ankiDb pathToCollection)
        pathToCollection
        |> AnkiImporter.loadFiles (fun sha256 -> db.File |> Seq.tryFind(fun f -> f.Sha256 = sha256))
        |> Result.bind (AnkiImporter.save db ankiDb userId)
        |> function
        | Ok x -> x
        | Error x -> failwith x
    }
