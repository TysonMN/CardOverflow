module FacetRepositoryTests

open LoadersAndCopiers
open Helpers
open CardOverflow.Api
open CardOverflow.Debug
open CardOverflow.Entity
open Microsoft.EntityFrameworkCore
open CardOverflow.Test
open System
open System.Linq
open Xunit
open CardOverflow.Pure
open System.Collections.Generic
open FSharp.Control.Tasks
open System.Threading.Tasks

let add templateName fieldValues (db: CardOverflowDb) userId tags =
    let cardTemplateInstance =
        db.CardTemplateInstance
            .Include(fun x -> x.CardTemplate)
            .Include(fun x -> x.User_CardTemplateInstances)
            .First(fun x -> x.CardTemplate.Name = templateName)
            |> AcquiredCardTemplateInstance.load
    TagRepository.Add db userId tags
    let tags = tags |> List.map (fun x -> (TagRepository.Search db x).Single().Id)
    let fieldValues =
        match fieldValues with
        | [] -> ["Front"; "Back"]
        | _ -> fieldValues
    let initialCard = {
        Created = DateTime.Now - TimeSpan.FromDays 1.
        CardTemplateHash = cardTemplateInstance.CardTemplateInstance.AcquireHash
        AuthorId = userId
        Description = templateName
        DefaultCardOptionId = cardTemplateInstance.DefaultCardOptionId
        CardTemplateInstanceIdAndTags = cardTemplateInstance.CardTemplateInstance |> fun x -> x.Id, tags
        FieldValues =
            cardTemplateInstance.CardTemplateInstance.Fields
            |> Seq.sortBy (fun x -> x.Ordinal)
            |> Seq.mapi (fun i field -> { Field = field; Value = fieldValues.[i] })
    }
    CardRepository.CreateCard db initialCard <| Seq.empty.ToList()

let addReversedBasicCard: CardOverflowDb -> int -> string list -> unit =
    add "Basic (and reversed card) - Card 2" []

let addBasicCard =
    add "Basic" []

let addBasicCustomCard x =
    add "Basic" x

[<Fact>]
let ``CardRepository.CreateCard on a basic facet acquires 1 card/facet``() =
    use c = new TestContainer()
    let userId = 3
    let tags = ["a"; "b"]
    
    addBasicCard c.Db userId tags

    Assert.SingleI <| c.Db.Card
    Assert.SingleI <| c.Db.Card
    Assert.SingleI <| c.Db.AcquiredCard
    Assert.SingleI <| CardRepository.GetAllCards c.Db userId
    Assert.Equal(
        """<!DOCTYPE html>
    <head>
        <style>
            .cloze-brackets-front {
                font-size: 150%;
                font-family: monospace;
                font-weight: bolder;
                color: dodgerblue;
            }
            .cloze-filler-front {
                font-size: 150%;
                font-family: monospace;
                font-weight: bolder;
                color: dodgerblue;
            }
            .cloze-brackets-back {
                font-size: 150%;
                font-family: monospace;
                font-weight: bolder;
                color: red;
            }
        </style>
        <style>
            .card {
 font-family: arial;
 font-size: 20px;
 text-align: center;
 color: black;
 background-color: white;
}

        </style>
    </head>
    <body>
        Front
        <script type="text/javascript" src="/js/iframeResizer.contentWindow.min.js"></script> 
    </body>
</html>""",
        (CardRepository.GetTodaysCards c.Db userId).GetAwaiter().GetResult()
        |> Seq.head
        |> Result.getOk
        |> fun x -> x.Front
    )
    Assert.Equal(
        """<!DOCTYPE html>
    <head>
        <style>
            .cloze-brackets-front {
                font-size: 150%;
                font-family: monospace;
                font-weight: bolder;
                color: dodgerblue;
            }
            .cloze-filler-front {
                font-size: 150%;
                font-family: monospace;
                font-weight: bolder;
                color: dodgerblue;
            }
            .cloze-brackets-back {
                font-size: 150%;
                font-family: monospace;
                font-weight: bolder;
                color: red;
            }
        </style>
        <style>
            .card {
 font-family: arial;
 font-size: 20px;
 text-align: center;
 color: black;
 background-color: white;
}

        </style>
    </head>
    <body>
        Front

<hr id=answer>

Back
        <script type="text/javascript" src="/js/iframeResizer.contentWindow.min.js"></script> 
    </body>
</html>""",
        (CardRepository.GetTodaysCards c.Db userId).GetAwaiter().GetResult()
        |> Seq.head
        |> Result.getOk
        |> fun x -> x.Back
    )
    Assert.Equal<FieldAndValue seq>(
        [{  Field = {
                Name = "Front"
                Font = "Arial"
                FontSize = 20uy
                IsRightToLeft = false
                Ordinal = 0uy
                IsSticky = false }
            Value = "Front" }
         {  Field = {
                Name = "Back"
                Font = "Arial"
                FontSize = 20uy
                IsRightToLeft = false
                Ordinal = 1uy
                IsSticky = false }
            Value = "Back"}],
        (CardRepository.GetAcquiredPages c.Db userId 1)
            .GetAwaiter()
            .GetResult()
            .Results
            .Single()
            |> Result.getOk
            |> fun x -> x.CardInstance.FieldValues
            |> Seq.sortByDescending (fun x -> x.Field.Name)
    )
    Assert.Equal<string seq>(
        tags,
        (CardRepository.GetAcquiredPages c.Db userId 1)
            .GetAwaiter()
            .GetResult()
            .Results
            .Single()
            |> Result.getOk
            |> fun x -> x.Tags
    )

[<Fact>]
let ``CardRepository.UpdateFieldsToNewInstance on a basic card updates the fields``() : Task<unit> = task {
    use c = new TestContainer()
    let userId = 3
    let tags = ["a"; "b"]
    addBasicCard c.Db userId tags
    let cardId = 1
    let newValue = Guid.NewGuid().ToString()
    let! old = 
        (CardRepository.GetAcquired c.Db userId cardId)
            .ContinueWith(fun (x: Task<Result<AcquiredCard, string>>) -> Result.getOk x.Result)
    let updated = {
        old with
            CardInstance = {
                old.CardInstance with
                    FieldValues =
                        old.CardInstance.FieldValues.Select(fun x ->
                            { x with Value = newValue}
                        ).ToList()
            }
        }
    
    do! CardRepository.UpdateFieldsToNewInstance c.Db updated
    
    let! updated =
        (CardRepository.GetAcquired c.Db userId cardId)
            .ContinueWith(fun (x: Task<Result<AcquiredCard, string>>) -> Result.getOk x.Result)
    Assert.Equal<string seq>(
        [newValue; newValue],
        updated.CardInstance.FieldValues.Select(fun x -> x.Value))
    Assert.Equal(
        2,
        c.Db.CardInstance.Count(fun x -> x.CardId = cardId))
    let! card = CardRepository.Get c.Db userId cardId
    Assert.Equal<ViewTag seq>(
        [{  Name = "a"
            Count = 1
            IsAcquired = true }
         {  Name = "b"
            Count = 1
            IsAcquired = true }],
        card.Tags)
    Assert.Equal<string seq>(
        [newValue; newValue],
        card.LatestInstance.FieldValues.OrderBy(fun x -> x.Field.Ordinal).Select(fun x -> x.Value)
    )
    let createds = c.Db.CardInstance.Select(fun x -> x.Created) |> Seq.toList
    Assert.NotEqual(createds.[0], createds.[1])

    let! revisions = CardRepository.Revisions c.Db userId cardId
    Assert.Equal(2, revisions.SortedMeta.Count())
    let! instance = CardRepository.instance c.Db userId revisions.SortedMeta.[0].Id
    let revision, _, _, _ = instance.FrontBackFrontSynthBackSynth
    Assert.Contains(newValue, revision)
    let! instance = CardRepository.instance c.Db userId revisions.SortedMeta.[1].Id
    let original, _, _, _ = instance.FrontBackFrontSynthBackSynth
    Assert.Contains("Front", original)
    }
    
// fuck merge
//[<Fact>]
//let ``CardRepository's SaveCards updates a Card``() =
//    use c = new TestContainer()
//    let facet = 
//        CardEntity(
//            Title = "",
//            Description = "",
//            Fields = "",
//            CardTemplateId = 1,
//            Modified = DateTime.UtcNow)
    
//    CardRepository.AddCard c.Db facet

//    let updatedCard = CardRepository.GetCards c.Db |> Seq.head
//    let updatedTitle = Guid.NewGuid().ToString()
//    updatedCard.Title <- updatedTitle

//    updatedCard 
//    |> Seq.singleton 
//    |> ResizeArray<CardEntity>
//    |> CardRepository.SaveCards c.Db

//    CardRepository.GetCards c.Db
//    |> Seq.filter(fun x -> x.Title = updatedTitle)
//    |> Assert.Single
