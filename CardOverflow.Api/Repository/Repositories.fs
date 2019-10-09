namespace CardOverflow.Api

open CardOverflow.Pure.Core
open System.Security.Cryptography
open System
open LoadersAndCopiers
open CardOverflow.Pure
open CardOverflow.Debug
open CardOverflow.Entity
open Microsoft.EntityFrameworkCore
open System.Linq
open Helpers
open FSharp.Control.Tasks
open System.Collections.Generic
open X.PagedList
open System.Threading.Tasks
open Microsoft.FSharp.Core

module RelationshipRepository =
    let addAndSaveAsync (db: CardOverflowDb) (relationship: RelationshipEntity) =
        db.Relationship.AddI relationship
        db.SaveChangesAsyncI ()
    let removeAndSaveAsync (db: CardOverflowDb) sourceId targetId userId name =
        db.Relationship.SingleOrDefault(fun x -> 
            ((x.SourceId = sourceId && x.TargetId = targetId)  ||
             (x.SourceId = targetId && x.TargetId = sourceId)) &&
            x.Name = name &&
            x.UserId = userId
        ) |> function
        | null -> ()
        | x -> db.Relationship.RemoveI x
        db.SaveChangesAsyncI ()

module CommentRepository =
    let addAndSaveAsync (db: CardOverflowDb) (comment: CommentCardEntity) =
        db.CommentCard.AddI comment
        db.SaveChangesAsyncI ()

module CardTemplateRepository =
    let GetFromInstance (db: CardOverflowDb) instanceId =
        task {
            let! instance =
                db.CardTemplateInstance
                    .Include(fun x -> x.CardTemplate.CardTemplateInstances)
                    .FirstAsync(fun x -> x.Id = instanceId)
            return instance.CardTemplate |> CardTemplate.load
        }
    let UpdateFieldsToNewInstance (db: CardOverflowDb) (template: CardTemplate) =
        task {
            let! cardTemplate = db.CardTemplate.SingleAsync(fun x -> x.Id = template.Id)
            cardTemplate.Name <- template.Name
            let newTemplateInstance = template.LatestInstance.CopyToNewInstance template.Id
            db.CardTemplateInstance.AddI newTemplateInstance
            use hasher = SHA256.Create ()
            db  
                .AcquiredCard
                .Include(fun x -> x.CardInstance)
                .Where(fun x -> x.CardInstance.CardTemplateInstanceId = template.LatestInstance.Id)
                |> Seq.iter(fun ac ->
                    db.Entry(ac.CardInstance).State <- EntityState.Added
                    ac.CardInstance.Id <- ac.CardInstance.GetHashCode()
                    db.Entry(ac.CardInstance).Property(Core.nameof <@ any<CardInstanceEntity>.Id @>).IsTemporary <- true
                    ac.CardInstance.Created <- DateTime.UtcNow
                    ac.CardInstance.Modified <- Nullable()
                    ac.CardInstance.CardTemplateInstance <- newTemplateInstance
                    ac.CardInstance.AcquireHash <- CardInstanceEntity.acquireHash ac.CardInstance newTemplateInstance.AcquireHash hasher
                )
            return! db.SaveChangesAsyncI()
        }

module HistoryRepository =
    let addAndSaveAsync (db: CardOverflowDb) e =
        db.History.AddI e
        db.SaveChangesAsyncI ()

module CardRepository =
    let instance (db: CardOverflowDb) userId instanceId = task {
        let! r =
            db.CardInstance
                .Include(fun x -> x.CardTemplateInstance)
                .SingleAsync(fun x -> x.Id = instanceId)
        return CardInstance.load userId r
    }
    let Revisions (db: CardOverflowDb) userId cardId = task {
        let! r =
            db.Card
                .Include(fun x -> x.Author)
                .Include(fun x -> x.CardInstances)
                .SingleAsync(fun x -> x.Id = cardId)
        return CardRevision.load userId r
    }
    let private getCompleteCards (db: CardOverflowDb) =
        db.AcquiredCard
            .Include(fun x -> x.CardOption)
            .Include(fun x -> x.CardInstance.CardTemplateInstance)
    let GetTodaysCards (db: CardOverflowDb) userId =
        let tomorrow = DateTime.UtcNow.AddDays 1.
        task {
            let! cards =
                (getCompleteCards db)
                    .Where(fun x -> x.UserId = userId && x.Due < tomorrow)
                    .OrderBy(fun x -> x.Due)
                    .Take(3) // highTODO fix
                    .ToListAsync()
            return
                cards |> Seq.map (QuizCard.load userId)
        }
    let GetAllCards (db: CardOverflowDb) userId =
        (getCompleteCards db)
            .Where(fun x -> x.UserId = userId)
            .AsEnumerable()
        |> Seq.map (QuizCard.load userId)
    let AcquireCardAsync (db: CardOverflowDb) userId cardInstanceId = task {
        let! defaultCardOption =
            db.CardOption
                .SingleAsync(fun x -> x.UserId = userId && x.IsDefault)
        let! cardInstance = db.CardInstance.SingleAsync(fun x -> x.Id = cardInstanceId)
        match! db.AcquiredCard.SingleOrDefaultAsync(fun x -> x.UserId = userId && x.CardInstance.CardId = cardInstance.CardId) with
        | null ->
            let card =
                AcquiredCard.InitialCopyTo
                    userId
                    defaultCardOption.Id
                    []
            card.CardInstanceId <- cardInstanceId
            card |> db.AcquiredCard.AddI
        | x -> x.CardInstanceId <- cardInstanceId
        return! db.SaveChangesAsyncI ()
        }
    let UnacquireCardAsync (db: CardOverflowDb) acquiredCardId =
        db.AcquiredCard.Single(fun x -> x.Id = acquiredCardId)
        |> db.AcquiredCard.RemoveI
        db.SaveChangesAsyncI ()
    let Get (db: CardOverflowDb) userId cardId =
        task {
            let! concept =
                if userId = 0 then
                    db.Card
                        .Include(fun x -> x.Author)
                        .Include(fun x -> x.CommentCards :> IEnumerable<_>)
                            .ThenInclude(fun (x: CommentCardEntity) -> x.User )
                        .Include(fun x -> x.CardInstances :> IEnumerable<_>)
                            .ThenInclude(fun (x: CardInstanceEntity) -> x.CardTemplateInstance)
                        .FirstAsync(fun x -> x.Id = cardId)
                else
                    db.Card
                        .Include(fun x -> x.Author)
                        .Include(fun x -> x.CommentCards :> IEnumerable<_>)
                            .ThenInclude(fun (x: CommentCardEntity) -> x.User )
                        .Include(fun x -> x.CardInstances :> IEnumerable<_>)
                            .ThenInclude(fun (x: CardInstanceEntity) -> x.CardTemplateInstance)
                        .Include(fun x -> x.CardInstances :> IEnumerable<_>)
                            .ThenInclude(fun (x: CardInstanceEntity) -> x.AcquiredCards :> IEnumerable<_>)
                            .ThenInclude(fun (x: AcquiredCardEntity) -> x.Tag_AcquiredCards :> IEnumerable<_>)
                            .ThenInclude(fun (x: Tag_AcquiredCardEntity) -> x.Tag)
                        .Include(fun x -> x.RelationshipSources :> IEnumerable<_>)
                            .ThenInclude(fun (x: RelationshipEntity) -> x.Target.CardInstances :> IEnumerable<_>)
                            .ThenInclude(fun (x: CardInstanceEntity) -> x.CardTemplateInstance)
                        .Include(fun x -> x.RelationshipSources :> IEnumerable<_>)
                            .ThenInclude(fun (x: RelationshipEntity) -> x.Target.CardInstances :> IEnumerable<_>)
                            .ThenInclude(fun (x: CardInstanceEntity) -> x.AcquiredCards)
                        .Include(fun x -> x.RelationshipTargets :> IEnumerable<_>)
                            .ThenInclude(fun (x: RelationshipEntity) -> x.Source.CardInstances :> IEnumerable<_>)
                            .ThenInclude(fun (x: CardInstanceEntity) -> x.CardTemplateInstance)
                        .Include(fun x -> x.RelationshipTargets :> IEnumerable<_>)
                            .ThenInclude(fun (x: RelationshipEntity) -> x.Source.CardInstances :> IEnumerable<_>)
                            .ThenInclude(fun (x: CardInstanceEntity) -> x.AcquiredCards)
                        .FirstAsync(fun x -> x.Id = cardId)
            return concept |> ExploreCard.load userId
        }
    let CreateCard (db: CardOverflowDb) (concept: InitialCardInstance) fileCardInstances =
        fileCardInstances |> concept.CopyToNew |> db.CardInstance.AddI
        db.SaveChangesI ()
    let private get (db: CardOverflowDb) =
        db.AcquiredCard
            .Include(fun x -> x.CardInstance.CardTemplateInstance)
            .Include(fun x -> x.CardInstance.AcquiredCards :> IEnumerable<_>)
                .ThenInclude(fun (x: AcquiredCardEntity) -> x.Tag_AcquiredCards :> IEnumerable<_>)
                .ThenInclude(fun (x: Tag_AcquiredCardEntity) -> x.Tag)
    let GetAcquired (db: CardOverflowDb) (userId: int) (cardId: int) =
        get(db)
            .FirstAsync(fun x -> x.CardInstance.CardId = cardId && x.UserId = userId)
            .ContinueWith(fun (x: Task<AcquiredCardEntity>) -> AcquiredCard.load x.Result)
    let GetAcquiredPages (db: CardOverflowDb) (userId: int) (pageNumber: int) =
        task {
            let! r =
                get(db)
                    .Where(fun x -> x.UserId = userId)
                    .ToPagedListAsync(pageNumber, 15)
            return {
                Results = r |> Seq.map AcquiredCard.load
                Details = {
                    CurrentPage = r.PageNumber
                    PageCount = r.PageCount
                }
            }
        } 
    let SearchAsync (db: CardOverflowDb) userId (pageNumber: int) (searchTerm: string) =
        task {
            let emptyStringCheck = if String.IsNullOrWhiteSpace searchTerm then "\"\"" else searchTerm // https://stackoverflow.com/a/347232
            let! r =
                db.Card
                    .Where(fun x ->
                        x.CardInstances.Any(fun x -> x.AcquiredCards.Any(fun x -> x.Tag_AcquiredCards.Any(fun x -> x.Tag.Name.Contains searchTerm))) ||
                        x.CardInstances.Any(fun x -> EF.Functions.FreeText(x.FieldValues, emptyStringCheck))
                    )
                    .Include(fun x -> x.Author)
                    .Include(fun x -> x.CardInstances :> IEnumerable<_>)
                        .ThenInclude(fun (x: CardInstanceEntity) -> x.CardTemplateInstance)
                    .Include(fun x -> x.CardInstances :> IEnumerable<_>)
                        .ThenInclude(fun (x: CardInstanceEntity) -> x.AcquiredCards :> IEnumerable<_>)
                        .ThenInclude(fun (x: AcquiredCardEntity) -> x.Tag_AcquiredCards :> IEnumerable<_>)
                        .ThenInclude(fun (x: Tag_AcquiredCardEntity) -> x.Tag)
                    .OrderByDescending(fun x -> x.Users)
                    .ToPagedListAsync(pageNumber, 15)
            return {
                Results = r |> Seq.map (ExploreCard.load userId)
                Details = {
                    CurrentPage = r.PageNumber
                    PageCount = r.PageCount
                }
            }
        }
    let UpdateFieldsToNewInstance (db: CardOverflowDb) (acquiredCard: AcquiredCard) =
        task {
            let! e = db.AcquiredCard.FirstAsync(fun x -> x.Id = acquiredCard.AcquiredCardId)
            e.CardInstance <- acquiredCard.CardInstance.CopyFieldsToNewInstance acquiredCard.CardId acquiredCard.CardTemplateInstance.Id
            return! db.SaveChangesAsyncI()
        }

    // member this.SaveCards(cards: ResizeArray<CardEntity>) =
    //                 this.GetCards().Merge cards
    //             (fun (x, y) -> x.Id = y.Id)
    //             id
    //             (db.Remove >> ignore)
    //             (db.Add >> ignore)
    //             (fun d s -> // todo make copyto
    //                 d.Title <- s.Title
    //                 d.Description <- s.Description
    //                 d.Fields <- s.Fields
    //                 d.CardTemplate <- s.CardTemplate
    //                 db.Update d |> ignore)
    //     )

module CardOptionsRepository =
    let defaultCardOptions =
        { Id = 0
          Name = "Default"
          IsDefault = true
          NewCardsSteps = [ TimeSpan.FromMinutes 1.; TimeSpan.FromMinutes 10. ]
          NewCardsMaxPerDay = int16 20
          NewCardsGraduatingInterval = TimeSpan.FromDays 1.
          NewCardsEasyInterval = TimeSpan.FromDays 4.
          NewCardsStartingEaseFactor = 2.5
          NewCardsBuryRelated = true
          MatureCardsMaxPerDay = int16 200
          MatureCardsEaseFactorEasyBonusFactor = 1.3
          MatureCardsIntervalFactor = 1.
          MatureCardsMaximumInterval = 36500. |> TimeSpanInt16.fromDays
          MatureCardsHardInterval = 1.2
          MatureCardsBuryRelated = true
          LapsedCardsSteps = [ TimeSpan.FromMinutes 10. ]
          LapsedCardsNewIntervalFactor = 0.
          LapsedCardsMinimumInterval = TimeSpan.FromDays 1.
          LapsedCardsLeechThreshold = byte 8
          ShowAnswerTimer = false
          AutomaticallyPlayAudio = false
          ReplayQuestionAudioOnAnswer = false }
    let defaultCardOptionsEntity =
        defaultCardOptions.CopyToNew
    //let defaultAnkiCardOptions =
    //    { Id = 0
    //      Name = "Default Anki Options"
    //      NewCardsSteps = [ TimeSpan.FromMinutes 1.; TimeSpan.FromMinutes 10. ]
    //      NewCardsMaxPerDay = int16 20
    //      NewCardsGraduatingInterval = TimeSpan.FromDays 1.
    //      NewCardsEasyInterval = TimeSpan.FromDays 4.
    //      NewCardsStartingEaseFactor = 2.5
    //      NewCardsBuryRelated = false
    //      MatureCardsMaxPerDay = int16 200
    //      MatureCardsEaseFactorEasyBonusFactor = 1.3
    //      MatureCardsIntervalFactor = 1.
    //      MatureCardsMaximumInterval = 36500. |> TimeSpanInt16.fromDays
    //      MatureCardsHardInterval = 1.2
    //      MatureCardsBuryRelated = false
    //      LapsedCardsSteps = [ TimeSpan.FromMinutes 10. ]
    //      LapsedCardsNewIntervalFactor = 0.
    //      LapsedCardsMinimumInterval = TimeSpan.FromDays 1.
    //      LapsedCardsLeechThreshold = byte 8
    //      ShowAnswerTimer = false
    //      AutomaticallyPlayAudio = false
    //      ReplayQuestionAudioOnAnswer = false }

module UserRepository =
    let add (db: CardOverflowDb) name email =
        let cardOption = CardOptionsRepository.defaultCardOptions.CopyToNew 0
        cardOption.IsDefault <- true
        UserEntity(
            DisplayName = name,
            Email = email,
            CardOptions = (cardOption |> Seq.singleton |> fun x -> x.ToList())
        ) |> db.User.AddI
        db.SaveChangesI ()
    let Get (db: CardOverflowDb) email =
        db.User.FirstOrDefault(fun x -> x.Email = email)

module TagRepository =
    let tagEntities (db: CardOverflowDb) newTags =
        let newTags = newTags |> Seq.distinct // https://stackoverflow.com/a/18113534
        db.Tag // medTODO there's no filter, you're .ToListing all tags into memory
            .Select(fun x -> x.Name)
            .AsEnumerable()
            .Where(newTags.Contains)
            .ToList()
            .Contains >> not
        |> newTags.Where
        |> Seq.map (fun x -> TagEntity(Name = x))
    let Add (db: CardOverflowDb) userId newTags =
        tagEntities db newTags
        |> db.Tag.AddRange
        db.SaveChangesI ()

    let AddTo (db: CardOverflowDb) newTag acquiredCardId =
        defaultArg
            (db.Tag.SingleOrDefault(fun x -> x.Name = newTag) |> Option.ofObj)
            (TagEntity(Name = newTag))
        |> fun x -> Tag_AcquiredCardEntity(AcquiredCardId = acquiredCardId, Tag = x)
        |> db.Tag_AcquiredCard.AddI
        db.SaveChangesI ()
    
    let DeleteFrom (db: CardOverflowDb) tagName acquiredCardId = task {
        let! tag = db.Tag.SingleOrDefaultAsync(fun x -> x.Name = tagName)
        return!
            tag
            |> function
            | null -> Task.FromResult()
            | tag ->
                db.Tag_AcquiredCard.Single(fun x -> x.AcquiredCardId = acquiredCardId && x.TagId = tag.Id)
                |> db.Tag_AcquiredCard.RemoveI
                db.SaveChangesAsyncI()
        }

    let Search (db: CardOverflowDb) (input: string) =
        db.Tag.Where(fun t -> t.Name.ToLower().Contains(input.ToLower())).ToList()
    
    let GetAll (db: CardOverflowDb) userId =
        db.Tag.ToList()
        
    let Update (db: CardOverflowDb) tag =
        db.Tag.UpdateI tag
        db.SaveChangesI ()

    let Delete (db: CardOverflowDb) tag =
        db.Tag.RemoveI tag
        db.SaveChangesI ()

module DeckRepository =
    let Create (db: CardOverflowDb) deck =
        db.Deck.AddI deck
        db.SaveChangesI ()

    let Get (db: CardOverflowDb) userId =
        db.Deck.Where(fun d -> d.UserId = userId).ToList()
        
    let Update (db: CardOverflowDb) deck =
        db.Deck.UpdateI deck
        db.SaveChangesI ()

    let Delete (db: CardOverflowDb) deck =
        db.Deck.RemoveI deck
        db.SaveChangesI ()
