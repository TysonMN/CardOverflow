namespace CardOverflow.Api

open System
open LoadersAndCopiers
open CardOverflow.Pure
open CardOverflow.Debug
open CardOverflow.Entity
open Microsoft.EntityFrameworkCore
open System.Linq
open Helpers

module CardRepository =
    let GetQuizCards (db: CardOverflowDb) userId =
        db.AcquiredCards
            .Include(fun x -> x.CardOption)
            //.Include(fun x -> x.Card)
            //    .ThenInclude(fun x -> x.Concept)
            //    .ThenInclude(fun x -> x.ConceptTemplate)
            .Where(fun x -> x.UserId = userId)
            .AsEnumerable()
        |> Seq.map QuizCard.Load

    let SaveCard (db: CardOverflowDb) card =
        db.Cards.AddI card
        db.SaveChangesI ()

    let AcquireCards (db: CardOverflowDb) userId cardIds =
        let user = db.Users.First(fun x -> x.Id = userId)
        cardIds
        |> List.map (fun i ->
            AcquiredCardEntity(
                UserId = userId, // eventualTODO missing ConceptInstanceId and CardTemplateId
                MemorizationState = MemorizationState.toDb New,
                CardState = CardState.toDb Normal,
                LapseCount = 0uy,
                EaseFactorInPermille = 0s,
                IntervalNegativeIsMinutesPositiveIsDays = 0s,
                StepsIndex = Nullable 0uy,
                Due = DateTime.UtcNow,
                CardOption = user.CardOptions.First(fun x -> x.IsDefault)
            ))
        |> db.AcquiredCards.AddRange
        db.SaveChangesI ()

module ConceptRepository =
    let CreateConcept (db: CardOverflowDb) (concept: InitialConceptInstance) fileConceptInstances =
        fileConceptInstances |> concept.CopyToNew |> db.ConceptInstances.AddI
        db.SaveChangesI ()

    // member this.SaveConcepts(concepts: ResizeArray<ConceptEntity>) =
    //                 this.GetConcepts().Merge concepts
    //             (fun (x, y) -> x.Id = y.Id)
    //             id
    //             (db.Remove >> ignore)
    //             (db.Add >> ignore)
    //             (fun d s -> // todo make copyto
    //                 d.Title <- s.Title
    //                 d.Description <- s.Description
    //                 d.Fields <- s.Fields
    //                 d.ConceptTemplate <- s.ConceptTemplate
    //                 db.Update d |> ignore)
    //     )

module UserRepository =
    let defaultCardOptions =
        { Id = 0
          Name = "Default"
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
    let add (db: CardOverflowDb) name email =
        let cardOption = defaultCardOptions.CopyToNew 0
        cardOption.IsDefault <- true
        UserEntity(
            DisplayName = name,
            Email = email,
            CardOptions = (cardOption |> Seq.singleton |> fun x -> x.ToList())
        ) |> db.Users.AddI
        db.SaveChangesI ()
    let GetUser (db: CardOverflowDb) email =
        db.Users.First(fun x -> x.Email = email)

module PrivateTagRepository =
    let Add (db: CardOverflowDb) userId newTags =
        let newTags = newTags |> List.distinct // https://stackoverflow.com/a/18113534
        db.PrivateTags
            .Where(fun x -> x.UserId = userId)
            .Select(fun x -> x.Name)
            .AsEnumerable()
            .Where(newTags.Contains)
            .ToList()
            .Contains >> not
        |> newTags.Where
        |> Seq.map (fun x -> PrivateTagEntity(Name = x, UserId = userId ))
        |> db.PrivateTags.AddRange
        db.SaveChangesI ()

    let Search (db: CardOverflowDb) (input: string) =
        db.PrivateTags.Where(fun t -> t.Name.ToLower().Contains(input.ToLower()))
        
    let Update (db: CardOverflowDb) tag =
        db.PrivateTags.UpdateI tag
        db.SaveChangesI ()

    let Delete (db: CardOverflowDb) tag =
        db.PrivateTags.RemoveI tag
        db.SaveChangesI ()

module DeckRepository =
    let Create (db: CardOverflowDb) deck =
        db.Decks.AddI deck
        db.SaveChangesI ()

    let Get (db: CardOverflowDb) userId =
        db.Decks.Where(fun d -> d.UserId = userId).ToList()
        
    let Update (db: CardOverflowDb) deck =
        db.Decks.UpdateI deck
        db.SaveChangesI ()

    let Delete (db: CardOverflowDb) deck =
        db.Decks.RemoveI deck
        db.SaveChangesI ()
