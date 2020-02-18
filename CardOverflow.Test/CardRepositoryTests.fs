module CardRepositoryTests

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
open CardOverflow.Pure
open CardOverflow.Sanitation

[<Fact>]
let ``Users can't acquire multiple instances of a card``(): Task<unit> = task {
    use c = new TestContainer()
    let userId = 3
    let! x = FacetRepositoryTests.addBasicCard c.Db userId []
    Assert.Empty x
    let! template = SanitizeTemplate.AllInstances c.Db 1
    let! ac = CardRepository.GetAcquired c.Db userId 1
    let! x = 
        {   EditCardCommand.EditSummary = ""
            FieldValues = [].ToList()
            TemplateInstance = template.Value.Instances.Single() |> ViewTemplateInstance.copyTo
        } |> CardRepository.UpdateFieldsToNewInstance c.Db ac.Value
    Assert.Empty x.Value

    let i2 = 1002
    do! CardRepository.AcquireCardAsync c.Db userId i2 // acquiring a different revision of a card doesn't create a new AcquiredCard; it only swaps out the CardInstanceId
    Assert.Equal(i2, c.Db.AcquiredCard.Single().CardInstanceId)
    
    use db = c.Db
    db.AcquiredCard.AddI <|
        AcquiredCardEntity(
            CardInstanceId = i2,
            Due = DateTime.UtcNow,
            UserId = userId,
            CardSettingId = userId)
    let ex = Assert.Throws<DbUpdateException>(fun () -> db.SaveChanges() |> ignore)
    Assert.Equal(
        "Cannot insert duplicate key row in object 'dbo.AcquiredCard' with unique index 'IX_AcquiredCard_UserId_CardInstanceId'. The duplicate key value is (3, 1002).
The statement has been terminated.", 
        ex.InnerException.Message)

    let i1 = 1001
    use db = c.Db
    db.AcquiredCard.AddI <|
        AcquiredCardEntity(
            CardInstanceId = i1,
            Due = DateTime.UtcNow,
            UserId = userId,
            CardSettingId = userId)
    let ex = Assert.Throws<DbUpdateException>(fun () -> db.SaveChanges() |> ignore)
    Assert.Equal(
        "Cannot insert duplicate key row in object 'dbo.UserAndCard' with unique index 'IX_UserAndCard_UserId_CardId'. The duplicate key value is (3, 1).
The statement has been terminated.", 
        ex.InnerException.Message)
    }

[<Fact>]
let ``AcquireCards works``(): Task<unit> = task {
    use c = new TestContainer()
    
    let authorId = 3
    
    let c1 = 1
    let ci1_1 = 1001
    let! _ = FacetRepositoryTests.addBasicCard c.Db authorId []
    Assert.Equal(1, c.Db.Card.Single().Users)
    Assert.Equal(1, c.Db.CardInstance.Single().Users)
    Assert.Equal(1, c.Db.Card.Single(fun x -> x.Id = c1).Users)
    Assert.Equal(1, c.Db.CardInstance.Single(fun x -> x.Id = ci1_1).Users)
    
    let c2 = 2
    let ci2_1 = 1002
    let! _ = FacetRepositoryTests.addReversedBasicCard c.Db authorId []
    Assert.Equal(1, c.Db.Card.Single(fun x -> x.Id = c2).Users)
    Assert.Equal(1, c.Db.CardInstance.Single(fun x -> x.Id = ci2_1).Users)
    
    let acquirerId = 1
    do! CardRepository.AcquireCardAsync c.Db acquirerId ci1_1
    Assert.Equal(2, c.Db.Card.Single(fun x -> x.Id = c1).Users)
    Assert.Equal(2, c.Db.CardInstance.Single(fun x -> x.Id = ci1_1).Users)
    do! CardRepository.AcquireCardAsync c.Db acquirerId ci2_1
    Assert.Equal(2, c.Db.Card.Single(fun x -> x.Id = c2).Users)
    Assert.Equal(2, c.Db.CardInstance.Single(fun x -> x.Id = ci2_1).Users)
    // misc
    Assert.Equal(2, c.Db.CardInstance.Count())
    Assert.Equal(4, c.Db.AcquiredCard.Count())
    Assert.Equal(2, c.Db.AcquiredCard.Count(fun x -> x.CardInstanceId = ci1_1));

    let! ac = CardRepository.GetAcquired c.Db authorId c1
    let! v = SanitizeCardRepository.getEdit c.Db ci1_1
    let v = { v.Value with FieldValues = [].ToList() }
    let! x = CardRepository.UpdateFieldsToNewInstance c.Db ac.Value v.load
    Assert.Empty x.Value
    let ci1_2 = 1003
    Assert.Equal(2, c.Db.Card.Single(fun x -> x.Id = c1).Users)
    Assert.Equal(1, c.Db.CardInstance.Single(fun x -> x.Id = ci1_2).Users)
    // misc
    Assert.Equal(3, c.Db.CardInstance.Count())
    Assert.Equal(4, c.Db.AcquiredCard.Count())
    Assert.Equal(1, c.Db.AcquiredCard.Count(fun x -> x.CardInstanceId = ci1_2))
    
    do! CardRepository.AcquireCardAsync c.Db acquirerId ci1_2
    Assert.Equal(2, c.Db.Card.Single(fun x -> x.Id = c1).Users)
    Assert.Equal(2, c.Db.CardInstance.Single(fun x -> x.Id = ci1_2).Users)
    // misc
    Assert.Equal(3, c.Db.CardInstance.Count())
    Assert.Equal(4, c.Db.AcquiredCard.Count())
    Assert.Equal(2, c.Db.AcquiredCard.Count(fun x -> x.CardInstanceId = ci1_2));

    do! CardRepository.UnacquireCardAsync c.Db ac.Value.AcquiredCardId
    Assert.Equal(1, c.Db.Card.Single(fun x -> x.Id = c1).Users)
    Assert.Equal(1, c.Db.CardInstance.Single(fun x -> x.Id = ci1_2).Users)
    // misc
    Assert.Equal(3, c.Db.CardInstance.Count())
    Assert.Equal(3, c.Db.AcquiredCard.Count())
    Assert.Equal(1, c.Db.AcquiredCard.Count(fun x -> x.CardInstanceId = ci1_2));

    let count = CardRepository.GetDueCount c.Db acquirerId ""
    Assert.Equal(2, count)
    let count = CardRepository.GetDueCount c.Db authorId ""
    Assert.Equal(1, count)

    let! a = CardRepository.GetQuizBatch c.Db acquirerId ""
    let getId (x: Result<QuizCard, string> seq) = x.First() |> Result.getOk |> fun x -> x.AcquiredCardId
    do! SanitizeHistoryRepository.AddAndSaveAsync c.Db (getId a) Score.Easy DateTime.UtcNow (TimeSpan.FromDays(13.)) 0. (TimeSpan.FromSeconds 1.) (Interval <| TimeSpan.FromDays 13.)
    let! b = CardRepository.GetQuizBatch c.Db acquirerId ""
    Assert.NotEqual(getId a, getId b)

    let count = CardRepository.GetDueCount c.Db acquirerId ""
    Assert.Equal(1, count)

    // getHeatmap returns one for today
    let! actual = HistoryRepository.getHeatmap c.Db acquirerId
    Assert.Equal(0, actual.DateCountLevels.Length % 7) // returns full weeks; not partial weeks
    Assert.Equal(
        {   Date = DateTime.UtcNow.Date
            Count = 1
            Level = 10 },
        actual.DateCountLevels.Single(fun x -> x.Count <> 0)
    )}
