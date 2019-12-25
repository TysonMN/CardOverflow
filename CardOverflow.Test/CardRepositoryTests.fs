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
let ``AcquireCards works``(): Task<unit> = task {
    use c = new TestContainer()
    
    let authorId = 3
    
    let c1 = 1
    let ci1_1 = 1
    do! FacetRepositoryTests.addBasicCard c.Db authorId []
    Assert.Equal(1, c.Db.Card.Single().Users)
    Assert.Equal(1, c.Db.CardInstance.Single().Users)
    Assert.Equal(1, c.Db.Card.Single(fun x -> x.Id = c1).Users)
    Assert.Equal(1, c.Db.CardInstance.Single(fun x -> x.Id = ci1_1).Users)
    
    let c2 = 2
    let ci2_1 = 2
    do! FacetRepositoryTests.addReversedBasicCard c.Db authorId []
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
    let ac = Result.getOk ac
    let! v = SanitizeCardRepository.getEdit c.Db c1
    let v = { Result.getOk v with FieldValues = [].ToList() }
    let! x = CardRepository.UpdateFieldsToNewInstance c.Db ac v.load
    Result.getOk x
    let ci1_2 = 3
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

    do! CardRepository.UnacquireCardAsync c.Db ac.AcquiredCardId
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
    Assert.NotEmpty(c.Db.History)

    let count = CardRepository.GetDueCount c.Db acquirerId ""
    Assert.Equal(1, count)
    }
