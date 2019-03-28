module ConceptRepositoryTests

open CardOverflow.Api
open CardOverflow.Entity
open CardOverflow.Test
open System
open System.Linq
open Xunit

[<Fact>]
let ``ConceptRepository can add and retreive a Concept``() =
  use tempDb = new TempDbService()
  let service = tempDb.RecreateDatabaseAndGetDbService()
  let conceptRepository = service |> ConceptRepository
  let concept = Concept(Title = "", Description = "")
  service.Command(fun db -> db.Concepts.Add(concept))
  let title = Guid.NewGuid().ToString()
  let description = Guid.NewGuid().ToString()

  conceptRepository.SaveConcept(Concept(Title = title, Description = description))

  conceptRepository.GetConcepts()
  |> Seq.filter(fun x -> x.Title = title && x.Description = description)
  |> Assert.Single

[<Fact>]
let ``ConceptRepository's SaveConcepts updates a Concept``() =
  use tempDb = new TempDbService()
  let service = tempDb.RecreateDatabaseAndGetDbService()
  let conceptRepository = service |> ConceptRepository
  service.Command(fun db -> db.Concepts.Add(Concept(Title = "", Description = "")))
  let updatedTitle = Guid.NewGuid().ToString()
  let updatedDescription = Guid.NewGuid().ToString()
  let updatedConcept = conceptRepository.GetConcepts().Single()
  updatedConcept.Title <- updatedTitle
  updatedConcept.Description <- updatedDescription

  updatedConcept 
  |> Seq.singleton 
  |> ResizeArray<Concept>
  |> conceptRepository.SaveConcepts

  conceptRepository.GetConcepts()
  |> Seq.filter(fun x -> x.Title = updatedTitle && x.Description = updatedDescription)
  |> Assert.Single

[<Fact>]
let ``ConceptRepository's SaveConcepts can add card to existing Concept``() =
  use tempDb = new TempDbService()
  let service = tempDb.RecreateDatabaseAndGetDbService()
  let conceptRepository = service |> ConceptRepository
  service.Command(fun db -> db.Concepts.Add(Concept(Title = "", Description = "")))
  let question = Guid.NewGuid().ToString()
  let answer = Guid.NewGuid().ToString()
  let updatedConcept = conceptRepository.GetConcepts().Single()
  updatedConcept.Cards.Add(Card(Question = question, Answer = answer))

  updatedConcept
  |> Seq.singleton 
  |> ResizeArray<Concept>
  |> conceptRepository.SaveConcepts

  conceptRepository.GetConcepts()
  |> Seq.filter(fun x -> x.Cards.Single().Question = question && x.Cards.Single().Answer = answer )
  |> Assert.Single


[<Fact>]
let ``ConceptRepository's SaveConcepts can add a Concept with a card``() =
  use tempDb = new TempDbService()
  let service = tempDb.RecreateDatabaseAndGetDbService()
  let conceptRepository = service |> ConceptRepository
  let question = Guid.NewGuid().ToString()
  let answer = Guid.NewGuid().ToString()

  Card(Question = question, Answer = answer)
  |> Seq.singleton
  |> ResizeArray<Card>
  |> fun cards -> Concept(Title = "", Description = "", Cards = cards)
  |> Seq.singleton 
  |> ResizeArray<Concept>
  |> conceptRepository.SaveConcepts

  conceptRepository.GetConcepts()
  |> Seq.filter(fun x -> x.Cards.Single().Question = question && x.Cards.Single().Answer = answer )
  |> Assert.Single
