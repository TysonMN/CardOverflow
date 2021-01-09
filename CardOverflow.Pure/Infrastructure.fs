[<AutoOpen>]
module Domain.Infrastructure

open FSharp.UMX
open System

type UserId = Guid<userId>
    and [<Measure>] userId

type DeckId = Guid<deckId>
    and [<Measure>] deckId
type CardSettingId = Guid<cardSettingId>
    and [<Measure>] cardSettingId

type StackId = Guid<stackId>
    and [<Measure>] stackId
type BranchId = Guid<branchId>
    and [<Measure>] branchId
type LeafId = Guid<leafId>
    and [<Measure>] leafId

type GromplateId = Guid<gromplateId>
    and [<Measure>] gromplateId
type GrompleafId = Guid<grompleafId>
    and [<Measure>] grompleafId