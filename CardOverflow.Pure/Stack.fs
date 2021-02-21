module Domain.Stack

open FsCodec
open FsCodec.NewtonsoftJson
open TypeShape
open FsToolkit.ErrorHandling

let streamName (id: StackId) = StreamName.create "Stack" (id.ToString())

// NOTE - these types and the union case names reflect the actual storage formats and hence need to be versioned with care
[<RequireQualifiedAccess>]
module Events =
    
    type Summary =
        { Id: StackId
          DefaultBranchId: BranchId
          AuthorId: UserId
          CopySourceLeafId: LeafId Option }
    type DefaultBranchChanged = { BranchId: BranchId }

    type Event =
        | DefaultBranchChanged of DefaultBranchChanged
        | Created              of Summary
        interface UnionContract.IUnionContract
    
    let codec = Codec.Create<Event> jsonSerializerSettings

module Fold =
    
    type State =
        | Initial
        | Active of Events.Summary
    let initial = State.Initial

    let evolve state =
        function
        | Events.Created s -> State.Active s
        | Events.DefaultBranchChanged b ->
            match state with
            | State.Initial  -> invalidOp "Can't change the default branch of an Initial Stack"
            | State.Active a -> { a with DefaultBranchId = b.BranchId } |> State.Active
    
    let fold : State -> Events.Event seq -> State = Seq.fold evolve
    let isOrigin = function Events.Created _ -> true | _ -> false

let decideCreate summary state =
    match state with
    | Fold.State.Active s -> Error $"Stack '{s.Id}' already exists."
    | Fold.State.Initial  -> Ok ()
    |> addEvent (Events.Created summary)

let decideDefaultBranchChanged (branchId: BranchId) (branchsStackId: StackId) callerId state =
    match state with
    | Fold.State.Initial  -> Error "Can't edit a branch that doesn't exist"
    | Fold.State.Active s -> result {
        do! Result.requireEqual s.AuthorId callerId $"Stack {s.Id} doesn't belong to User {callerId}"
        do! Result.requireEqual s.Id branchsStackId $"Branch {branchId} doesn't belong to Stack {s.Id}"
    } |> addEvent (Events.DefaultBranchChanged { BranchId = branchId })
