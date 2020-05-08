namespace CardOverflow.Pure

open System.Runtime.InteropServices
open CardOverflow.Pure.Extensions
open CardOverflow.Pure.Core
open System
open System.Linq
open Microsoft.FSharp.Core.Operators.Checked
open System.ComponentModel.DataAnnotations

type Score = | Again | Hard | Good | Easy
module Score =
    let create =
        function
        | 0s -> Ok Again
        | 1s -> Ok Hard
        | 2s -> Ok Good
        | 3s -> Ok Easy
        | x -> sprintf "Invalid Score in database: %A" x |> Error
    let toDb =
        function
        | Again -> 0
        | Hard -> 1
        | Good -> 2
        | Easy -> 3
        >> int16

type CardState = | Normal | SchedulerBuried | UserBuried | Suspended
module CardState =
    let create =
        function
        | 0s -> Ok Normal
        | 1s -> Ok SchedulerBuried
        | 2s -> Ok UserBuried
        | 3s -> Ok Suspended
        | x -> sprintf "Invalid CardState in database: %A" x |> Error
    let toDb =
        function
        | Normal -> 0
        | SchedulerBuried -> 1
        | UserBuried -> 2
        | Suspended -> 3
        >> int16

module TimeSpanInt16 =
    type TimeSpanInt16 = private TimeSpanInt16 of TimeSpan
    let fromDays days = min (float Int16.MaxValue) days |> TimeSpan.FromDays |> TimeSpanInt16
    let value (TimeSpanInt16 t) = t
    let totalDays t = (value t).TotalDays |> int16

type CardSetting = {
    Id: int
    Name: string
    IsDefault: bool
    NewCardsSteps: TimeSpan list
    NewCardsMaxPerDay: int16
    NewCardsGraduatingInterval: TimeSpan
    NewCardsEasyInterval: TimeSpan
    NewCardsStartingEaseFactor: float
    NewCardsBuryRelated: bool
    MatureCardsMaxPerDay: int16
    MatureCardsEaseFactorEasyBonusFactor: float
    MatureCardsIntervalFactor: float // medTODO unused
    MatureCardsMaximumInterval: TimeSpanInt16.TimeSpanInt16
    MatureCardsHardIntervalFactor: float
    MatureCardsBuryRelated: bool
    LapsedCardsSteps: TimeSpan list
    LapsedCardsNewIntervalFactor: float // percent by which to multiply the current interval when a card goes has lapsed, called "new interval" in anki gui
    LapsedCardsMinimumInterval: TimeSpan
    LapsedCardsLeechThreshold: int16
    ShowAnswerTimer: bool
    AutomaticallyPlayAudio: bool
    ReplayQuestionAudioOnAnswer: bool
}

type Field = {
    Name: string
    IsRightToLeft: bool
    Ordinal: byte
    IsSticky: bool
} with
    member this.toString =
        [   this.Name
            this.IsRightToLeft |> string
            this.Ordinal |> string
            this.IsSticky |> string
        ] |> MappingTools.joinByUnitSeparator
    static member fromString x =
        let x = x |> MappingTools.splitByUnitSeparator
        {   Name = x.[0]
            IsRightToLeft = x.[1] = "True"
            Ordinal = x.[2] |> byte
            IsSticky = x.[3] = "True"
        }

module Fields =
    let toString: (Field seq -> string) =
        Seq.map (fun x -> x.toString)
        >> MappingTools.joinByRecordSeparator
    let fromString =
        MappingTools.splitByRecordSeparator
        >> List.map Field.fromString

type TemplateInstance = {
    Id: int
    Name: string
    TemplateId: int
    Css: string
    Fields: Field list
    Created: DateTime
    Modified: DateTime option
    LatexPre: string
    LatexPost: string
    QuestionTemplate: string
    AnswerTemplate: string
    ShortQuestionTemplate: string
    ShortAnswerTemplate: string
    EditSummary: string
} with
    member this.IsCloze =
        Cloze.isCloze this.QuestionTemplate
    member this.ClozeFields =
        AnkiImportLogic.clozeFields this.QuestionTemplate
    member this.FrontBackFrontSynthBackSynth =
        CardHtml.generate [] this.QuestionTemplate this.AnswerTemplate this.Css

type AcquiredTemplateInstance = {
    DefaultTags: int seq
    DefaultCardSettingId: int
    TemplateInstance: TemplateInstance
}

type Template = {
    Id: int
    AuthorId: int
    LatestInstance: TemplateInstance
}

type IntervalOrStepsIndex =
    | NewStepsIndex of byte
    | LapsedStepsIndex of byte
    | Interval of TimeSpan

type QuizCard = {
    AcquiredCardId: int
    BranchInstanceId: int
    Due: DateTime
    Front: string
    Back: string
    FrontSynthVoice: string
    BackSynthVoice: string
    CardState: CardState
    IsLapsed: bool
    EaseFactor: float
    IntervalOrStepsIndex: IntervalOrStepsIndex
    Settings: CardSetting
}

//type BranchInstance = {
//    Id: int
//    Created: DateTime
//    Modified: DateTime option
//    Fields: string seq
//}

// medTODO delete?
//type AcquiredDisplayCard = { // Acquired cause only private tags can be on a card
//    TemplateName: string
//    Front: string
//    Back: string
//    Tags: string seq
//}

[<CLIMutable>]
type FieldAndValue = {
    Field: Field
    Value: string
}

module IntervalOrStepsIndex =
    //                 255            |             255             |            1439         | <- The value of this is 1, not 0, cause 0 days is 0 minutes
    //       |------------------------|-----------------------------|-------------------------|-------------------|
    //           New Step Indexes   n1|l0   Lapsed Step Indexes   l1|m0         Minutes       |d0      Days
    let minutesInADay = TimeSpan.FromDays(1.).TotalMinutes
    let n1 = Int16.MinValue + int16 Byte.MaxValue |> float
    let l0 = n1 + 1.
    let l1 = l0 + float Byte.MaxValue
    let m0 = l1 + 1.
    let d0 = m0 + float minutesInADay
    let intervalFromDb (x: int16) =
        let x = float x
        if x <= n1
        then x - float Int16.MinValue |> byte |> NewStepsIndex
        elif x > d0 // exclusive because we start counting at 1
        then x - d0 |> float |> TimeSpan.FromDays |> Interval
        elif l0 <= x && x <= l1
        then x - float l0 |> byte |> LapsedStepsIndex
        else x - m0 |> float |> TimeSpan.FromMinutes |> Interval
    let intervalToDb =
        function
        | NewStepsIndex x ->
            int16 x + Int16.MinValue
        | LapsedStepsIndex x ->
            int16 x + int16 l0
        | Interval x ->
            if x.TotalMinutes >= minutesInADay
            then x.TotalDays + d0
            else x.TotalMinutes + m0
            |> int16

//[<CLIMutable>]
//type AcquiredConcept = {
//    Id: int
//    // medTODO 100 needs to be tied to the DB max somehow
//    [<StringLength(100, ErrorMessage = "Name must be less than 100 characters.")>] Name: string
//    AuthorId: int
//    AcquiredCards: AcquiredCard ResizeArray
//}

type PagedListDetails = {
    CurrentPage: int
    PageCount: int
}

type PagedList<'T> = {
    Results: 'T seq
    Details: PagedListDetails
}

[<CLIMutable>]
type CommunalFieldValue = {
    InstanceId: int option
    CommunalBranchInstanceIds: int ResizeArray
}

[<CLIMutable>]
type EditFieldAndValue = {
    EditField: Field
    [<StringLength(10000)>]
    Value: string
    Communal: CommunalFieldValue option
} with
    member this.IsCommunal =
        match this.Communal with
        | Some _ -> true
        | _ -> false
    member this.CommunalBranchInstanceIds =
        match this.Communal with
        | Some x -> x.CommunalBranchInstanceIds
        | None -> [].ToList()

type BranchInstanceView = {
    FieldValues: FieldAndValue ResizeArray
    TemplateInstance: TemplateInstance
} with
    member this.FrontBackFrontSynthBackSynth = // medTODO split this up
        CardHtml.generate
            <| this.FieldValues.Select(fun x -> x.Field.Name, x.Value |?? lazy "").ToFList()
            <| this.TemplateInstance.QuestionTemplate
            <| this.TemplateInstance.AnswerTemplate
            <| this.TemplateInstance.Css

type CommunalFieldInstance = {
    Id: int
    FieldName: string
    Value: string
}

[<CLIMutable>]
type ViewTag = {
    Name: string
    Count: int
    IsAcquired: bool
}

[<CLIMutable>]
type ViewRelationship = {
    Name: string
    SourceCardId: int
    TargetCardId: int
    IsAcquired: bool
    Users: int
} with
    member this.PrimaryName =
        Relationship.split this.Name |> fst
    member this.SecondaryName =
        Relationship.split this.Name |> snd

[<CLIMutable>]
type BranchInstanceMeta = {
    Id: int
    CardId: int
    Created: DateTime
    Modified: DateTime option
    IsDmca: bool
    IsAcquired: bool
    IsLatest: bool
    StrippedFront: string
    StrippedBack: string
    CommunalFields: CommunalFieldInstance ResizeArray
    Users: int
}

[<CLIMutable>]
type AcquiredCard = {
    CardId: int
    BranchId: int
    AcquiredCardId: int
    UserId: int
    BranchInstanceMeta: BranchInstanceMeta
    CardState: CardState
    IsLapsed: bool
    EaseFactorInPermille: int16
    IntervalOrStepsIndex: IntervalOrStepsIndex
    Due: DateTime
    CardSettingId: int
    Tags: string list
}

type Comment = {
    User: string
    UserId: int
    Text: string
    Created: DateTime
    IsDmca: bool
}

[<CLIMutable>]
type ExploreBranchSummary = {
    Id: int
    Users: int
    Author: string
    AuthorId: int
    Instance: BranchInstanceMeta
} with
    member this.IsAcquired = this.Instance.IsAcquired

type ExploreCardAcquiredStatus =
    | ExactInstanceAcquired of int // card instance ids
    | OtherInstanceAcquired of int
    | LatestBranchAcquired of int
    | OtherBranchAcquired of int
    | NotAcquired
    with
        member this.InstanceId =
            match this with
            | ExactInstanceAcquired x -> Some x
            | OtherInstanceAcquired x -> Some x
            | LatestBranchAcquired x -> Some x
            | OtherBranchAcquired x -> Some x
            | NotAcquired -> None

type Branch = {
    Name: string
    Summary: ExploreBranchSummary
} with
    member this.Id = this.Summary.Id
    member this.Users = this.Summary.Users
    member this.Author = this.Summary.Author
    member this.AuthorId = this.Summary.AuthorId
    member this.Instance = this.Summary.Instance

[<CLIMutable>]
type ExploreCard = {
    Summary: ExploreBranchSummary
    Tags: ViewTag ResizeArray
    Relationships: ViewRelationship ResizeArray
    Comments: Comment ResizeArray
    AcquiredStatus: ExploreCardAcquiredStatus
    Branches: Branch ResizeArray
} with
    member this.Id = this.Summary.Id
    //don't add users - the UI needs it to be mutable
    member this.Author = this.Summary.Author
    member this.AuthorId = this.Summary.AuthorId
    member this.Instance = this.Summary.Instance
    member this.IsAnyAcquired =
        match this.AcquiredStatus with
        | NotAcquired -> false
        | _ -> true

type BranchRevision = {
    Id: int
    Author: string
    AuthorId: int
    SortedMeta: BranchInstanceMeta list
}

type CardSource =
    | Original
    | CopySourceInstanceId of int
    | BranchSourceCardId of int
with
    member this.TryGetCopySourceInstanceId([<Out>] x:byref<_>) = // https://stackoverflow.com/a/17264768
        match this with
        | CopySourceInstanceId instanceId -> x <- instanceId; true
        | _ -> false
    member this.TryGetBranchSourceCardId([<Out>] x:byref<_>) =
        match this with
        | BranchSourceCardId cardId -> x <- cardId; true
        | _ -> false
    

type EditCardCommand = {
    EditSummary: string
    FieldValues: EditFieldAndValue ResizeArray
    TemplateInstance: TemplateInstance
    Source: CardSource
} with
    member this.CardView = {   
        FieldValues =
            this.FieldValues.Select(fun x ->
                {   Field = x.EditField
                    Value =  x.Value
                }).ToList()
        TemplateInstance = this.TemplateInstance }
    member this.CommunalFieldValues =
        this.FieldValues.Where(fun x -> x.IsCommunal).ToList()
    member this.CommunalNonClozeFieldValues =
        this.CommunalFieldValues
            .Where(fun x -> not <| this.TemplateInstance.ClozeFields.Contains x.EditField.Name)
            .ToList()
    member this.ClozeFieldValues =
        this.CommunalFieldValues
            .Where(fun x -> this.TemplateInstance.ClozeFields.Contains x.EditField.Name)
            .ToList()
