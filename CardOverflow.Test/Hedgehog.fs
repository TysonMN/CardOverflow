module Hedgehog

open Hedgehog
open CardOverflow.Pure
open CardOverflow.Test

// lowTODO this tries to shrink down to 1 element, which may be semantically incorrect depending on use case
module SeqGen =
    let traverse (f: Gen<'a> -> Gen<'b>) (ma: seq<Gen<'a>>): Gen<list<'b>> =
        let mutable cache = ResizeArray()
        gen {
            for a in ma do
                let! b = f a
                cache.Add b
            let r = cache |> Seq.toList 
            cache <- ResizeArray()
            return r
        }

    let sequence ma = traverse id ma 

let unicode max = Gen.string (Range.constant 1 max) Gen.unicode
let standardTemplate fields =
    gen {
        let templateGen =
            gen {
                let! name = unicode 100
                let! front = Gen.item fields
                let! back  = Gen.item fields
                return
                    {   Name = name
                        Front = "{{" + front + "}}"
                        Back = "{{FrontSide}}<hr id=answer>{{" + back + "}}"
                        ShortFront = ""
                        ShortBack = ""
                    }
            }
        let! templates = GenX.cList 1 100 templateGen
        return Standard templates
    }
let clozeTemplate fields =
    gen {
        let! name  = unicode 100
        let! text  = Gen.item fields
        let! extra = Gen.item fields
        return
            {   Name = name
                Front = "{{cloze:" + text + "}}"
                Back = "{{cloze:" + text + "}}<br>{{" + extra + "}}"
                ShortFront = ""
                ShortBack = ""
            } |> Cloze
    }
let gromplateType fields =
    Gen.choice [
        standardTemplate fields
        clozeTemplate fields
    ]

let instantGen = GenX.auto |> Gen.map NodaTime.Instant.FromDateTimeOffset

let fields = List.map (fun fieldName -> GenX.auto<Field> |> Gen.map(fun field -> { field with Name = fieldName })) >> SeqGen.sequence
let grompleaf gromplateType fieldNames =
    gen {
        let! fields = fieldNames |> fields
        let! id = Gen.guid
        let! name = Gen.latin1 |> GenX.lString 0 50
        let! gromplateId = Gen.guid
        let! css = Gen.latin1 |> GenX.lString 0 50
        let! created = instantGen
        let! modified = instantGen
        let! latexPre  = Gen.latin1 |> GenX.lString 0 50
        let! latexPost = Gen.latin1 |> GenX.lString 0 50
        let! editSummary = Gen.latin1 |> GenX.lString 0 50
        return {
            Id = id
            Name = name
            GromplateId = gromplateId
            Css = css
            Fields = fields
            Created = created
            Modified = Some modified
            LatexPre = latexPre
            LatexPost = latexPost
            Templates = gromplateType
            EditSummary = editSummary
        }
    }

let clozeText =
    gen { // medTODO make more realistic
        let! a = Gen.alphaNum |> GenX.cString 1 100
        let! b = Gen.alphaNum |> GenX.cString 1 100
        let! c = Gen.alphaNum |> GenX.cString 1 100
        return sprintf "%s{{c1::%s}}%s" a b c
    }
let editStackCommand =
    gen {
        let! fieldNames =
            Gen.unicode
            |> Gen.string (Range.constant 1 100)
            |> GenX.cList 1 100
        let! gromplateType = gromplateType fieldNames
        let! grompleaf = grompleaf gromplateType fieldNames
        let values =
            match gromplateType with
            | Standard _ -> Gen.alphaNum |> Gen.string (Range.constant 1 100)
            | Cloze _ -> clozeText
        let! fields = fields fieldNames
        let! fields =
            fields
            |> List.map (fun f -> values |> Gen.map (fun value -> { EditField = f; Value = value }))
            |> SeqGen.sequence
        let! editSummary = Gen.latin1 |> GenX.lString 0 50
        let! kind = GenX.auto<UpsertKind>
        let! ids = GenX.auto<UpsertIds>
        return {
            EditSummary = editSummary
            FieldValues = fields |> toResizeArray
            Grompleaf = grompleaf
            Kind = kind
            Ids = ids
        }
    }

open Hedgehog.Xunit
type StandardConfig =
    static member __ =
        GenX.defaults
        |> AutoGenConfig.addGenerator editStackCommand
        |> AutoGenConfig.addGenerator instantGen


type StandardProperty(i) =
    inherit PropertyAttribute(typeof<StandardConfig>, LanguagePrimitives.Int32WithMeasure i)
    new () = StandardProperty(100)