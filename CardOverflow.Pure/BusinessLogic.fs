namespace CardOverflow.Pure

open CardOverflow.Debug
open System.Linq
open CardOverflow.Pure.Core
open System
open Microsoft.FSharp.Core.Operators.Checked
open System.ComponentModel.DataAnnotations
open System.Text.RegularExpressions
open CardOverflow.Debug

module Relationship =
    type RelationshipRegex = FSharp.Text.RegexProvider.Regex< """(?<source>.+)\/(?<target>.+)""" >
    let isDirectional = RelationshipRegex().IsMatch
    let split name =
        let x = RelationshipRegex().TypedMatch name
        if x.Success then
            x.source.Value, x.target.Value
        else
            name, ""
    let flipName name =
        let x = RelationshipRegex().TypedMatch name
        if x.Success then
            x.target.Value + "/" + x.source.Value
        else
            name

module Cloze =
    let isCloze questionXemplate =
        ClozeTemplateRegex().IsMatch questionXemplate

module CardHtml =
    let generate fieldNameValueMap questionXemplate answerXemplate css i =
        let questionXemplate, answerXemplate =
            fieldNameValueMap
            |> List.filter(fun (_, value) -> ClozeRegex().IsMatch value)
            |> List.tryExactlyOne
            |> function
            | None -> questionXemplate, answerXemplate
            | Some (fieldName, _) ->
                let irrelevantCloze = Regex <| "{{cloze:(?!" + fieldName + ").+?}}"
                irrelevantCloze.Replace(questionXemplate, ""), irrelevantCloze.Replace(answerXemplate, "")
        let replaceFields isFront template =
            (template, fieldNameValueMap)
            ||> List.fold(fun (previous: string) (fieldName, value) -> 
                let simple =
                    previous.Replace("{{" + fieldName + "}}", value)
                let showIfHasText =
                    let regex = Regex <| "{{#" + fieldName + @"}}(.*?){{\/" + fieldName + "}}"
                    if String.IsNullOrWhiteSpace value
                    then regex.Replace(simple, "")
                    else regex.Replace(simple, "$1")
                let showIfEmpty =
                    let regex = Regex <| @"{{\^" + fieldName + @"}}(.*?){{\/" + fieldName + "}}"
                    if String.IsNullOrWhiteSpace value
                    then regex.Replace(showIfHasText, "$1")
                    else regex.Replace(showIfHasText, "")
                let stripHtml =
                    showIfEmpty.Replace("{{text:" + fieldName + "}}", MappingTools.stripHtmlTags value)
                let cloze =
                    if isFront then
                        let regexMatches = ClozeRegex().TypedMatches(value).Select(fun x -> x.hint, x.Value) |> List.ofSeq
                        (value, regexMatches) ||> List.fold(fun current (hintGroup, rawCloze) ->
                            let hint =
                                if hintGroup.Success then
                                    hintGroup.Value
                                else
                                    "..."
                            let brackets = hint |> sprintf """
        <span class="cloze-brackets-front">[</span>
        <span class="cloze-filler-front">%s</span>
        <span class="cloze-brackets-front">]</span>
        """
                            current.Replace(rawCloze, brackets)
                        ) |> fun x -> stripHtml.Replace("{{cloze:" + fieldName + "}}", x)
                    else
                        let html =
                            sprintf """
        <span class="cloze-brackets-back">[</span>
        %s
        <span class="cloze-brackets-back">]</span>
        """
                        let answer = ClozeRegex().TypedReplace(value, fun f -> html f.answer.Value)
                        stripHtml.Replace("{{cloze:" + fieldName + "}}", answer)
                cloze
            )
        let frontSide =
            replaceFields true questionXemplate
        let backSide =
            (replaceFields false answerXemplate).Replace("{{FrontSide}}", replaceFields false questionXemplate)
        let htmlBase =
            sprintf """<!DOCTYPE html>
    <head>
        <style>
            .cloze-brackets-front {
                font-size: 150%%;
                font-family: monospace;
                font-weight: bolder;
                color: dodgerblue;
            }
            .cloze-filler-front {
                font-size: 150%%;
                font-family: monospace;
                font-weight: bolder;
                color: dodgerblue;
            }
            .cloze-brackets-back {
                font-size: 150%%;
                font-family: monospace;
                font-weight: bolder;
                color: red;
            }
        </style>
        <style>
            %s
        </style>
    </head>
    <body>
        %s
        <script type="text/javascript" src="/js/iframeResizer.contentWindow.min.js"></script> 
    </body>
</html>"""
                css
        htmlBase frontSide,
        htmlBase backSide,
        MappingTools.stripHtmlTagsForDisplay <| frontSide,
        MappingTools.stripHtmlTagsForDisplay <| (replaceFields false answerXemplate).Replace("{{FrontSide}}", "")

type DateCount = {
    Date: DateTime
    Count: int
}
type DateCountLevel = {
    Date: DateTime
    Count: int
    Level: int
}
type Heatmap = {
    DateCountLevels: DateCountLevel list
    DailyAverageReviews: int
    DaysLearnedPercent: int
    LongestStreakDays: int
    CurrentStreakDays: int
}
module Heatmap =
    let maxConseuctive =
        let rec maxConseuctive localMax globalMax =
            function
            | h :: t ->
                if h = 0 then
                    maxConseuctive 0 globalMax t
                else
                    let localMax = localMax + 1
                    maxConseuctive localMax (max localMax globalMax) t
            | [] -> globalMax
        maxConseuctive 0 0
    let allDateCounts startDate (endDate: DateTime) (dateCounts: DateCount list) =
        let allDatesSorted = startDate |> List.unfold (fun x -> if x <= endDate then Some(x, x.AddDays 1.) else None) // https://stackoverflow.com/a/20362003
        query { // https://stackoverflow.com/a/26008852
            for date in allDatesSorted do
            leftOuterJoin dateCount in dateCounts
                on (date = dateCount.Date) into result
            for dateCount in result do
            select {
                Date = date
                Count = dateCount |> toOption |> Option.map (fun x -> x.Count) |> Option.defaultValue 0
            }
        } |> List.ofSeq
    let addLevels (dateCounts: DateCount list) =
        let levelCount = 11. // 0 to 10 is 11
        let maxCount = dateCounts |> List.map (fun x -> x.Count) |> List.ifEmptyThen 0 |> List.max |> max 1 |> float
        dateCounts |> List.map (fun { Date = date; Count = count } ->
        {   Date = date
            Count = count
            Level = float count / maxCount * (levelCount - 1.) |> round // -1 to remove 0
        })
    let get (startDate: DateTime) (endDate: DateTime) (dateCounts: DateCount list) =
        let dateCounts = allDateCounts startDate.Date endDate.Date dateCounts
        let counts = dateCounts |> List.map (fun x -> x.Count)
        let relevantRange = counts |> List.skipWhile (fun x -> x = 0) |> List.ifEmptyThen 0
        {   DateCountLevels = addLevels dateCounts
            DailyAverageReviews = relevantRange |> List.averageBy (fun x -> float x) |> round
            DaysLearnedPercent = float (relevantRange.Count(fun x -> x <> 0)) / float relevantRange.Length * 100. |> round 
            LongestStreakDays = counts |> maxConseuctive
            CurrentStreakDays = counts |> Seq.rev |> Seq.takeWhile (fun x -> x <> 0) |> Seq.length
        }
