namespace CardOverflow.Api

open CardOverflow.Pure
open System
open Serilog
open NodaTime

module Scheduler =
    let max a b = if a > b then a else b
    let min a b = if a < b then a else b
    let equals a b threshold = abs(a-b) < threshold

    let rawInterval utcNow (card: QuizCard) score =
        let calculateStepsInterval toStep stepTimespans graduatingInterval easyInterval stepIndex =
            match score with
            | Again -> stepTimespans |> List.head, toStep 0uy
            | Hard ->
                match stepTimespans |> List.tryItem (int stepIndex) with
                | Some span -> span, toStep stepIndex
                | None ->
                    sprintf "Hard was chosen for QuizCard %A and it had %i as its stepIndex - an illegal value." card stepIndex |> Log.Error
                    stepTimespans.Head, toStep 0uy
            | Good ->
                let stepIndex = stepIndex + 1uy
                match stepTimespans |> List.tryItem (int stepIndex) with
                | Some span -> span, toStep stepIndex
                | None -> graduatingInterval, IntervalXX graduatingInterval
            | Easy -> easyInterval, IntervalXX easyInterval
        let calculateInterval previousInterval =
            let interval (previousInterval: Duration) (rawInterval: Duration) =
                max (rawInterval * card.Settings.MatureCardsIntervalFactor)
                    (Duration.FromDays 1. |> (+) previousInterval)
                |> min card.Settings.MatureCardsMaximumInterval
            let delta = utcNow - card.Due |> max Duration.Zero
            let hard = interval previousInterval <| previousInterval * card.Settings.MatureCardsHardIntervalFactor
            let good = interval hard (delta * 0.5 |> (+) previousInterval |> fun x -> x * card.EaseFactor)
            let easy = interval good (delta * 1.  |> (+) previousInterval |> fun x -> x * card.EaseFactor) |> fun x -> x * card.Settings.MatureCardsEaseFactorEasyBonusFactor
            match score with
            | Again -> card.Settings.NewCardsSteps.Head, 0.
            | Hard -> hard, card.EaseFactor - 0.15
            | Good -> good, card.EaseFactor
            | Easy -> easy, max (card.EaseFactor + 0.15) 1.3
        match card.IntervalOrStepsIndex with
        | NewStepsIndex i ->
            calculateStepsInterval
                NewStepsIndex
                card.Settings.NewCardsSteps
                card.Settings.NewCardsGraduatingInterval
                card.Settings.NewCardsEasyInterval
                i,
            card.Settings.NewCardsStartingEaseFactor
        | LapsedStepsIndex i ->
            calculateStepsInterval
                LapsedStepsIndex
                card.Settings.LapsedCardsSteps
                card.Settings.NewCardsGraduatingInterval // medTODO consider an option for this
                card.Settings.NewCardsGraduatingInterval // medTODO actually the card settings are all screwed up, refactor the entire scheduler later when you figure out how the hell the Anki one works
                i,
            card.Settings.LapsedCardsNewIntervalFactor
        | IntervalXX i ->
            let interval, ease = calculateInterval i
            (interval, IntervalXX interval), ease

open Scheduler
type Scheduler(randomProvider: RandomProvider, time: TimeProvider) =
    let fuzz(interval: Duration) =
        let fuzzRangeInDaysInclusive =
            let days = interval.TotalDays
            let atLeastOneDay = max 1.
            let buildFuzzierInterval = atLeastOneDay >> fun x -> (days - x, days + x)
            if days < 2.              then (1., 1.)
            elif equals days 2. 0.001 then (2., 3.)
            elif days < 7.  then        (days * 0.25) |> buildFuzzierInterval
            elif days < 30. then max 2. (days * 0.15) |> buildFuzzierInterval
            else                 max 4. (days * 0.05) |> buildFuzzierInterval
        // lowTODO find an implementation that is max inclusive
        randomProvider.float fuzzRangeInDaysInclusive |> Duration.FromDays

    member _.Calculate (card: QuizCard) score =
        rawInterval time.utcNow card score
        |> fun ((interval, intervalOrSteps), easeFactor) -> fuzz interval, intervalOrSteps, easeFactor

    member _.Intervals (card: QuizCard) =
        rawInterval time.utcNow card Again |> fst |> fst |> ViewLogic.toString,
        rawInterval time.utcNow card Hard  |> fst |> fst |> ViewLogic.toString,
        rawInterval time.utcNow card Good  |> fst |> fst |> ViewLogic.toString,
        rawInterval time.utcNow card Easy  |> fst |> fst |> ViewLogic.toString
