open System

open System;
open System.Globalization
open System.IO

type Event = 
    | WakesUp
    | FallsAsleep
    | GuardBeginsShift of id:int

type Entry = { DateTime: DateTime; Event: Event; }

let parse (str:string) : Entry = 
    let time = 
        let rawTime = str.Substring(0, 19).Trim('[',']', ' ') 
        Console.WriteLine rawTime
        DateTime.ParseExact (rawTime, "yyyy-MM-dd HH:mm", CultureInfo.InvariantCulture)
        
    let event = 
        let raw = str.Substring(19).Trim()
        match raw with
        | "wakes up" -> WakesUp
        | "falls asleep" -> FallsAsleep
        | raw -> 
            let rawId = 
                raw.Split(' ') |> Array.find (fun x -> x.StartsWith "#" )
            let intId = rawId.Trim('#') |> int32
            GuardBeginsShift intId

    { DateTime = time; Event = event }

let input = 
    File.ReadAllLines( __SOURCE_DIRECTORY__ + "\Day4.txt" )
    |> Array.map parse
    |> Array.sortBy (fun it -> it.DateTime)


let testInput = 
    [|
        "[1518-11-01 00:00] Guard #10 begins shift"
        "[1518-11-01 00:05] falls asleep"
        "[1518-11-01 00:25] wakes up"
        "[1518-11-01 00:30] falls asleep"
        "[1518-11-01 00:55] wakes up"
        "[1518-11-01 23:58] Guard #99 begins shift"
        "[1518-11-02 00:40] falls asleep"
        "[1518-11-02 00:50] wakes up"
        "[1518-11-03 00:05] Guard #10 begins shift"
        "[1518-11-03 00:24] falls asleep"
        "[1518-11-03 00:29] wakes up"
        "[1518-11-04 00:02] Guard #99 begins shift"
        "[1518-11-04 00:36] falls asleep"
        "[1518-11-04 00:46] wakes up"
        "[1518-11-05 00:03] Guard #99 begins shift"
        "[1518-11-05 00:45] falls asleep"
        "[1518-11-05 00:55] wakes up"
    |] |> Array.map parse |> Array.sortBy (fun it -> it.DateTime)


type Schedule = Map<GuardId, SleepPeriods>
and GuardId = int
and SleepPeriods = SleepPeriod list
and SleepPeriod = { Duration : Duration ; Range : MinuteRange }
and Duration = int
and MinuteRange = int * int

let zero : Schedule = Map.empty

let buildSchedule events : Schedule = 
    
    let rec go events ((guardId, schedule) as acc) = 
        match events with
        | { Event = GuardBeginsShift id } :: rest -> 
            go rest (id, schedule)

        | { Event = FallsAsleep; DateTime = start } :: { Event = WakesUp; DateTime = end' }:: rest ->

            let list = schedule |> Map.tryFind guardId |> Option.defaultValue []
            let duration = (end' - start).TotalMinutes |> int
            
            let startMinute = start.TimeOfDay.Minutes
            let endMinute = end'.TimeOfDay.Minutes

            let newEntry = { Duration = duration; Range = ( startMinute , endMinute ) }

            let newList = newEntry :: list
            let newSchedule = schedule |> Map.add guardId newList

            go rest (guardId, newSchedule)

        | [] -> acc

    let (_, result) = go (events |> List.ofArray) (0, zero)
    result

let expand (from, to') = 
    let to' = if to' = 0 then 60 else to'
    [| from .. to'-1 |]

let answer1 events = 
    
    let schedule = buildSchedule events
    
    let composed = schedule |> Map.map (fun k v -> v |> List.sumBy (fun x -> x.Duration))
    
    let maxSleepGuardId = 
        composed 
        |> Map.toSeq
        |> Seq.maxBy (fun (k,v) -> v)
        |> fst

    let mostMinuteAsleep = 
        schedule 
        |> Map.find maxSleepGuardId
        |> Array.ofList
        |> Array.collect (fun x -> x.Range |> expand)
        |> Array.groupBy id
        |> Array.maxBy (fun (k,v) -> v.Length)
        |> fst

    (maxSleepGuardId * mostMinuteAsleep)

let answer2 events = 
    
    let schedule = buildSchedule events
    
    let mostMinuteAsleep = 
        schedule 
        |> Map.map (fun k v -> 
            let allMinutes = 
                v
                |> Array.ofList
                |> Array.collect (fun period -> period.Range |> expand)

            allMinutes
            |> Array.groupBy id
            |> Array.maxBy (fun (minute, occurances) -> occurances.Length)
            |> fun (minute, occurances) -> minute, occurances.Length
        )
        |> Map.toArray
        |> Array.maxBy (fun (id,(minute, occurances)) -> occurances)
        |> fun (id,(minute, occurances)) -> id * minute

    mostMinuteAsleep

answer1 testInput
answer1 input

answer2 testInput
answer2 input

