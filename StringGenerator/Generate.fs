module Generate
    open RegularExpressionParser
    open System.Text

    type StringGenerator = unit -> string option

    let asciiCharacters = seq{yield 9}|> Seq.append (seq{32 .. 126}) |> Seq.map (fun ch -> char ch) |> Seq.toList

    let permutationWithRepitionMap size numberOfElements =
        seq{
            let currentElementIds = ref (Array.create size 1)
            yield !currentElementIds
            while !currentElementIds |> Array.exists (fun elmntId -> not (elmntId = numberOfElements)) do
                let incrementedElementIds = !currentElementIds |> Array.mapi (fun i elmntId -> if i = 0 then elmntId + 1 else elmntId)
                incrementedElementIds
                |> Array.iteri 
                    (fun i elmntId -> 
                        if not (i = size - 1) && elmntId > numberOfElements then 
                            Array.set incrementedElementIds i 1
                            Array.get incrementedElementIds (i+1) |> (+) 1 |> Array.set incrementedElementIds (i+1)
                    )
                currentElementIds := incrementedElementIds      
                yield incrementedElementIds
        }

    

    let generateStrings length =
        let appendCharToStringBuilder (ch:char) (sb:StringBuilder) = sb.Append(ch)
        permutationWithRepitionMap length asciiCharacters.Length
        |> Seq.map 
            (fun permMap -> 
                permMap 
                |> Array.fold  (fun sb charPos -> 
                    sb |> appendCharToStringBuilder (charPos - 1 |> List.nth asciiCharacters)) (new StringBuilder(permMap.Length)
                )
                |> fun sb -> sb.ToString()
            )
        |> Seq.toArray

    let randomStringGenerator (): StringGenerator =
        let generatorState = ref (0,0)
        let currentStrings: string array option ref = ref None
        let nextString = function 
            | (0,_) -> ("", (1,0))
            | (size, currentStringIndex) as state -> 
                match !currentStrings with
                | None -> 
                    let currentStrings' = generateStrings size
                    currentStrings := currentStrings' |> Some
                    (currentStringIndex |> Array.get currentStrings', state)
                | Some(currentStrings') when currentStringIndex = (currentStrings' |> Array.length) - 1 ->
                    currentStrings := None
                    (currentStringIndex |> Array.get currentStrings', (size+1, 0))
                | Some(currentStrings) -> 
                    (currentStringIndex |> Array.get currentStrings, (size, currentStringIndex + 1))
        fun () ->
            let str, newState = !generatorState |> nextString
            generatorState := newState
            str |> Some
            

    let rec regexToStringGenerator = function
        | RAny as rany -> anyStringGenerator rany
        | RChar(_) as rchar -> charStringGenerator rchar
        | RStar(_) as rstar -> starStringGenerator rstar
        | RPlus(_) as rplus -> plusStringGenerator rplus
        | RGroup(_) as rgroup -> groupStringGenerator rgroup
    and anyStringGenerator rany : StringGenerator = 
        let currentStrings : string array option ref = ref None
        let nextString = function 
            | currentStringIndex as state -> 
                match !currentStrings with
                | None -> 
                    let currentStrings' = generateStrings 1
                    currentStrings := currentStrings' |> Some
                    Some(currentStringIndex |> Array.get currentStrings', state)
                | Some(currentStrings') when currentStringIndex = (currentStrings' |> Array.length) ->
                    None
                | Some(currentStrings) -> 
                    Some(currentStringIndex |> Array.get currentStrings, state + 1)
        let generatorState = ref 0
        match rany with
        | RAny ->
            fun () ->
                match !generatorState |> nextString with
                | Some(str, newState) ->
                    generatorState := newState
                    str |> Some
                | _ -> None
        | _ -> fun () -> None
    and charStringGenerator rchar : StringGenerator = 
        match rchar with
        | RChar(ch) -> fun () -> ch.ToString() |> Some
        | _ -> fun () -> None
    and starStringGenerator rstar : StringGenerator =
        match rstar with
        | RStar(regex) ->
            let stringGenerator = regex |> regexToStringGenerator
            let generatorState = ref 0
            fun () ->
                let sb = new StringBuilder()
                let rec generate times (accum:StringBuilder) =
                    if times > 0 then accum.Append(stringGenerator()) |> generate (times - 1)
                    else accum
                sb |> generate !generatorState |> ignore
                incr generatorState
                sb.ToString() |> Some
        | _ -> fun () -> None
    and plusStringGenerator rplus : StringGenerator =
        match rplus with
        | RPlus(regex) ->
            let stringGenerator = regex |> regexToStringGenerator
            let generatorState = ref 1
            fun () ->
                let sb = new StringBuilder()
                let rec generate times (accum:StringBuilder) =
                    if times > 0 then accum.Append(stringGenerator()) |> generate (times - 1)
                    else accum
                sb |> generate !generatorState |> ignore
                incr generatorState
                sb.ToString() |> Some
        | _ -> fun () -> None
    and groupStringGenerator rgroup : StringGenerator =
        match rgroup with
        | RGroup(regex) -> regex |> regexToStringGenerator
        | _ -> fun() -> None
    and startsWithStringGenerator rstartsWith : StringGenerator =
        match rstartsWith with
        | RStartsWith(regex) ->
            let prefixStringGenerator = regexToStringGenerator regex
            let postfixStringGenerator =  randomStringGenerator() |> ref
            let currentPostfixStrings : string array option ref  = ref None
            let currentPrefixString : string option ref = ref None
            fun () -> None
        | _ -> fun () -> None

    let matchingStringsFor regExp length =
        let parseResult = parse regExp
        let generator = regexToStringGenerator parseResult
        generator
        |> Seq.unfold (
            fun generator ->
                let str = generator()
                match str with
                | None -> None
                | Some(str) -> Some(str, generator)
        )
        |> Seq.takeWhile (fun str -> str |> String.length <= length)