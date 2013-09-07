module Generate
    open RegularExpressionParser
    open System.Text

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

    

    let generateString length =
        let appendCharToStringBuilder (ch:char) (sb:StringBuilder) = sb.Append(ch)
        permutationWithRepitionMap length asciiCharacters.Length
        |> Seq.map 
            (fun permMap -> 
                permMap 
                |> Array.fold 
                    (fun sb charPos -> sb |> appendCharToStringBuilder (charPos - 1 |> List.nth asciiCharacters)) (new StringBuilder(permMap.Length))
            )

    let regExToString regex =
        let cloneStringBuilders sbs = sbs |> Seq.map (fun sb -> new StringBuilder(sb.ToString()))
        let rec regExToString (sbs:StringBuilder seq) =
            function
            | RChar(ch) -> seq{for sb in sbs do yield sb.Append(ch)}

            |RConcat(regex1, regex2) -> regex2 |> regExToString (regex1 |> regExToString sbs)

            | RStartsWith(regex') 
            | REndsWith(regex') 
            | RExact(regex') -> regExToString sbs regex'

            | RUnion(regexL, regexR) -> 
                let leftResult = regExToString sbs regexL
                let rightResult = regExToString (sbs |> cloneStringBuilders) regexR
                Seq.append leftResult rightResult

            | RStar(regex') ->
                (cloneStringBuilders sbs, RNone)
                |> Seq.unfold 
                        (
                            fun state -> 
                                let result =
                                    match state with
                                    | clonedSbs,RNone -> clonedSbs
                                    | clonedSbs, regex'' -> regExToString clonedSbs regex''
                                Some(result, (cloneStringBuilders result, regex'))
                        )
                |> Seq.collect (fun sbs -> sbs)
                    

            | RNone -> sbs
        regExToString (seq{yield new StringBuilder()}) regex |> Seq.map (fun sb -> sb.ToString())

    let matchingStringsFor regExp length =
        let parseResult = parse regExp
        let regExStrings = regExToString parseResult
        let helper (regExString:string) =
            match length, parseResult, length - regExString.Length with

            | _ when regExString.Length = 0 -> Seq.empty
        
            | _, _, difference when difference = 0 -> seq{yield regExString}

            | _, RStartsWith(_), difference when difference >= 1 -> 
                generateString difference |> Seq.map (fun sb -> sb.Insert(0, regExString).ToString())

            | _, REndsWith(_), difference when difference >= 1 -> 
                generateString difference |> Seq.map (fun sb -> sb.Append(regExString).ToString())

            | _, RExact(_), difference when difference > 0 -> Seq.empty

            | _, _, difference when difference >= 1 -> 
                generateString difference
                |> Seq.map (fun sb -> sb.ToString())
                |> Seq.collect (fun str -> seq{for i in 0 .. str.Length-1 do yield str.Insert(i, regExString)})

            | _ -> Seq.empty
        regExStrings |> Seq.takeWhile (fun regExString -> regExString.Length <= length ) |> Seq.collect helper