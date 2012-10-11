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

    let regExToString regEx =
        let rec regExToString (sb:StringBuilder) =
            function
            | RChar(ch) -> sb.Append(ch)
            | RStartsWith(regexs) | RContains(regexs) | REndsWith(regexs) | RExact(regexs) -> 
                regexs |> List.fold (fun sb' regex -> regExToString sb' regex) sb
            | RNone -> sb
            
        (regExToString (new StringBuilder()) regEx).ToString()

    let matchingStringsFor regExp length =
        let parseResult = parse regExp
        let regExString = regExToString parseResult
        match length, parseResult, length - regExString.Length with
        | _ when regExString.Length = 0 -> Seq.empty
        
        | _, _, difference when difference = 0 -> seq{yield regExString}

        | _, RContains(_), difference when difference >= 1 -> 
            generateString difference
            |> Seq.map (fun sb -> sb.ToString())
            |> Seq.collect (fun str -> seq{for i in 0 .. str.Length-1 do yield str.Insert(i, regExString)})

        | _, RStartsWith(_), difference when difference >= 1 -> 
            generateString difference |> Seq.map (fun sb -> sb.Insert(0, regExString).ToString())

        | _, REndsWith(_), difference when difference >= 1 -> 
            generateString difference |> Seq.map (fun sb -> sb.Append(regExString).ToString())

        | _, RExact(_), difference when difference = 0 -> seq{yield regExString}

        | _ -> Seq.empty