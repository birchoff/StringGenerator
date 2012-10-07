module Tests

open MbUnit.Framework
open FsUnit.MbUnit
open Generate

let invalidSingleCharacterRegExp =
    [|"$";"^";"*";"(";")";"-";"_";"=";"+";"{";"}";"[";"]";@"\";"|"|]
    |> Seq.map (fun spChr -> [|spChr|])

let validSingleCharacterRegExp =
    let lowerCaseAlphaNumeric = seq{for ch in 97 .. 122 -> [| ch |> char |> string|]}
    let upperCaseAlphaNumeric = seq{for ch in 65 .. 90 -> [| ch |> char |> string|]}
    let numeric = seq{for ch in 48 .. 57 -> [| ch |> char |> string|]}
    let nonAlphaNumeric = 
        [|"~"; "`"; "!"; "@"; "%"; "&"; ";"; ":"; "'"; "<"; ","; ">"; "/"|] 
        |> Seq.map (fun spChr -> [|spChr|])
    
    lowerCaseAlphaNumeric
    |> Seq.append upperCaseAlphaNumeric
    |> Seq.append numeric
    |> Seq.append nonAlphaNumeric

[<Test>]
[<Factory("validSingleCharacterRegExp")>]
let ``Given a single character regular expression. When generating a matching string whose length is 1 character. Then their should only be one matching string`` (regExp: string) =
    let result = Generate.stringsFor 1 regExp
    Seq.length result |> should equal 1

[<Test>]
[<Factory("validSingleCharacterRegExp")>]
let ``Given a single character regular expression. When generating a matching string whose length is 1 character. Then the matching string should be the same as the regExp`` (regExp: string) =
    let result = Generate.stringsFor 1 regExp
    Seq.exactlyOne result |> should equal regExp

[<Test>]
[<Factory("invalidSingleCharacterRegExp")>]
let ``Given a single invalid character regular expression. When generating a matching string whose length is 1 character. Then there should be no matching string`` (regExp: string) =
    let result = Generate.stringsFor 1 regExp
    Seq.length result |> should equal 0