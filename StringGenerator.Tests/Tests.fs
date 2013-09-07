module Tests

    open NUnit.Framework
    open FsUnit
    open Generate
    open System.Text.RegularExpressions

    let invalidSingleCharacterRegExp =
        [|"$";"^";"*";"(";")";"-";"_";"=";"+";"{";"}";"[";"]";@"\";"|";"?"|]
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
    [<TestCaseSource("validSingleCharacterRegExp")>]
    let ``Given a single character regular expression. When generating a matching string whose length is 1 character. Then their should only be one matching string`` (regExp: string) =
        let result = Generate.matchingStringsFor regExp 1
        Seq.length result |> should equal 1

    [<Test>]
    [<TestCaseSource("validSingleCharacterRegExp")>]
    let ``Given a single character regular expression. When generating a matching string whose length is 1 character. Then the matching string should be the same as the regExp`` (regExp: string) =
        let result = Generate.matchingStringsFor regExp 1
        Seq.exactlyOne result |> should equal regExp

    [<Test>]
    [<TestCaseSource("invalidSingleCharacterRegExp")>]
    let ``Given a single invalid character regular expression. When generating a matching string whose length is 1 character. Then there should be no matching string`` (regExp: string) =
        let result = Generate.matchingStringsFor regExp 1
        Seq.length result |> should equal 0

    [<Test>]
    [<TestCaseSource("validSingleCharacterRegExp")>]
    let ``Given a single character regular expression. When generating a matching string whose length is 3 characters. Then there should be more than one matching string`` (regExp: string) =
        let results = Generate.matchingStringsFor regExp 3
        Seq.length results |> should greaterThan 0

    [<Test>]
    [<TestCaseSource("validSingleCharacterRegExp")>]
    let ``Given a single character regular expression. When generating a matching string whose length is 3 characters. Then all matching strings should match regular expression`` (regExp: string) =
        let regExpObj = new Regex(regExp)
        let results = Generate.matchingStringsFor regExp 3 |> Seq.toList
        let numberOfMatches = results |> List.map (fun result -> regExpObj.IsMatch(result)) |> List.length
        List.length results |> should equal numberOfMatches

    [<Test>]
    let ``Given the regular expression abc123. When generating a matching string whose length is less than 6 characters. Then there should be no matching strings`` 
        ([<Values(-1,0,1,2,3,4,5)>]stringSize) =
        let results = Generate.matchingStringsFor "abc123" stringSize
        Seq.length results |> should equal 0

    [<Test>]
    let ``Given the regular expression abc123. When generating a matching string whose length is exactly 6 characters. Then there should be 1 matching string`` () =
        let results = Generate.matchingStringsFor "abc123" 6
        Seq.length results |> should equal 1

    [<Test>]
    let ``Given the regular expression abc123. When generating a matching string whose length is exactly 6 characters. Then the matching string should be abc123`` () =
        let results = Generate.matchingStringsFor "abc123" 6
        Seq.exactlyOne results |> should equal "abc123"

    [<Test>]
    let ``Given the regular expression abc123. When generating a matching string whose length is greater than 6 characters. Then all matching strings should match regular expression``() =
        let regExp = "abc123"
        let regExpObj = new Regex(regExp)
        let results = Generate.matchingStringsFor regExp 8 |> Seq.toList
        let numberOfMatches = results |> List.map (fun result -> regExpObj.IsMatch(result)) |> List.length
        List.length results |> should equal numberOfMatches

    [<Test>]
    let ``Given the regular expression ^abc. When generating a matching string whose length is more than 3 characters. Then the number of matching strings should be greater than zero`` () =
        let results = Generate.matchingStringsFor "^abc" 5
        results |> Seq.length |> should greaterThan 0

    [<Test>]
    let ``Given the regular expression ^abc. When generating a matching string whose length is more than 3 characters. Then all matching strings should start with abc`` () =
        let results = Generate.matchingStringsFor "^abc" 5
        results |> Seq.iter (fun result -> result |> should startWith "abc")

    [<Test>]
    let ``Given the regular expression abc$. When generating a matching string whose length is more than 3 characters. Then the number of matching strings should be greater than zero`` () =
        let results = Generate.matchingStringsFor "abc$" 5
        results |> Seq.length |> should greaterThan 0

    [<Test>]
    let ``Given the regular expression abc$. When generating a matching string whose length is more than 3 characters. Then all matching strings should end with abc`` () =
        let results = Generate.matchingStringsFor "abc$" 5
        results |> Seq.iter (fun result -> result |> should endWith "abc")

    [<Test>]
    let ``Given the regular expression ^abc$. When generating a matching string whose length is more than 3 characters. Then there should be no matching strings`` () =
        let results = Generate.matchingStringsFor "^abc$" 5
        results |> Seq.length |> should equal 0

    [<Test>]
    let ``Given the regular expression ^abc$. When generating a matching string whose length is exactly 3 characters. Then there should only be a single matching string`` () =
        let results = Generate.matchingStringsFor "^abc$" 3
        results |> Seq.length |> should equal 1

    [<Test>]
    let ``Given the regular expression ^abc$. When generating a matching string whose length is exactly 3 characters. Then the matching string should be abc`` () =
        let results = Generate.matchingStringsFor "^abc$" 3
        results |> Seq.exactlyOne |> should equal "abc"

    [<Test>]
    let ``Given the regular expression a|b. When generate a matching string whose length is exactly 1 character. Then there should be 2 matching characters``() = 
        let results = Generate.matchingStringsFor "a|b" 1
        results |> Seq.length |> should equal 2

    [<Test>]
    let ``Given the regular expression a|b. When generate a matching string whose length is exactly 1 character. Then the matching strings should contain`` 
        ([<Values("a", "b")>]matchingString) = 
        let results = Generate.matchingStringsFor "a|b" 1
        results |> should contain matchingString

    [<Test>]
    let ``Given the regular expression a*. when generating a matching string whose length is 0. Then there should be no matching strings`` () =
        let results = Generate.matchingStringsFor "a*" 0
        results |> Seq.length |> should equal 0

    [<Test>]
    let ``Given the regular expression a*. when generating a matching string whose length is 1. Then there should be two matching strings`` () =
        let results = Generate.matchingStringsFor "a*" 1
        results |> Seq.length |> should equal 2

    [<Test>]
    let ``Given the regular expression ba*. when generating a matching string whose length is 1. Then there should be one matching string`` () =
        let results = Generate.matchingStringsFor "ba*" 1
        results |> Seq.length |> should equal 0

    [<Test>]
    let ``Given the regular expression ba*. when generating a matching string whose length is 1. Then the matching strings should be b`` () =
        let results = Generate.matchingStringsFor "ba*" 1
        results|> Seq.exactlyOne |> should equal "b"

    [<Test>]
    let ``Given the regular expression ba*. when generating a matching string whose length is 2. Then there should be two matching strings`` () =
        let results = Generate.matchingStringsFor "ba*" 2
        results |> Seq.length |> should equal 2