#r @"C:\Users\birchoff\Documents\GitHub\StringGenerator\StringGenerator\packages\FParsec.0.9.2.0\lib\net40\FParsecCS.dll"
#r @"C:\Users\birchoff\Documents\GitHub\StringGenerator\StringGenerator\packages\FParsec.0.9.2.0\lib\net40\FParsec.dll"
open FParsec
open CharParsers
let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg