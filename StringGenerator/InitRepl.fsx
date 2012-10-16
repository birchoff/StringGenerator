#r @"C:\Users\birchoff\Documents\GitHub\StringGenerator\StringGenerator\packages\FParsec.0.9.2.0\lib\net40\FParsecCS.dll"
#r @"C:\Users\birchoff\Documents\GitHub\StringGenerator\StringGenerator\packages\FParsec.0.9.2.0\lib\net40\FParsec.dll"
#r @"C:\Users\birchoff\Documents\GitHub\StringGenerator\StringGenerator\bin\Debug\StringGenerator.dll"
open FParsec
open CharParsers
open RegularExpressionParser
///<summary>
///Executes the supplied parser and prints out the result.
///<summary/>
let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

