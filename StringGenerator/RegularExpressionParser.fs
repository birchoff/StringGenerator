//<RE>	            ::=	 <union> | <simple-RE>
//<union>	        ::=	<RE> "|" <simple-RE>
//<simple-RE>	    ::=	 <concatenation> | <basic-RE>
//<concatenation>	::=	<simple-RE> <basic-RE>
//<basic-RE>	    ::=	 <star> | <plus> | <elementary-RE>
//<star>	        ::=	<elementary-RE> "*"
//<plus>	        ::=	<elementary-RE> "+"
//<elementary-RE>	::=	 <group> | <any> | <char> | <set>
//<group>	        ::=	"(" <RE> ")"
//<any>	            ::=	"."
//<char>	        ::=	any non metacharacter | "\" metacharacter
//<set>	            ::=	 <positive-set> | <negative-set>
//<positive-set>	::=	"[" <set-items> "]"
//<negative-set>	::=	"[^" <set-items> "]"
//<set-items>	    ::=	<set-item> | <set-item> <set-items>
//<set-items>	    ::=	<range> | <char>
//<range>	        ::=	<char> "-" <char>

module RegularExpressionParser
    open FParsec
    open CharParsers

    ///<summary>
    ///Traces the execution of a parser.
    ///</summary>
    let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
        fun stream ->
            printfn "%A: Entering %s" stream.Position label
            let reply = p stream
            printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
            reply

    type RegEx = 
        | RAny
        | RChar of char
        | RStar of RegEx
        | RPlus of RegEx
        | RGroup of RegEx
        | RStartsWith of RegEx
        | REndsWith of RegEx
        | RExact of RegEx
        | RUnion of RegEx * RegEx
        | RConcat of RegEx * RegEx
        | RNone

    let hat = '^'
    let dollar = '$'

    let regularExpression, regularExpressionRef = createParserForwardedToRef()
    let simpleRegEx, simpleRegExRef = createParserForwardedToRef()

    let nonAlphaNumeric = anyOf "~`!@%&;:'<,>/" |>> RChar

    let alphaNumeric = asciiLetter |>> RChar

    let numeric = digit |>> RChar

    let character = choice [alphaNumeric; numeric; nonAlphaNumeric;]

    let any = skipChar '.' >>% RAny

    let group = attempt (regularExpression |> between (skipChar '(') (skipChar ')')) |>> RGroup

    let elementaryRegEx = choice[group; any; character]

    let star = attempt (elementaryRegEx .>> skipChar '*') |>> RStar

    let plus = attempt (elementaryRegEx .>> skipChar '+') |>> RPlus

    let basicRegEx = choice [star; plus; elementaryRegEx]

    let concatenation = attempt (tuple2 basicRegEx simpleRegEx) |>> RConcat

    do simpleRegExRef := choice [concatenation; basicRegEx]

    let union = attempt (simpleRegEx .>>. (skipChar '|' >>. regularExpression)) |>> RUnion

    let startsWith = attempt (skipChar '^' >>.  choice [union;simpleRegEx]) |>> RStartsWith

    let endsWith = attempt (choice [union;simpleRegEx] .>> skipChar '$') |>> REndsWith

    let exact = attempt (skipChar '^' >>. choice [union;simpleRegEx] .>> skipChar '$') |>> RExact

    do regularExpressionRef := choice [exact; startsWith; endsWith; union; simpleRegEx]

    let parse regExp = 
        match run regularExpression regExp with
        | Success(result, _, _) -> result
        | Failure(_,_,_) -> RNone