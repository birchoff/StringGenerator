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

// . ^
module RegularExpressionParser
    open FParsec
    open CharParsers

    type RegEx = 
        | RChar of char
        | RContains of RegEx list
        | RStartsWith of RegEx list
        | REndsWith of RegEx list
        | RExact of RegEx list
        | RNone

    let hat = '^'
    let dollar = '$'

    let nonAlphaNumeric = anyOf "~`!@%&;:'<,>/" |>> RChar

    let alphaNumeric = asciiLetter |>> RChar

    let numeric = digit |>> RChar

    let character = choice [alphaNumeric; numeric; nonAlphaNumeric]

    let elementaryRegularExpression = many character

    let regularExpression = 
        choice 
            [
                lookAhead (skipChar hat >>. elementaryRegularExpression .>> skipChar dollar) |>> RExact;
                lookAhead (elementaryRegularExpression .>> skipChar dollar) |>> REndsWith;
                skipChar hat >>. elementaryRegularExpression .>> notFollowedBy (skipChar dollar) |>> RStartsWith;
                elementaryRegularExpression |>> RContains;
                //elementaryRegularExpression .>> skipDollar;
                //elementaryRegularExpression |> between skipHat skipDollar
            ]

    let parse regExp = 
        match run regularExpression regExp with
        | Success(result, _, _) -> result
        | Failure(_,_,_) -> RNone