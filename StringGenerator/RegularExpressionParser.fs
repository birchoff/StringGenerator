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

    let nonAlphaNumeric = anyOf "~`!@%&;:'<,>/" |>> string

    let alphaNumeric = asciiLetter |>> string

    let numeric = digit |>> string

    let character = choice [alphaNumeric; numeric; nonAlphaNumeric]

    let regularExpresion = character

    let parse regExp = 
        
        match run regularExpresion regExp with
        | Success(result, _, _) -> result
        | Failure(_,_,_) -> ""