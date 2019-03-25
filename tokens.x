{
    module Tokens where
}

%wrapper "basic"
$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    $white+     ;
    "stdout"    {\s -> TokStdout}
    "->"        {\s -> TokArrow}
    "="         {\s -> TokEquals}
    "("         {\s -> TokLeftPar}
    ";"         {\s -> TokSemiCol}
    ")"         {\s -> TokRightPar}
    "+"         {\s -> TokPlus}
    "-"         {\s -> TokMinus}   
    "*"         {\s -> TokProd}
    "/"         {\s -> TokDiv}
    "%"         {\s -> TokMod}
    "^"         {\s -> TokPow}
    "let"       {\s -> TokLet}
    "{"         {\s -> TokLeftBrace}
    "}"         {\s -> TokRightBrace}
    ","         {\s -> TokComma}
    "print"     {\s -> TokPrint}
    $digit+     {\s -> TokDigit (read s)}
    $alpha[$alpha $digit \_ \']*  {\s -> TokAlphaNum s}

{
data Token = 
    TokEquals |
    TokLeftBrace |
    TokRightBrace |
    TokLeftPar |
    TokRightPar |
    TokSemiCol |
    TokComma |
    TokArrow |
    TokStdout |
    TokStdin |
    TokLet |
    TokDigit Int |
    TokAlphaNum String |
    TokPlus |
    TokMinus |
    TokProd |
    TokDiv |
    TokMod |
    TokPow |
    TokPrint
    deriving (Eq, Show)

}