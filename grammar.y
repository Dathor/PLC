{
module Grammar where
import Tokens
}

%name lang
%tokentype { Token }
%error { parseError }
%token
    "="         {TokEquals}
    "("         {TokLeftPar}
    ";"         {TokSemiCol}
    ")"         {TokRightPar}
    "+"         {TokPlus}
    "-"         {TokMinus}
    "*"         {TokProd}
    "/"         {TokDiv}
    "%"         {TokMod}
    "^"         {TokPow}
    "stdout"    {TokStdout}
    "->"        {TokArrow}
    "let"       {TokLet}
    "{"         {TokLeftBrace}
    "}"         {TokRightBrace}
    ","         {TokComma}
    "print"     {TokPrint}
    name        {TokAlphaNum $$}
    num         {TokDigit $$}

%left "->"
%left "+" "-"
%left "*" "/" "%"
%left "^"
%left NEG
%left DECL
%%


FuncList : FuncDecl {FuncList $1 FEmpty}
           | FuncDecl "->" FuncList {FuncList $1 $3}
           | "stdout" {FEmpty}

FuncDecl : "{" VarList "->" Exp "}" %prec DECL {FuncDecl $2 $4}

VarList : name {VarList $1 VEmpty}
        | name "," VarList {VarList $1 $3}
        | {VEmpty}


Exp : "let" name "=" Exp {ELet $2 $4}
      | Exp "*" Exp {EProd $1 $3}
      | Exp "/" Exp {EDiv $1 $3}
      | Exp "+" Exp {EPlus $1 $3}
      | Exp "-" Exp {EMinus $1 $3}
      | Exp "%" Exp {EMod $1 $3}
      | Exp "^" Exp {EPow $1 $3}
      | "(" Exp ")" {$2}
      | "-" Exp  %prec NEG {ENeg $2}
      | "print" Exp {EPrint $2}
      | name {Var $1}
      | num {Val $1}
      | Exp ";" Exp {ENext $1 $3}


{
parseError :: [Token] -> a
parseError _ = error "Parse error"
    
data FuncList = FuncList FuncDecl FuncList | FEmpty
                deriving (Show, Eq)

data FuncDecl = FuncDecl VarList Exp
                deriving (Show, Eq)

data VarList = VarList String VarList | VEmpty
               deriving (Show, Eq)

            
data Exp = ELet String Exp
           | EProd Exp Exp
           | EDiv Exp Exp
           | EPlus Exp Exp
           | EMinus Exp Exp
           | EMod Exp Exp
           | EPow Exp Exp
           | ENeg Exp
           | EPrint Exp
           | Var String
           | Val Int
           | ENext Exp Exp
           deriving (Show, Eq)

}

