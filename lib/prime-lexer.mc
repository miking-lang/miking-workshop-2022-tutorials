include "parser/lexer.mc"

lang PrimeTokenParser = TokenParser
  syn Token =
  | PrimeTok {info : Info}
  syn TokenRepr =
  | PrimeRepr ()

  sem parseToken pos =
  | "'" ++ str ->
    let pos2 = advanceCol pos 1 in
    let info = makeInfo pos pos2 in
    {token = PrimeTok {info = info}, lit = "'", info = info, stream = {pos = pos2, str = str}}

  sem tokKindEq tokRepr =
  | PrimeTok _ -> match tokRepr with PrimeRepr _ then true else false

  sem tokInfo =
  | PrimeTok x -> x.info

  sem tokReprToStr =
  | PrimeRepr _ -> "'"

  sem tokToStr =
  | PrimeTok _ -> "'"

  sem tokToRepr =
  | PrimeTok _ -> PrimeRepr ()
end
