namespace DerivativeParser


module Script =
  open DerivativeParser
  open Grammar


  (* simple grammar:
   * B = '1' | '0'
   * S = Epsilon | B S
   *
   * In simple-form:
   * B = Disj(Token 1, Token 0)
   * S = Disj(Epsilon, Conj(NT "B", NT "S"))
   *)

  let simpleMap : Map<string, int Rule> = Map.add "B" (Disj(Token 0, Token 1)) (Map.add "S" (Disj(Epsilon, Conj(NT "B", NT "S"))) Map.empty )
  let simpleGrm : int Grm = "S", simpleMap

  let d1 = derive 1 simpleGrm

