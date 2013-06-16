namespace DerivativeParser

module Grammar =

  type 'a Rule = 
  | Epsilon 
  | Empty 
  | Token of 'a 
  | NT of string
  | Conj of 'a Rule * 'a Rule 
  | Disj of 'a Rule * 'a Rule

  type 'a Grm = string * Map<string, 'a Rule> 

  let get name grm = Map.find name (snd grm)
    
  let rec nullable_rule rule grm =
    match rule with
    | Epsilon -> true
    | NT other -> nullable_rule (get other grm) grm
    | Conj (l, r) -> nullable_rule l grm && nullable_rule r grm
    | Disj (l, r) -> nullable_rule l grm || nullable_rule r grm
    | _ -> false in

  let derive token grm =
    let start_name = fst grm in
    let rule = get start_name grm in
    let rec derive_rule rule = 
      match rule with
      | Epsilon -> Empty
      | Empty -> Empty
      | Token tok -> 
          if tok = token
          then Epsilon
          else Empty
      | NT other -> derive_rule (get other grm) 
      | Conj (l, r) -> 
          if nullable_rule l grm
          then Disj(Conj(derive_rule l, r),derive_rule r)
          else Conj(derive_rule l, r)
      | Disj (l, r) -> Disj(derive_rule l, derive_rule r)
    in
    derive_rule rule
   

