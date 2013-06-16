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
   
  let set_fix f cur =
    let rec loop cur =
      let next = f cur in
      if Set.isSubset next cur
      then cur
      else loop (Set.union next cur) in
    loop cur

  let compute_nullable grm =
    let rec loop_one nt_set rule =
      match rule with
      | Epsilon -> true
      | NT other -> Set.contains other nt_set  (* nullable_rule (get other grm) grm *)
      | Conj (l, r) -> loop_one nt_set l && loop_one nt_set r 
      | Disj (l, r) -> loop_one nt_set l || loop_one nt_set r 
      | _ -> false in
    let f nts = Map.fold (fun acc_nts nt rule -> 
                                 if loop_one acc_nts rule 
                                 then Set.add nt acc_nts 
                                 else acc_nts) nts (snd grm) in
    set_fix f Set.empty

  let nullable_rule rule grm =
    let nullables = compute_nullable grm in
    let rec loop rule = 
      match rule with 
      | Epsilon -> true
      | Empty -> false
      | NT other -> Set.contains other nullables
      | Conj (l, r) -> loop l && loop r
      | Disj (l, r) -> loop l || loop r
      | _ -> false in 
    loop rule

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
      | Disj (l, r) -> Disj(derive_rule l, derive_rule r) in
    derive_rule rule

