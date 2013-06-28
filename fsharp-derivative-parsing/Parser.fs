namespace DerivativeParser

module Grammar =
//  type IToken = interface 
//    abstract Str : unit -> string
//  end


  (* compute fixed-point of function 'f' on set until no new
   * elements are found by 'f' relative to the already computed
   * set 'cur'
   *)
  let set_fix f cur =
    let rec loop cur =
      let next = f cur in
      if Set.isSubset next cur
      then cur
      else loop (Set.union next cur) in
    loop cur

  let fix f cur =
    let rec loop cur =
      let next = f cur in
      if next = cur
      then next
      else loop next in
    loop cur

  type 'a Rule (* when 'a :> IToken *) = 
  | Epsilon 
  | Empty 
  | Token of 'a 
  | NT of string
  | Conj of 'a Rule * 'a Rule 
  | Disj of 'a Rule * 'a Rule
  
  type 'a LazyRule (* when 'a :> IToken *) =
    | Derive of Lazy<'a Rule>
    | Rule of 'a Rule

  let (!!) r =
    match r with
    | Rule x -> x
    | Derive thunk -> thunk.Force()

  let make_derive fx = Derive(Lazy.Create fx)

  type 'a Grm (* when 'a :> IToken *) = string * Map<string, 'a LazyRule> 
      

  let get name grm = Map.find name (snd grm)

  let try_get name grm = 
    if Map.containsKey name (snd grm)
    then Some (get name grm)
    else None

  (* get a set of all NTs reachable from the given initial_nt *)
  let get_reachable_nts initial_nt grm =
    let rule_map = snd grm in
    let rec get_referenced_nts acc rule =
      match rule with
      | Epsilon | Empty | Token _ -> Set.empty
      | NT other -> Set.add other acc
      | Conj (l, r) | Disj (l, r) -> get_referenced_nts (get_referenced_nts acc l) r in
    let f reached_nts = 
      Set.fold 
        (fun new_nts nt_name ->
          match try_get nt_name grm with
          | None -> new_nts
          | Some rule -> 
              get_referenced_nts Set.empty !!rule 
              |> Set.union new_nts)
        Set.empty reached_nts in
    set_fix f (Set.ofList [initial_nt])

  let string_of_rule rule = 
    let par x = "(" + x + ")" in
    let rec loop rule = 
      match rule with
      | Epsilon -> "e"
      | Empty -> "E"
      | Token tok -> tok.ToString()
      | NT other -> other
      | Conj (l, r) -> loop l + " " + loop r
      | Disj (l, r) -> par(loop l) + "|" + par(loop r) in
    match rule with
    | Rule x -> loop x
    | Derive thunk -> thunk.Force() |> loop

  let string_of_grm grm = 
    let (start_name, rules) = grm in
    Map.fold 
      (fun str rule_name rule -> rule_name + " -> " + (string_of_rule rule) + "\n" + str)
      ("start: " + start_name) rules
  
  
  let get_undefined_nts rule grm =
    let rec loop acc rule =
      match rule with
      | Epsilon | Empty | Token _ -> acc
      | Conj (l, r) | Disj (l, r) -> 
          loop (loop acc l) r
      | NT other -> match try_get other grm with
                    | None -> Set.add other acc
                    | Some _ -> acc in
    loop Set.empty rule


  let compute_nullable grm =
    let rec loop_one nt_set rule =
      match rule with
      | Epsilon -> true
      | NT other -> Set.contains other nt_set  (* nullable_rule (get other grm) grm *)
      | Conj (l, r) -> loop_one nt_set l && loop_one nt_set r 
      | Disj (l, r) -> loop_one nt_set l || loop_one nt_set r 
      | _ -> false in
    let f nts = Map.fold (fun acc_nts nt rule -> 
                                 if loop_one acc_nts !!rule 
                                 then Set.add nt acc_nts 
                                 else acc_nts) nts (snd grm) in
    set_fix f Set.empty

  let is_nullable name nullables = Set.contains name nullables

  let is_nullable_rule rule nullables =
    let rec loop rule = 
      match rule with 
      | Epsilon -> true
      | Empty -> false
      | NT other -> is_nullable other nullables
      | Conj (l, r) -> loop l && loop r
      | Disj (l, r) -> loop l || loop r
      | _ -> false in 
    loop rule
  
//  let get_next_name (token : IToken) name  =
  let get_next_name token name  =
    "d" + token.ToString() (* Str() *) + name

//  let get_prev_name (token : IToken) (next_name : string) = 
  let get_prev_name token (next_name : string) = 
    next_name.Substring(token.ToString().Length + 1 (*Str()*))

  let derive_with token grm nullables : 'a Grm (* when 'a :> IToken *) = 
    let start_name = fst grm in
    let rule = get start_name grm in
    let new_name = get_next_name token start_name in
    let rec derive_rule rule = 
      match rule with
      | Epsilon -> Empty
      | Empty -> Empty
      | Token tok -> 
          if tok = token
          then Epsilon
          else Empty
      | Conj (l, r) -> 
          if is_nullable_rule l nullables
          then Disj(Conj(derive_rule l, r),derive_rule r)
          else Conj(derive_rule l, r)
      | Disj (l, r) -> Disj(derive_rule l, derive_rule r) 
      | NT other -> NT (get_next_name token other) in
    let rule' = derive_rule !!rule in
    let undefined = get_undefined_nts rule' grm in
    let rules' = if not(Set.contains new_name undefined)
                 then Map.add new_name (make_derive(fun () -> rule')) (snd grm)
                 else snd grm in
    (new_name, Set.fold (fun acc_rules name -> 
                          let prev_name = get_prev_name token name in
                          Map.add name
                                  (make_derive(fun () -> derive_rule !!(get prev_name grm)))
                                  acc_rules
                        ) rules' undefined)

  let derive token grm : 'a Grm (* when 'a :> IToken *) =
    let nullables = compute_nullable grm in
    derive_with token grm nullables

  (* Compute the FIRST-set for grm *)
  let compute_first_with grm nullables =
    let fold_first_maps map_left map_right = 
      Map.fold 
        (fun acc_map name first_set -> 
          acc_map |> match Map.tryFind name acc_map with
                     | None -> Map.add name first_set 
                     | Some first_set_acc -> Map.add name (Set.union first_set_acc first_set))
        map_right map_left in
    let rec step first_map_acc name rule = 
      match rule with
      | Epsilon | Empty -> Map.add name Set.empty first_map_acc
      | NT other -> 
          match Map.tryFind other first_map_acc with
          | Some first_set -> Map.add name first_set first_map_acc
          | None -> Map.add name Set.empty first_map_acc
      | Token tok -> Map.add name (Set.ofList [tok]) first_map_acc 
      | Disj (l, r) -> let map_left = step first_map_acc name l in
                       let map_right = step first_map_acc name r in
                       fold_first_maps map_left map_right
      | Conj (l, r) -> let map_left = step first_map_acc name l in
                       if not(is_nullable_rule l nullables)
                       then map_left
                       else fold_first_maps map_left (step first_map_acc name r) in
    let first_step first_map = 
      Map.fold (fun acc name d_rule -> step acc name !!d_rule) 
               first_map (snd grm) in
    fix first_step Map.empty 
    
