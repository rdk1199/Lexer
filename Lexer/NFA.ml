type nfa_state = int

module TransitionMap = Map.Make(Int)

type nfa_transition_label =
  | Empty (**epsilon transition*)
  | Character of char (**single char transition*)
  | CharRange of char * char (**all chars between start and end, inclusive*)
  | CharList of char list  (**transition for each char in list*)

type nfa_transition = {
  from_state : nfa_state;
  to_state : nfa_state;
  label : nfa_transition_label;
}

let transition_is_empty transition =
  match transition.label with
  | Empty -> true
  | _ -> false

(**Returns whether transition accepts char and, 
if so, the number of characters it accepts (0 in case of empty transition, 1 otherwise)*)
let transition_accepts_char transition c =
  match transition.label with
  | Empty -> (true, 0)
  | Character ch -> (c = ch, 1)
  | CharRange (ch_start, ch_end) -> (ch_start <= c && c <= ch_end, 1)
  | CharList ch_list -> (List.mem c ch_list, 1)

(**Intermediate NFA meant for building other NFAs out of - not great for parsing*)
type intermediate_nfa = {
  start_state : nfa_state;
  end_state : nfa_state;

  (**All transitions in the NFA state machine*)
  transitions : nfa_transition list
}

let add_transition intermediate_nfa transition =
  {intermediate_nfa with transitions = transition :: intermediate_nfa.transitions}

(**NFA for string matching - constructed from intermediate_nfa once operations are done*)
type nfa = {
  start_state : nfa_state;
  end_state : nfa_state;

  transition_map : (nfa_transition list) TransitionMap.t;
}

(**Use recursive backtracking to feed string through the NFA - not particularly efficient*)
let rec nfa_accepts_str nfa str =
  (**true if nfa starting from state curr_state accepts suffix of str starting at index i *)
  let rec nfa_state_accepts_suffix nfa str i curr_state =
    let opt_transition_list = TransitionMap.find_opt curr_state nfa.transition_map in
    if i >= String.length str then
      if curr_state = nfa.end_state then 
        true
      else
        match opt_transition_list with
        | None -> false
        | Some transition_list ->
          List.fold_left (fun acc transition ->
            acc ||
            if transition_is_empty transition then
              nfa_state_accepts_suffix nfa str i transition.to_state
            else
              false
          ) false transition_list

    else
      let next_char = str.[i] in
      match opt_transition_list with
      | None -> false
      | Some transition_list ->
        List.fold_left (fun acc transition -> 
          acc ||
          let (accepts, num_accepted) = transition_accepts_char transition next_char in
          if accepts then
            nfa_state_accepts_suffix nfa str (i + num_accepted) transition.to_state
          else
            false
    ) false transition_list
  in
  nfa_state_accepts_suffix nfa str 0 nfa.start_state



(**Thompson's construction union - next_state contains the next available state we can add*)
let nfa_union  (nfa1 : intermediate_nfa) (nfa2 : intermediate_nfa) next_state =
  let new_start = next_state in
  let new_end = next_state + 1 in
  let new_next_state = next_state + 2 in

  let new_start_to_old_start_1 = {
    from_state = new_start; 
    to_state = nfa1.start_state;
    label = Empty;
  }
  in

  let new_start_to_old_start_2 = {
    from_state = new_start; 
    to_state = nfa2.start_state;
    label = Empty;
  }
  in

  let old_finish_1_to_new_finish = {
    from_state = nfa1.end_state; 
    to_state = new_end;
    label = Empty;
  }
  in

  let old_finish_2_to_new_finish = {
    from_state = nfa2.end_state; 
    to_state = new_end;
    label = Empty;
  }
  in

  let new_transition_list = [
                              new_start_to_old_start_1; 
                              new_start_to_old_start_2;
                              old_finish_1_to_new_finish; 
                              old_finish_2_to_new_finish;   
                            ]
  in

  let union_nfa =
  {
    start_state = new_start;
    end_state = new_end;
    transitions = new_transition_list @ nfa1.transitions @ nfa2.transitions
  }
  in
  (union_nfa, new_next_state)

(**Thompson's construction concatenation - next_state contains the next available state we can add*)
(**ASSUMES the operands' state sets are disjoint*)
let nfa_concat (nfa1 : intermediate_nfa) (nfa2 : intermediate_nfa) next_state =
  let new_transition = {
    from_state = nfa1.end_state;
    to_state = nfa2.start_state;
    label = Empty;
  }
  in

  let concat_nfa =
  {
    start_state = nfa1.start_state;
    end_state = nfa2.end_state;
    transitions = [new_transition] @ nfa1.transitions @ nfa2.transitions;
  }
  in

  (**no new states created, so next_state stays the same*)
  (concat_nfa, next_state)


(**Thompson's construction Kleene star - next_state contains the next available state we can add*)
let nfa_star (nfa : intermediate_nfa) next_state = 
  let new_start = next_state in
  let new_end = next_state + 1 in

  let new_start_to_old_start = {
    from_state = new_start;
    to_state = nfa.start_state;
    label = Empty;
  }
  in

  let old_end_to_new_end = {
    from_state = nfa.end_state;
    to_state = new_end;
    label = Empty;
  }
  in

  let old_end_to_old_start = {
    from_state = nfa.end_state;
    to_state = nfa.start_state;
    label = Empty;
  }
  in

  let new_start_to_new_end = {
    from_state = new_start;
    to_state = new_end;
    label = Empty;
  }
  in

  let new_transitions = [
    new_start_to_old_start;
    old_end_to_new_end;
    old_end_to_old_start;
    new_start_to_new_end;
  ]
  in

  let star_nfa =
  {
    start_state = new_start;
    end_state = new_end;
    transitions = new_transitions @ nfa.transitions
  }
  in
  (star_nfa, next_state + 2)

let finalize_nfa intermediate_nfa =
  let empty_transition_map = TransitionMap.empty in
  let final_transition_map = List.fold_left (fun curr_map transition -> 
    let from_state = transition.from_state in
    match TransitionMap.find_opt from_state curr_map  with
    | Some transition_list -> 
      TransitionMap.add from_state (transition :: transition_list) (TransitionMap.remove from_state curr_map)
    | None ->
      TransitionMap.add from_state [transition] curr_map
  ) empty_transition_map intermediate_nfa.transitions
  in
  {
    start_state = intermediate_nfa.start_state;
    end_state = intermediate_nfa.end_state;
    transition_map = final_transition_map;
  }

(**nfa for accepting a single char*)
let char_nfa c next_state =
  let start_state = next_state in
  let end_state = next_state + 1 in
  let new_next_state = next_state + 2 in

  let transition = {
    from_state = start_state;
    to_state = end_state;
    label = Character c
  }
  in

  let final_nfa = 
  {
    start_state = start_state;
    end_state = end_state;
    transitions = [transition];
  }
  in
  (final_nfa, new_next_state)

(**nfa that only accepts str*)
let string_nfa str next_state = 
  let start_state = next_state in
  let next_state_1 = next_state + 1 in

  (**recursively build a path NFA*)
  let rec build_string_nfa str i prev_state next_state transition_list =
    if i >= String.length str then
      (**
        We are done - final next state is next_state, prev_state is the final state
        of the whole string NFA, and all transitions are in transition_list
      *)
      (prev_state, next_state, transition_list)
    else
      let new_state = next_state in
      let next_next_state = next_state + 1 in
      let new_transition = {
        from_state = prev_state;
        to_state = new_state;
        label = Character str.[i]
      }
      in
      build_string_nfa str (i+1) new_state next_next_state (new_transition :: transition_list)
  in
  let (final_state, final_next_state, transition_list) = build_string_nfa str 0 start_state next_state_1 [] in
  let final_nfa = 
  {
    start_state = start_state;
    end_state = final_state;
    transitions = transition_list;
  }
  in
  (final_nfa, final_next_state)


