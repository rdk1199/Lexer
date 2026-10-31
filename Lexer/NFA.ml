type nfa_state = int

module TransitionMap = Map.Make(Int)
module StateMap = Map.Make(Int)
module StateSet = Set.Make(Int)

type nfa_transition_label =
  | Empty (**epsilon transition*)
  | Character of char (**single char transition*)
  | CharRange of char * char (**all chars between start and end, inclusive*)

module NFATransitionLabel = struct 

  type t = nfa_transition_label

  let compare a b =
    match a,b with
    | Empty, Empty -> 0
    | Empty, _ -> -1
    | _, Empty -> 1
    | Character a_char, Character b_char -> Char.compare a_char b_char
    | Character _, CharRange _ -> -1
    | CharRange _, Character _ -> 1
    | CharRange (a_start, a_end), CharRange (b_start, b_end) ->
      let start_comp = Char.compare a_start b_start in
      if start_comp = 0 then
        Char.compare a_end b_end
      else
        start_comp
end

module LabelSet = Set.Make(NFATransitionLabel)

let string_of_nfa_transition_label label =
  match label with
  | Empty -> "empty"
  | Character c -> String.of_char c
  | CharRange (c_start, c_end) -> (String.of_char c_start) ^ "-" ^ (String.of_char c_end)

type nfa_transition = {
  from_state : nfa_state;
  to_state : nfa_state;
  label : nfa_transition_label;
}

let string_of_nfa_transition transition =
  string_of_int transition.from_state ^ "---" ^
  (string_of_nfa_transition_label transition.label) ^ "-->" ^
  string_of_int transition.to_state


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


(**Intermediate NFA meant for building other NFAs out of - not great for parsing*)
type intermediate_nfa = {
  start_state : nfa_state;
  end_state : nfa_state;

  (**All transitions in the NFA state machine*)
  transitions : nfa_transition list
}

(**NFA accepting no strings*)
let empty_intermediate_nfa = {
  start_state = 0;
  end_state = -1;
  transitions = [];
}

let add_transition intermediate_nfa transition =
  {intermediate_nfa with transitions = transition :: intermediate_nfa.transitions}

(**NFA for string matching - constructed from intermediate_nfa once operations are done*)
type nfa = {
  start_state : nfa_state;
  end_state : nfa_state;

  transition_map : (nfa_transition list) TransitionMap.t;
}

let string_of_nfa nfa =
  let states_string = 
  "start_state: " ^ (string_of_int nfa.start_state) ^ "\n" ^
  "end_state: " ^ (string_of_int nfa.end_state) ^ "\n"
  in

  let map_string = TransitionMap.fold (fun state transition_list acc ->
    acc ^
    "state: " ^ (string_of_int state) ^ "\n" ^
    "transitions: " ^ 
    (
      List.fold_left ( fun acc_str transition ->
        acc_str ^ " " ^ (string_of_nfa_transition transition) ^ ", "
      ) "" transition_list
    ) ^ "\n"
    ) nfa.transition_map ""
  in
  states_string ^ map_string


(**Find the epsilon closure of a state in the nfa*)
let build_epsilon_closure_for_state nfa nfa_state =
  let rec build_epsilon_closure_rec nfa nfa_state state_set =
      let new_set = StateSet.add nfa_state state_set in
      let new_state = ((StateSet.cardinal new_set) > (StateSet.cardinal state_set)) in
      if new_state then
        let opt_transition_list = TransitionMap.find_opt nfa_state nfa.transition_map in
        match opt_transition_list with
        | None -> new_set
        | Some transition_list ->
          List.fold_left (fun acc_set transition ->
            if transition_is_empty transition then
              StateSet.union acc_set (build_epsilon_closure_rec nfa transition.to_state new_set)
            else
              acc_set
            ) new_set transition_list
      else
        (**We have already processed this state - don't traverse*)
        new_set
  in
  StateSet.to_list (build_epsilon_closure_rec nfa nfa_state StateSet.empty)


let get_all_nfa_states nfa =
  let rec get_states_rec nfa state state_set =
    if StateSet.mem state state_set then
      (*already traversed this state - return*)
      state_set
    else
      let new_set = StateSet.add state state_set in
      let opt_transition_list = TransitionMap.find_opt state nfa.transition_map in
      match opt_transition_list with
      | None -> new_set
      | Some transition_list ->
        List.fold_left (fun acc_set transition ->
          StateSet.union acc_set (get_states_rec nfa transition.to_state new_set)  
        ) new_set transition_list
  in
  StateSet.to_list (get_states_rec nfa nfa.start_state StateSet.empty)

(*Returns a map state->epsilon closure*)
let compute_all_epsilon_closures nfa = 
  let state_list = get_all_nfa_states nfa in
  List.fold_left (fun acc state ->
    StateMap.add state (build_epsilon_closure_for_state nfa state) acc
  ) StateMap.empty state_list

(**given a state subset and the map of each state to its epsilon closure, return epsilon closure of the subset*)
let compute_epsilon_closure_of_set state_set epsilon_closure_map =
  (**For each state in the state_set*)
  StateSet.fold (fun state acc_set_epsilon_closure  ->
    let state_epsilon_closure_opt = StateMap.find_opt state epsilon_closure_map in
    match state_epsilon_closure_opt with
    | None -> acc_set_epsilon_closure
    (**For each state in the epsilon closure of this state, add to total set epsilon_closure*)
    | Some state_epsilon_closure -> List.fold_left (fun acc_epsilon_closure epsilon_closure_state ->
      StateSet.add epsilon_closure_state acc_epsilon_closure
      ) acc_set_epsilon_closure state_epsilon_closure
  ) state_set StateSet.empty


(**Traverse all the empty transitions to see if there is a path to the final state*)
(**Note that an evil NFA could probably get this to loop forever, as is true of the other string matching functions*)
(**However, an NFA constructed through regex parsing should not allow that*)
(**This could be prevented with a visited list or even just depth limit, but it's OK*)
let rec nfa_accepts_empty_str nfa =
  (**true if nfa starting from state curr_state accepts empty string *)
  let rec nfa_state_accepts_empty nfa curr_state = 
    if curr_state = nfa.end_state then 
      true
    else
      let opt_transition_list = TransitionMap.find_opt curr_state nfa.transition_map in
      match opt_transition_list with
      | None -> false
      | Some transition_list ->
        List.fold_left (fun acc transition ->
          acc ||
          if transition_is_empty transition then
            nfa_state_accepts_empty nfa transition.to_state
          else
            false
        ) false transition_list

  in
  nfa_state_accepts_empty nfa nfa.start_state

(**Feed a string through an NFA starting from index i and get the maximum length that will be accepted*)
(**Returns length, so if j is returned then up to i + j - 1 is accepted*)
let get_longest_nfa_match nfa str i =
  (*j = index in the string to check NEXT (not yet processed), current_max = length of longest accepted prefix*)
  let rec get_longest_nfa_match_rec nfa str j current_max curr_state =

    (**if we are in the final state, then the string "[i,j)" was accepted, so get a new max*)
    let new_max = if curr_state = nfa.end_state then Int.max (j-i) current_max else current_max in
    let opt_transition_list = TransitionMap.find_opt curr_state nfa.transition_map in
  
    match opt_transition_list with
    (**Nowhere to proceed - the best so far is what we return*)
    | None -> new_max
    | Some transition_list ->
      List.fold_left (fun acc transition ->
          let (accepts, num_accepted) = 
            (**If there is a next char to process, we can proceed down any accepting transition*)
            (**Otherwise, only proceed down empty ones*)
            if j < String.length str then
               transition_accepts_char transition str.[j]
            else
              (transition_is_empty transition, 0)
          in
          if accepts then
            Int.max acc
            (get_longest_nfa_match_rec nfa str (j + num_accepted) new_max transition.to_state)
          else
            acc
      ) new_max transition_list
  in
  get_longest_nfa_match_rec nfa str i 0 nfa.start_state 


let nfa_accepts_str nfa str = 
  let length = String.length str in
  if length = 0 then
    nfa_accepts_empty_str nfa
  else
    (get_longest_nfa_match nfa str 0) = (String.length str)


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

(**Thompson's construction concatenation - no new states needed*)
(**ASSUMES the operands' state sets are disjoint*)
let nfa_concat (nfa1 : intermediate_nfa) (nfa2 : intermediate_nfa) =
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
  concat_nfa


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

(*Takes an intermediate nfa and produces an equivalent nfa with no epsilon transitions except at the very end 
(so we can have only one final state)*)
let remove_all_epsilon_transitions intermediate_nfa next_state =
  let finalized_nfa = finalize_nfa intermediate_nfa in
  let to_epsilon_closure_map = compute_all_epsilon_closures finalized_nfa in
  let state_list = get_all_nfa_states finalized_nfa in

  let new_transition_list = 
    (**For all states in the NFA*)
    List.fold_left (fun acc_transition_list state ->
      (**For all states in the epsilon closure of this state*)
      let from_state_epsilon_closure_opt = StateMap.find_opt state to_epsilon_closure_map in
      match from_state_epsilon_closure_opt with
      | None -> acc_transition_list
      | Some from_state_epsilon_closure ->
        List.fold_left (fun acc_transition_list from_epsilon_state ->
          (**For all transitions out of this state in the epsilon closure*)
          let transition_list_opt = TransitionMap.find_opt from_epsilon_state finalized_nfa.transition_map in
          match transition_list_opt with
          | None -> acc_transition_list
          | Some transition_list ->
            List.fold_left(fun acc_transition_list transition ->
              if transition_is_empty transition then
                acc_transition_list
              else
                (**For all states in the epsilon closure in the to state*)
                let to_state_epsilon_closure_opt = StateMap.find_opt transition.to_state to_epsilon_closure_map in
                match to_state_epsilon_closure_opt with
                | None -> acc_transition_list
                | Some to_state_epsilon_closure ->
                  List.fold_left(fun acc_transition_list to_epsilon_state ->
                    let new_transition = {
                      from_state = state;
                      to_state = to_epsilon_state;
                      label = transition.label;
                    }
                    in
                    new_transition :: acc_transition_list
                  ) acc_transition_list to_state_epsilon_closure
            ) acc_transition_list transition_list
        ) acc_transition_list from_state_epsilon_closure 
    ) [] state_list
  in

  (*let new_start_state = next_state in*)
  let new_final_state = next_state in

  (*Find all "final" states - we will have just one actual final state and use epsilon transitions only here*)
  let final_transition_list =
    StateMap.fold (fun state epsilon_closure acc_list ->
      if (List.mem intermediate_nfa.end_state epsilon_closure) then
        let new_transition = {
          from_state = state;
          to_state = new_final_state;
          label = Empty;
        }
        in
        new_transition :: acc_list
      else
        acc_list
    ) to_epsilon_closure_map new_transition_list
  in

  let final_intermediate_nfa = {
    start_state = intermediate_nfa.start_state;
    end_state = new_final_state;
    transitions = final_transition_list;
  }
  in
  final_intermediate_nfa, next_state + 1
  
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


