
type dfa_state = int

(**Try to reuse as much as we can from NFAs*)
type dfa_transition = NFA.nfa_transition

type dfa = {
  start_state : dfa_state;
  end_states : NFA.StateSet.t;

  transition_map : (dfa_transition list) NFA.TransitionMap.t
}

let string_of_dfa dfa =
  let start_state_string = 
  "start_state: " ^ (string_of_int dfa.start_state) ^ "\n"
  in

  let end_state_string = (NFA.StateSet.fold (fun state acc_str -> 
    acc_str ^ (string_of_int state) ^ ", " 
  ) dfa.end_states "end_states: ") ^ "\n"
  in

  let map_string = NFA.TransitionMap.fold (fun state transition_list acc ->
    acc ^
    "state: " ^ (string_of_int state) ^ "\n" ^
    "transitions: " ^ 
    (
      List.fold_left ( fun acc_str transition ->
        acc_str ^ " " ^ (NFA.string_of_nfa_transition transition) ^ ", "
      ) "" transition_list
    ) ^ "\n"
    ) dfa.transition_map ""
  in
  start_state_string ^ end_state_string ^ map_string

(**Helper - check if any set in set list is equal to set - returns (bool,  index)*)
let set_list_contains_set set_list set =
  let index = List.find_index (fun elt -> NFA.StateSet.equal elt set) set_list in
  match index with
  | None -> (false, -1)
  | Some i -> (true, i)

(**Given an nfa and a set of states - get the unique NONEMPTY transition LABELS coming out of that set*)
let get_transition_label_set (nfa : NFA.nfa) state_set =
  (**iterate over each state*) 
  NFA.StateSet.fold (fun state acc_label_set ->
    let transition_list_opt = NFA.StateMap.find_opt state nfa.transition_map in
    match transition_list_opt with
    | None -> acc_label_set
    | Some transition_list ->
      (**iterate over each transition from this state, adding its label to the total set*)
      List.fold_left (fun acc_label_set transition -> 
        if NFA.transition_is_empty transition then
          acc_label_set
        else
          NFA.LabelSet.add transition.label acc_label_set
      ) acc_label_set transition_list
  ) state_set NFA.LabelSet.empty 

(**return whether this state has transitions out with the given label, and the to states in that case*)
let state_has_transitions (nfa : NFA.nfa) state transition_label =
  let transition_list_opt = NFA.StateMap.find_opt state nfa.transition_map in
  match transition_list_opt with
  | None -> []
  | Some transition_list -> 
    List.fold_left (fun acc_found transition ->
      if transition.NFA.label = transition_label then
        transition.to_state :: acc_found
      else
        acc_found
    ) [] transition_list


(**Get list of subsets that are reachable from this subset via some transition*)
(**Ignore empty transitions!*)
let get_reachable_subsets nfa state_set epsilon_closure_map =
  let transition_label_set = get_transition_label_set nfa state_set in
  (**iterate over each unique transition label*)
  NFA.LabelSet.fold ( fun label (acc_reached_subsets, acc_labels) -> 
    match label with
    | Empty -> (acc_reached_subsets, acc_labels)
    | Character _ | CharRange _ ->
      (**iterate over all states, accumulate subset reachable from the original subset via this transition label*)
      let init_subset = 
      NFA.StateSet.fold (fun state acc_subset ->
        let reachable_from_state_via_transition = state_has_transitions nfa state label in
        NFA.StateSet.union acc_subset (NFA.StateSet.of_list reachable_from_state_via_transition)
      ) state_set NFA.StateSet.empty
      in
      (**get epsilon closure of the set*)
      ((NFA.compute_epsilon_closure_of_set init_subset epsilon_closure_map) :: acc_reached_subsets, 
      label :: acc_labels)
  ) transition_label_set ([], [])


(**Subset construction algorithm to convert NFA -> DFA*)
let construct_dfa nfa =
  let epsilon_closure_map = NFA.compute_all_epsilon_closures nfa in
  let initial_set = NFA.StateSet.of_list (NFA.StateMap.find nfa.start_state epsilon_closure_map) in

  (**all subset states reachable from current_finished_set_list are either in current_finished_set_list or new_set_list*)
  (**new_set_list = subset states to explore transitions from*)
  (**returns list of all unique reachable subsets and list of all transitions, where the to/from states are the indices of the subsets corresponding to those states,*)
  (**indexed from the BACK of the list (since we build the list by prepending and we need the indices to stay stable)*)
  let rec construct_subset_states current_finished_set_list new_set_list found_transitions =    
    let current_finished_sets_size = List.length current_finished_set_list in
    let new_set_list_size = List.length new_set_list in
    let potential_new_new_sets, _ , partial_transition_list = 
    (**iterate over all new sets*)
    (**current index will contain the current index in the new_set_list we are at*)
    (**partial transition list will contain the list of new transitions we have built -we don't know*)
    (**The end state yet, but we know the start state (which is the index of the subset in the list)*)
    (** new_set_list @ current_finished_set_list  - we index from the back since we are pre-pending*)
    List.fold_left (fun (acc_new_subsets, current_index, acc_partial_transition_list) subset -> 
      let reachable_from_this_subset, label_list = get_reachable_subsets nfa subset epsilon_closure_map in

      (**The index of this subset (which will be the start state of this transition), starting from the end of*)
      (**The current finished sets size list*)
      let this_subset_index = (new_set_list_size - 1 - current_index) + current_finished_sets_size in

      (**We need to fold_right in order to maintain the order of the list*)
      let new_partial_transition_list = List.fold_right (fun label acc_transition_list  ->
          let new_partial_transition : (NFA.nfa_transition) = {
            from_state = this_subset_index;
            to_state = -1;
            label = label;
          }
          in
          new_partial_transition :: acc_transition_list
        ) label_list []
      in
      (
        reachable_from_this_subset @ acc_new_subsets, 
        current_index + 1, 
        new_partial_transition_list @ acc_partial_transition_list
      )
    ) ([], 0, []) new_set_list 
    in

    (**Filter out previously encountered subsets and fill in partial transitions with their final to state,*)
    (**Which is the index of the to subset in the list*)
    (**We iterate over the partial transition list as well, so we process the head and pass the tail through to the next function*)
    let new_new_sets, new_transitions, _ = List.fold_left (fun (acc_new_sets, acc_new_transitions, partial_transition_list_tail) subset ->
      
      let (in_acc_new_sets, acc_new_set_index) = set_list_contains_set acc_new_sets subset in
      let (in_new_set_list, new_set_list_index) = set_list_contains_set new_set_list subset in
      let (in_finished_set_list, finished_set_list_index) = set_list_contains_set current_finished_set_list subset in

      if in_finished_set_list then
        (**we index from the back of the list so indices stay consistent!*)
        let to_state = current_finished_sets_size - 1 - finished_set_list_index in
        let new_transition : (NFA.nfa_transition) = {(List.hd partial_transition_list_tail) with to_state = to_state;} in
        (acc_new_sets, new_transition :: acc_new_transitions, List.tl partial_transition_list_tail)
      else if in_new_set_list then
        let to_state = new_set_list_size - 1 - new_set_list_index + current_finished_sets_size in
        let new_transition : (NFA.nfa_transition) = {(List.hd partial_transition_list_tail) with to_state = to_state; } in
        (acc_new_sets, new_transition :: acc_new_transitions, List.tl partial_transition_list_tail)
      else if in_acc_new_sets then
        let acc_new_sets_size = List.length acc_new_sets in
        let to_state = acc_new_sets_size - 1 - acc_new_set_index + new_set_list_size + current_finished_sets_size in
        let new_transition : (NFA.nfa_transition) = {(List.hd partial_transition_list_tail) with to_state = to_state;} in
        (acc_new_sets, new_transition :: acc_new_transitions, List.tl partial_transition_list_tail)
      else
        (**This is a new subset - add it onto the list*) 
        let acc_new_sets_size = List.length acc_new_sets in
        let to_state = acc_new_sets_size + new_set_list_size + current_finished_sets_size in
        let new_transition : (NFA.nfa_transition) = {(List.hd partial_transition_list_tail) with to_state = to_state;} in
        (subset::acc_new_sets, new_transition :: acc_new_transitions, List.tl partial_transition_list_tail)
    ) ([], [], partial_transition_list) potential_new_new_sets

    in

    let new_current_finished_set_list = new_set_list @ current_finished_set_list in
    let new_found_transitions = new_transitions @ found_transitions in 

    (**No new sets found - terminate*)
    if List.is_empty new_new_sets then
      (new_current_finished_set_list, new_found_transitions)
    else
      construct_subset_states new_current_finished_set_list new_new_sets new_found_transitions
  in

  let subset_states, transition_list = construct_subset_states [] [initial_set] [] in

  (**Final states are any states containing the original NFA's end state*)
  let final_states, _ =
    List.fold_left (fun (final_state_set_acc, index) subset ->
      if NFA.StateSet.mem nfa.end_state subset then 
        (NFA.StateSet.add index final_state_set_acc, index - 1)
      else
        (final_state_set_acc, index - 1)
    ) (NFA.StateSet.empty, (List.length subset_states) - 1) subset_states
  in

  (**Build the transition map from the transition list*)
  let final_transition_map = List.fold_left (fun curr_map (transition : NFA.nfa_transition) -> 
    let from_state = transition.from_state in
    match NFA.TransitionMap.find_opt from_state curr_map  with
    | Some transition_list -> 
      NFA.TransitionMap.add from_state (transition :: transition_list) (NFA.TransitionMap.remove from_state curr_map)
    | None ->
      NFA.TransitionMap.add from_state [transition] curr_map
  ) NFA.TransitionMap.empty transition_list

  in 
  {
    start_state = 0;
    end_states = final_states;
    transition_map = final_transition_map;
  }

let dfa_from_regex regex =
  construct_dfa (Regex.parse_regex_string regex)

(**Feed a string through a DFA starting from index i and get the maximum length that will be accepted*)
(**Returns length, so if j is returned then up to i + j - 1 is accepted*)
let get_longest_dfa_match dfa str i =
  (*j = index in the string to check NEXT (not yet processed), current_max = length of longest accepted prefix*)
  let rec get_longest_dfa_match_rec dfa str j current_max curr_state =

    (**if we are in a final state, then the string "[i,j)" was accepted, so get a new max*)
    let new_max = if NFA.StateSet.mem curr_state dfa.end_states  then Int.max (j-i) current_max else current_max in
    
    if j >= String.length str then
      (**We are at the end of the string and there are no epsilon transitions in a DFA, so finish*)
      new_max
    else
      let opt_transition_list = NFA.TransitionMap.find_opt curr_state dfa.transition_map in
      match opt_transition_list with
      (**Nowhere to proceed - the best so far is what we return*)
      | None -> new_max
      | Some transition_list ->
        List.fold_left (fun acc transition ->
            (**There should be at most one accepting transition for this character in a DFA, and no emptys!*)
            let (accepts, num_accepted) = 
              (**If there is a next char to process, we can proceed down any accepting transition*)
              NFA.transition_accepts_char transition str.[j]
            in
            if accepts then
              Int.max acc
              (**proceed down this transition, since it's a match*)
              (get_longest_dfa_match_rec dfa str (j + num_accepted) new_max transition.to_state)
            else
              acc
        ) new_max transition_list
  in
  (**Start current max accepted at -1*)
  get_longest_dfa_match_rec dfa str i (-1) dfa.start_state 

let dfa_accepts_str dfa str = 
  (get_longest_dfa_match dfa str 0) = String.length str

