
type dfa_state = int

(**Try to reuse as much as we can from NFAs*)
type dfa_transition = NFA.nfa_transition

type dfa = {
  start_state : dfa_state;
  end_states : NFA.StateSet.t;

  transition_map : (dfa_transition list) NFA.TransitionMap.t
}

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

(**return whether this state has a transition out with the given label, and the to state in that case*)
let state_has_transition (nfa : NFA.nfa) state transition_label =
  let transition_list_opt = NFA.StateMap.find_opt state nfa.transition_map in
  match transition_list_opt with
  | None -> (false, -1)
  | Some transition_list -> 
    List.fold_left (fun (acc_found, acc_state) transition ->
      (acc_found || (transition.NFA.label = transition_label), transition.to_state)
    ) (false, -1) transition_list


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
        let (has_transition, to_state) = state_has_transition nfa state label in
        if has_transition then
          NFA.StateSet.add to_state acc_subset
        else
          acc_subset
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

      let new_partial_transition_list = List.fold_left (fun acc_transition_list label ->
          let new_partial_transition : (NFA.nfa_transition) = {
            from_state = this_subset_index;
            to_state = -1;
            label = label;
          }
          in
          new_partial_transition :: acc_transition_list
        ) [] label_list
      in
      (
        reachable_from_this_subset @ acc_new_subsets, 
        current_index + 1, 
        new_partial_transition_list @ acc_partial_transition_list
      )
    ) (new_set_list, 0, []) [] 
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
        (acc_new_sets, new_transition :: acc_new_transitions, List.tl partial_transition_list)
      else if in_new_set_list then
        let to_state = new_set_list_size - 1 - new_set_list_index + current_finished_sets_size in
        let new_transition : (NFA.nfa_transition) = {(List.hd partial_transition_list_tail) with to_state = to_state; } in
        (acc_new_sets, new_transition :: acc_new_transitions, List.tl partial_transition_list)
      else if in_acc_new_sets then
        let acc_new_sets_size = List.length acc_new_sets in
        let to_state = acc_new_sets_size - 1 - acc_new_set_index + new_set_list_size + current_finished_sets_size in
        let new_transition : (NFA.nfa_transition) = {(List.hd partial_transition_list_tail) with to_state = to_state;} in
        (acc_new_sets, new_transition :: acc_new_transitions, List.tl partial_transition_list)
      else
        (**This is a new subset - add it onto the list*) 
        let acc_new_sets_size = List.length acc_new_sets in
        let to_state = acc_new_sets_size + new_set_list_size + current_finished_sets_size in
        let new_transition : (NFA.nfa_transition) = {(List.hd partial_transition_list_tail) with to_state = to_state;} in
        (subset::acc_new_sets, new_transition :: acc_new_transitions, List.tl partial_transition_list)
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
    ) (NFA.StateSet.empty, List.length subset_states) subset_states
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




