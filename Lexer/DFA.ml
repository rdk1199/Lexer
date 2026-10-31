
type dfa_state = int

(**Try to reuse as much as we can from NFAs*)
type dfa_transition = NFA.nfa_transition

type dfa = {
  start_state : dfa_state;
  end_states : NFA.StateSet.t;

  transition_map : (dfa_transition list) NFA.StateMap.t
}

(**Helper - check if any set in set list is equal to set*)
let rec set_list_contains_set set_list set =
  match set_list with
  | [] -> false
  | head :: tail ->
    if NFA.StateSet.equal head set then
      true
    else
      set_list_contains_set tail set

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
let get_reachable_subsets nfa state_set epsilon_closure_map =
  let transition_label_set = get_transition_label_set nfa state_set in
  (**iterate over each unique transition label*)
  NFA.LabelSet.fold ( fun label acc_reached_subsets -> 
    
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
    (NFA.compute_epsilon_closure_of_set init_subset epsilon_closure_map) :: acc_reached_subsets
  ) transition_label_set []


(**Subset construction algorithm to convert NFA -> DFA*)
let construct_dfa nfa =
  let epsilon_closure_map = NFA.compute_all_epsilon_closures nfa in
  let initial_set = NFA.StateSet.of_list (NFA.StateMap.find nfa.start_state epsilon_closure_map) in

  (**all subset states reachable from current_finished_set_list are either in current_finished_set_list or new_set_list*)
  (**new_set_list = subset states to explore transitions from*)
  let rec construct_subset_states nfa epsilon_closure_map current_finished_set_list new_set_list =    
    let potential_new_new_sets = 
    (**iterate over all new sets*)
    List.fold_left (fun acc_new_subsets subset -> 
      let reachable_from_this_subset = get_reachable_subsets nfa subset epsilon_closure_map in
      reachable_from_this_subset @ acc_new_subsets
    ) [] new_set_list
    in

    (**Filter out previously encountered subsets*)
    let new_new_sets = List.fold_left (fun acc_new_sets subset ->
      if (not (set_list_contains_set acc_new_sets subset)) &&
         (not (set_list_contains_set new_set_list subset)) &&
         (not (set_list_contains_set current_finished_set_list subset))
      then
        subset :: acc_new_sets
      else
        acc_new_sets
    ) [] potential_new_new_sets

    in

    let new_current_finished_set_list = new_set_list @ current_finished_set_list in

    if List.is_empty new_new_sets then
      new_current_finished_set_list
    else
      construct_subset_states nfa epsilon_closure_map new_current_finished_set_list new_new_sets
  in

  let subset_states = construct_subset_states nfa epsilon_closure_map [] [initial_set] in
  subset_states
