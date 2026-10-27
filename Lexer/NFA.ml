
type nfa_state = int

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

(**Intermediate NFA meant for building other NFAs out of - not great for parsing*)
type intermediate_nfa = {
  start_state : nfa_state;
  end_state : nfa_state;

  (**All transitions in the NFA state machine*)
  transitions : nfa_transition list
}

(**Thompson's construction union - next_state contains the next available state we can add*)
let nfa_union nfa1 nfa2 next_state =
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




