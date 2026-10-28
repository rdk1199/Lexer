


let transition_for_single_char from_state to_state c =
  let open NFA in
  {from_state = from_state; to_state = to_state; label = Character c;}

(**Add a char transition from start_state->end_state in an NFA*)
let add_char_transition c (nfa: NFA.intermediate_nfa) =
  let open NFA in
  let new_transition = transition_for_single_char nfa.start_state nfa.end_state c in
  add_transition nfa new_transition

(**Assumes a bracket expression conforming to POSIX standard - outside of that is not supported*)
(**Parse a bracket expression starting at index i (i is the index of the first '[' ) and return an equivalent intermediate nfa*)
(**Per POSIX standard, a bracket expression MUST be nonempty, thus we assume that.*)
(**We don't support ^ operators which substitute any character not in the brackets*)
let parse_bracket_expression regex i next_state =
  let open NFA in

  (**A bracket expression always evaluates to a set of characters, which could include ranges*)
  (**We represent this with a single NFA consisting of two states, connected by all the chars in the bracket expression*)

  (**Start with no transitions - we will add them as we parse through*)
  let start_nfa = {
    start_state = next_state;
    end_state = next_state + 1;
    transitions = [];
  }
  in  

  (**recursively parse a bracket expression starting at index j (j starts AFTER the first bracket)*)
  (**assumes that everything before it has been parsed properly*)
  let rec parse_bracket_expression_rec regex j current_nfa =
    let next_char = regex.[j] in
    match next_char with
    | ']' ->
      (**If this character is meant to be included, then by POSIX standard it must be either the first after the start bracket or last*)
      if j = i+1 then
        (**This is the first char in bracket expression - treat it like any other*)
        (**Note it cannot be the last bracket because that would be an illegal empty bracket expression, which we do not consider*)
        let new_nfa = add_char_transition ']' current_nfa in
        parse_bracket_expression_rec regex (j+1) new_nfa
      else
        (**This must be the last character or the closing bracket*)
        if j = (String.length regex) - 1 || not (regex.[j+1] = ']') then
          (**This is the closing bracket - we are done!*)
          current_nfa
        else
          (**This is not the closing bracket but rather a character to be included!*)
          let new_nfa = add_char_transition ']' current_nfa in
          parse_bracket_expression_rec regex (j+1) new_nfa
    | c -> 
      (**Because we assume that the input is well-formed an everything before is parsed, we handle '[' and '-' the same as any other char*)
      let new_nfa = add_char_transition c current_nfa in
      parse_bracket_expression_rec regex (j+1) new_nfa 
  in
  ((parse_bracket_expression_rec regex (i+1) start_nfa), next_state + 2)

(**Takes a POSIX basic regex string and produces an equivalent NFA*)
let parse_regex_string regex =
  ()