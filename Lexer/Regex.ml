


let transition_for_single_char from_state to_state c =
  let open NFA in
  {from_state = from_state; to_state = to_state; label = Character c;}

let transition_for_char_range from_state to_state c_start c_end =
  let open NFA in
  {from_state = from_state; to_state = to_state; label = CharRange (c_start, c_end);}

(**Add a char transition from start_state->end_state in an NFA*)
let add_char_transition c (nfa: NFA.intermediate_nfa) =
  let open NFA in
  let new_transition = transition_for_single_char nfa.start_state nfa.end_state c in
  add_transition nfa new_transition

let add_char_range_transition c_start c_end (nfa: NFA.intermediate_nfa) =
  let open NFA in
  let new_transition = transition_for_char_range nfa.start_state nfa.end_state c_start c_end in
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
        (**This must be the closing bracket - we are done!*)
        current_nfa
    | c -> 
      (**Because we assume that the input is well-formed an everything before is parsed, we handle '[' and '-' the same as any other char*)
      (**We must check if this is a range expression (next char is a - )*)
      (**Note there must be a next char because we have not hit the ']' yet*)
      if regex.[j+1] != '-' then
        (**not a range expression - treat as regular character*)
        let new_nfa = add_char_transition c current_nfa in
        parse_bracket_expression_rec regex (j+1) new_nfa
      else
        (**could be a range expression - get the end*)
        (**note that there must be a valid character past the - in a valid regex expression, so this is valid*)
        let past_char = regex.[j+2] in
        if not (past_char = ']') then
          (**this is a range expression -> add that range transition to the nfa*)
          let new_nfa = add_char_range_transition c past_char current_nfa in
          (**pickup parsing past the end of the bracket expression (i.e. past the end char)*)
          parse_bracket_expression_rec regex (j+3) new_nfa
        else
          (**This must be the last character in the bracket expression - note that something like a-]] is not valid*)
          (**Just treat current character like a normal character, then parse the next char as normal*)
          let new_nfa = add_char_transition c current_nfa in
          parse_bracket_expression_rec regex (j+1) new_nfa
  in
  ((parse_bracket_expression_rec regex (i+1) start_nfa), next_state + 2)

(**Takes a POSIX basic regex string and produces an equivalent NFA*)
let parse_regex_string regex =
  let nfa, _ = parse_bracket_expression regex 0 0 in
  NFA.finalize_nfa nfa