


let transition_for_single_char from_state to_state c =
  let open NFA in
  {from_state = from_state; to_state = to_state; label = Character c;}

let transition_for_char_range from_state to_state c_start c_end =
  let open NFA in
  {from_state = from_state; to_state = to_state; label = CharRange (c_start, c_end);}

let transition_for_all_chars from_state to_state =
  let open NFA in
  (*match everything past new line (in ASCII)*)
  {from_state = from_state; to_state = to_state; label = CharRange (char_of_int 11, char_of_int 127);}

(**Add a char transition from start_state->end_state in an NFA*)
let add_char_transition c (nfa: NFA.intermediate_nfa) =
  let open NFA in
  let new_transition = transition_for_single_char nfa.start_state nfa.end_state c in
  add_transition nfa new_transition

let add_char_range_transition c_start c_end (nfa: NFA.intermediate_nfa) =
  let open NFA in
  let new_transition = transition_for_char_range nfa.start_state nfa.end_state c_start c_end in
  add_transition nfa new_transition

let add_transition_for_all_chars (nfa: NFA.intermediate_nfa) =
  let open NFA in
  let new_transition = transition_for_all_chars nfa.start_state nfa.end_state in
  add_transition nfa new_transition

let nfa_for_char c next_state = 
  let open NFA in
  let nfa = {
    start_state = next_state;
    end_state = next_state + 1;
    transitions = [transition_for_single_char next_state (next_state + 1) c;]
  }
  in
  (nfa, next_state + 2)

let nfa_for_all_chars next_state =
  let open NFA in
  let nfa = {
    start_state = next_state;
    end_state = next_state + 1;
    transitions = [transition_for_all_chars next_state (next_state + 1);]
  }
  in
  (nfa, next_state + 2)

(**Assumes a bracket expression conforming to POSIX standard - outside of that is not supported*)
(**Parse a bracket expression starting at index i (i is the index of the first '[' ) and return an equivalent intermediate nfa*)
(**Per POSIX standard, a bracket expression MUST be nonempty, thus we assume that.*)
(**We don't support ^ operators which substitute any character not in the brackets*)
(**Returns NFA, next NFA state, and index of first char PAST the last bracket (could be past end of whole string)*)
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
        (current_nfa, j+1)
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
  let bracket_nfa, next_index = parse_bracket_expression_rec regex (i+1) start_nfa in
  (bracket_nfa, next_index, next_state + 2)

(**takes a list of nfas and concatenates them in reverse order*)
(** Not tail recursive - not a big deal? *)
let rec reverse_concatenate_list nfa_list =
  let open NFA in
  match nfa_list with
  | [] -> None
  | nfa :: tail ->
    let tail_concat = reverse_concatenate_list tail in
    match tail_concat with
    | None -> Some nfa
    | Some tail_nfa -> Some (NFA.nfa_concat tail_nfa nfa)

let finalize_list nfa_list = 
  let open NFA in
  let concatenated_list = reverse_concatenate_list nfa_list in
  match concatenated_list with
  | None -> empty_intermediate_nfa
  | Some nfa -> nfa

(**\ is an escape character -> map the next character to the appropriate literal character*)
(**i is the index of the \ *)
(**Note we assume valid input, so there must be a char after \ in str*)
let unescape_char str i = 
  let next_char = str.[i+1] in
  match next_char with
  | 'n' -> '\n'
  | 't' -> '\t'
  | 'r' -> '\r'
  | c -> c



(**Takes a POSIX basic regex string and produces an equivalent NFA*)
let parse_regex_string regex =
  (**Recursively parse regex with i as the next character - nfa_list is list of nfas from previous parsing, most recent first*)
  (**Note that we could be parsing a subexpression, so we stop if we hit a ')' and return the index of the )
    or index past the end of the string if we are there - also return the next nfa state!*)
  let rec parse_regex_string_rec regex i nfa_list next_nfa_state =
    if i >= String.length regex || (regex.[i] = ')') then
      begin
      (**we are done*)
      ((finalize_list nfa_list), i, next_nfa_state)
      end
    else
      begin
      let next_char = regex.[i] in
      match next_char with
      | '.' ->
        let all_char_nfa, new_next_nfa_state = nfa_for_all_chars next_nfa_state in
        parse_regex_string_rec regex (i+1) (all_char_nfa :: nfa_list) new_next_nfa_state
      | '[' ->
        let bracket_exp_nfa, next_index, new_next_nfa_state = (parse_bracket_expression regex i next_nfa_state) in
        parse_regex_string_rec regex next_index (bracket_exp_nfa :: nfa_list) new_next_nfa_state
      | '*' -> 
        let star_nfa, new_next_nfa_state = NFA.nfa_star (List.hd nfa_list) next_nfa_state in
        parse_regex_string_rec regex (i+1) (star_nfa :: List.tl nfa_list) new_next_nfa_state
      | '(' ->
        (**This is the start of a sub-expression - parse and return*)
        let sub_nfa, sub_nfa_end_index, new_next_nfa_state = parse_regex_string_rec regex (i+1) [] next_nfa_state in
        (**start parsing after this subexpression*)
        parse_regex_string_rec regex (sub_nfa_end_index + 1) (sub_nfa :: nfa_list) new_next_nfa_state
      | '|' ->
        (**Union -> compress the accumulated list so far and take union with what remains*)
        (**Not tail recursive :( *)
        let finalized_nfa_section = finalize_list nfa_list in
        let remaining_nfa, next_index, new_next_nfa_state = parse_regex_string_rec regex (i+1) [] next_nfa_state in
        let union_nfa, new_new_next_nfa_state = NFA.nfa_union finalized_nfa_section remaining_nfa new_next_nfa_state in
        (**next index is the end of our current regex expression - either a ) or past the string - just pass it through*)
        parse_regex_string_rec regex next_index [union_nfa;] new_new_next_nfa_state
      | '\\' ->
        (**escape - just add next character, since in our case every escaped character just corresponds to itself (i.e. \[ -> '[' )
         We assume a valid input so no additional checking is needed and continue parsing at the one after it*)
        let unescaped_char = unescape_char regex i in
        let char_nfa, new_next_nfa_state = nfa_for_char unescaped_char next_nfa_state in
        (**skip over this backslash and the char after it, since we processed them both*)
        parse_regex_string_rec regex (i+2) (char_nfa :: nfa_list) new_next_nfa_state
      | '?' ->
        (**optional - just pop the last nfa and replace it with the optional version*)
        let optional_nfa = NFA.nfa_option (List.hd nfa_list) in
        parse_regex_string_rec regex (i+1) (optional_nfa :: List.tl nfa_list) next_nfa_state
      | c ->
        (**default*)
        let char_nfa, new_next_nfa_state = nfa_for_char c next_nfa_state in
        parse_regex_string_rec regex (i+1) (char_nfa :: nfa_list) new_next_nfa_state
      end
  in

  let nfa, _ , _ = parse_regex_string_rec regex 0 [] 0 in
  (*let no_epsilon_nfa, _ = NFA.remove_all_epsilon_transitions nfa next_nfa_state in*)
  (*Don't bother removing epsilons since we convert to DFA anyways and don't want the states to explode*)
  NFA.finalize_nfa nfa