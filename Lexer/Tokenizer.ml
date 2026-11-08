module StringMap = Map.Make(String)

(* A token is a type + lexeme *)
type token = {
  token_type : string;
  lexeme : string;
}

let token_to_string token =
  "(" ^ token.token_type ^ ":" ^ token.lexeme ^ ")"

let token_list_to_string token_list = 
  List.fold_left (fun acc_string token -> acc_string ^ (token_to_string token)) "" token_list

(* Holds a set of keywords and a set of NFAs for tokenizing strings *)
type tokenizer = {
  (**map keyword -> corresponding keyword token name*)
  keywords : string StringMap.t;
  (**map token type -> NFA classifier for that type*)
  classifiers : DFA.dfa StringMap.t;
}

(**Takes a list of (TOKEN_TYPE, keyword) pairs and a list of (TOKEN_TYPE, regex) pairs and returns a tokenizer*)
let build_tokenizer keyword_pair_list regex_classifier_list =
  let keyword_map = List.fold_left ( fun acc_map (keyword_type, keyword) ->
    StringMap.add keyword keyword_type acc_map
  ) StringMap.empty keyword_pair_list
  in

  let classifier_map = List.fold_left (fun acc_map (token_type, regex) -> 
    let dfa = DFA.dfa_from_regex regex in
    StringMap.add token_type dfa acc_map
  ) StringMap.empty regex_classifier_list
  in

  {keywords = keyword_map; classifiers = classifier_map}

(*Takes a string and returns a list of tokens*)
(*Tokenization algorithm is as follows: *)
(*The longest accepted prefix of str with respect to each DFA is computed, then*)
(*the max of those is taken and the lexeme is classified based on which DFA it was assigned to*)
(*If the lexeme happens to be a keyword, then it will instead become a keyword token*)
let tokenize_string tokenizer str =
  let rec tokenize_string_from i token_list =
    if i >= String.length str then
      (**We are done!*)
      token_list
    else
    (**Accumulate the max length and token type of max length over all DFAs*)
    let (token_length, token_type) = StringMap.fold (fun token_type dfa (max_length, token_type_acc) ->
      let match_length = DFA.get_longest_dfa_match dfa str i in
      if match_length > max_length then
        (match_length, token_type)
      else
        (max_length, token_type_acc)
    ) tokenizer.classifiers (-1, "")
    in
    let () = assert(token_length > 0) in
    let lexeme = String.sub str i token_length in
    let token = 
      match (StringMap.find_opt lexeme tokenizer.keywords) with
      | None -> {token_type = token_type; lexeme = lexeme}
      | Some keyword -> {token_type = keyword; lexeme = lexeme}
    in
    tokenize_string_from (i + token_length) (token :: token_list)
  in
  (**We prepend tokens to the list, so it must be reversed at the end - always have an EOF token at the end*)
  List.rev ({token_type = "$EOF$"; lexeme = "$EOF$"} :: (tokenize_string_from 0 []))

  