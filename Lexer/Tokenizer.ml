module StringMap = Map.Make(String)

(* A token is a type + lexeme *)
type token = {
  token_type : string;
  lexeme : string;
}

(* Holds a set of keywords and a set of NFAs for tokenizing strings *)
type tokenizer = {
  (**map keyword -> corresponding keyword token name*)
  keywords : string StringMap.t;
  (**map token type -> NFA classifier for that type*)
  classifiers : NFA.nfa StringMap.t;
}


(*Takes a string and returns a list of tokens*)
(*Tokenization algorithm is as follows: *)
(*The longest accepted prefix of str with respect to each NFA is computed, then*)
(*the max of those is taken and the lexeme is classified based on which NFA it was assigned to*)
(*If the lexeme happens to be a keyword, then it will instead become a keyword token*)
let tokenize_string tokenizer str =
  ()