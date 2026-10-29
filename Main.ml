open Lexer.Acceptor
open Lexer.NFA
open Lexer.Regex

  
let test_regex = "[abr-t]"
let test_nfa = parse_regex_string test_regex

let () =
  assert (nfa_accepts_str test_nfa "a");
  assert (nfa_accepts_str test_nfa "b");
  assert (nfa_accepts_str test_nfa "r");
  assert (nfa_accepts_str test_nfa "s");
  assert (nfa_accepts_str test_nfa "t");
  assert (not (nfa_accepts_str test_nfa "f"));
  assert (not (nfa_accepts_str test_nfa "k"))

let test_regex_2 = "[]a-ce-g-]"
let test_nfa_2 = parse_regex_string test_regex_2

let () =
  assert (nfa_accepts_str test_nfa_2 "a");
  assert (nfa_accepts_str test_nfa_2 "b");
  assert (nfa_accepts_str test_nfa_2 "c");
  assert (nfa_accepts_str test_nfa_2 "e");
  assert (nfa_accepts_str test_nfa_2 "f");
  assert (nfa_accepts_str test_nfa_2 "g");
  assert (nfa_accepts_str test_nfa_2 "]");
  assert (nfa_accepts_str test_nfa_2 "-");
  assert (not (nfa_accepts_str test_nfa_2 "d"));
  assert (not (nfa_accepts_str test_nfa_2 "h"))

let () = print_endline "Regex->NFA tests passed!"