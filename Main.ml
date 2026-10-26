open Lexer.NFA

let test_nfa = string_nfa "test"

let kleene_star_test_nfa = nfa_kleene_star test_nfa



let () =
  assert (nfa_accepts_string kleene_star_test_nfa "testtesttesttest");
  assert (nfa_accepts_string kleene_star_test_nfa "");
  assert (nfa_accepts_string kleene_star_test_nfa "test");
  assert (not (nfa_accepts_string kleene_star_test_nfa "testtesttesrtest"))


let abcd_nfa = string_nfa "abcd"

let begin_end_concat_nfa = nfa_concatenate abcd_nfa (nfa_concatenate kleene_star_test_nfa abcd_nfa)

let () =
  assert (nfa_accepts_string begin_end_concat_nfa "abcdtesttestabcd");
  assert (nfa_accepts_string begin_end_concat_nfa "abcdabcd");
  assert (not (nfa_accepts_string begin_end_concat_nfa "testtestabcd"));
  assert (not (nfa_accepts_string begin_end_concat_nfa "abcdtest"));
  assert (not (nfa_accepts_string begin_end_concat_nfa "abcdtesabcd"))

let () = print_endline "Acceptor tests passed!"
