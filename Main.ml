open Lexer.Acceptor
open Lexer.NFA

let test_acceptor = string_acceptor "test"

let kleene_star_test_acceptor = acceptor_kleene_star test_acceptor

let () =
  assert (acceptor_accepts_string kleene_star_test_acceptor "testtesttesttest");
  assert (acceptor_accepts_string kleene_star_test_acceptor "");
  assert (acceptor_accepts_string kleene_star_test_acceptor "test");
  assert (not (acceptor_accepts_string kleene_star_test_acceptor "testtesttesrtest"))


let abcd_acceptor = string_acceptor "abcd"

let begin_end_concat_acceptor = acceptor_concatenate abcd_acceptor (acceptor_concatenate kleene_star_test_acceptor abcd_acceptor)

let () =
  assert (acceptor_accepts_string begin_end_concat_acceptor "abcdtesttestabcd");
  assert (acceptor_accepts_string begin_end_concat_acceptor "abcdabcd");
  assert (not (acceptor_accepts_string begin_end_concat_acceptor "testtestabcd"));
  assert (not (acceptor_accepts_string begin_end_concat_acceptor "abcdtest"));
  assert (not (acceptor_accepts_string begin_end_concat_acceptor "abcdtesabcd"))

let () = print_endline "Acceptor tests passed!"


let start_nfa_state = 0

let (test_nfa, next_state) = string_nfa "test" start_nfa_state

let (test_star_nfa, next_state_1) = nfa_star test_nfa next_state

let test_star_nfa_final = finalize_nfa test_star_nfa

let () =
  assert (nfa_accepts_str test_star_nfa_final "");
  assert (nfa_accepts_str test_star_nfa_final "test");
  assert (nfa_accepts_str test_star_nfa_final "testtesttesttest");
  assert (not (nfa_accepts_str test_star_nfa_final "testtesttes"));
  assert (not (nfa_accepts_str test_star_nfa_final "testtesttert"))


let (abcd_nfa, next_state_2) = string_nfa "abcd" next_state_1
let (abcd_concat_test_star, next_state_3) = nfa_concat abcd_nfa test_star_nfa next_state_2

let abcd_concat_test_star_final = finalize_nfa abcd_concat_test_star

let () = 
  assert (nfa_accepts_str abcd_concat_test_star_final "abcd");
  assert (nfa_accepts_str abcd_concat_test_star_final "abcdtesttest");
  assert (not (nfa_accepts_str abcd_concat_test_star_final "testtest"));
  assert (not (nfa_accepts_str abcd_concat_test_star_final "abctest"));
  assert (not (nfa_accepts_str abcd_concat_test_star_final "abcdtestterttesttest"))

let (xyz_nfa, next_state_4) = string_nfa "xyz" next_state_3

let xyz_star, next_state_5 = nfa_star xyz_nfa next_state_4

let xyz_union_abcd_concat_test, next_state_6 = nfa_union xyz_star abcd_concat_test_star next_state_5

let xyz_union_abcd_concat_test_final = finalize_nfa xyz_union_abcd_concat_test

let () = 
  assert (nfa_accepts_str xyz_union_abcd_concat_test_final "abcd");
  assert (nfa_accepts_str xyz_union_abcd_concat_test_final "abcdtesttest");
  assert (nfa_accepts_str xyz_union_abcd_concat_test_final "xyzxyzxyz");
  assert (nfa_accepts_str xyz_union_abcd_concat_test_final "");
  assert (not (nfa_accepts_str xyz_union_abcd_concat_test_final "testtest"));
  assert (not (nfa_accepts_str xyz_union_abcd_concat_test_final "abctest"));
  assert (not (nfa_accepts_str xyz_union_abcd_concat_test_final "xyztest"))

let () = print_endline "NFA tests passed!"
  
