open Lexer.Acceptor
open Lexer.NFA
open Lexer.Regex

  
let expect_accept_all nfa regex_str string_list =
  List.iter (fun str ->
    if not (nfa_accepts_str nfa str) then (
      print_endline (regex_str ^ " does not accept " ^ str ^ " even though it should!");
      assert(false)
    )
    else
      ()
  ) string_list

let expect_reject_all nfa regex_str string_list =
  List.iter (fun str ->
    if nfa_accepts_str nfa str then (
      print_endline (regex_str ^ " accepts " ^ str ^ " even though it should not!");
      assert(false)
    )
    else
      ()
  ) string_list


let test_regex = "[abr-t]"
let test_nfa = parse_regex_string test_regex

let () = expect_accept_all test_nfa test_regex [
  "a";
  "b";
  "r";
  "s";
  "t";
]

let () = expect_reject_all test_nfa test_regex [
  "f";
  "k";
]

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

let test_nfa_3 = parse_regex_string "a*b"

let () = expect_accept_all test_nfa_3 "a*b" [
  "b";
  "ab";
  "aaaaaab";
  "aaaaaaaaaaaaab";
]

let () = expect_reject_all test_nfa_3 "a*b" [
  "";
  "aba";
  "abb";
  "c";
  "ac";
  "cb"
]

let test_regex_4 = "dog|cat"
let test_nfa_4 = parse_regex_string test_regex_4

let () = expect_accept_all test_nfa_4 test_regex_4 [
  "dog";
  "cat";
]


let () = expect_reject_all test_nfa_4 test_regex_4 [
  "dog|cat";
  "";
  "dogcat"
]

let test_regex_5 = "dog\\|cat"
let test_nfa_5 = parse_regex_string test_regex_5

let () = expect_accept_all test_nfa_5 test_regex_5 [
  "dog|cat";
]

let () = expect_reject_all test_nfa_5 test_regex_5 [
  "dog";
  "";
  "cat"
]

let test_regex_7 = "a(c|d)|e"
let test_nfa_7 = parse_regex_string test_regex_7

let () = expect_accept_all test_nfa_7 test_regex_7 [
  "ac";
  "ad";
  "e";
]

let () = expect_reject_all test_nfa_7 test_regex_7 [
  "ae";
  "a";
  "";
]
 

let test_regex_6 = "ab(c|d)([e-g]|(hi.[t-x]*z)*)|56346"
let test_nfa_6 = parse_regex_string test_regex_6

let () = expect_accept_all test_nfa_6 test_regex_6 [
  "56346";
  "abdf";
  "abc";
  "abchiqz";
  "abchiptuvwxtwxz";
  "abdhiptuvwxtwxzhiptwxtuvz";
]

let () = expect_reject_all test_nfa_6 test_regex_6 [
  "5634";
  "";
  "adf";
  "abcl";
  "abchiz";
  "abchiqyz";
]

let test_regex_8 = "((((c))))"
let test_nfa_8 = parse_regex_string test_regex_8

let () = expect_accept_all test_nfa_8 test_regex_8 [
  "c";
]

let () = expect_reject_all test_nfa_8 test_regex_8 [
  ")";
  "";
  "(";
]

let () = print_endline "Regex->NFA tests passed!"