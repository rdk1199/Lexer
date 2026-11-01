open Lexer.Acceptor
open Lexer.NFA
open Lexer.DFA
open Lexer.Regex
open Lexer.Tokenizer

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

let expect_dfa_accept_all dfa regex_str string_list =
  List.iter (fun str ->
    if not (dfa_accepts_str dfa str) then (
      print_endline (regex_str ^ " DFA does not accept " ^ str ^ " even though it should!");
      assert(false)
    )
    else
      ()
  ) string_list

let expect_dfa_reject_all dfa regex_str string_list =
  List.iter (fun str ->
    if dfa_accepts_str dfa str then (
      print_endline (regex_str ^ " DFA accepts " ^ str ^ " even though it should not!");
      assert(false)
    )
    else
      ()
  ) string_list

let test_regex = "[abr-t]"
let test_nfa = parse_regex_string test_regex
let test_dfa = construct_dfa test_nfa

let () = expect_dfa_accept_all test_dfa test_regex [
  "a";
  "b";
  "r";
  "s";
  "t";
]

let () = expect_dfa_reject_all test_dfa test_regex [
  "f";
  "k";
]

let () = print_endline "test 1 passed!"

let test_regex_2 = "[]a-ce-g-]"
let test_nfa_2 = parse_regex_string test_regex_2
let test_dfa_2 = construct_dfa test_nfa_2

let () = expect_dfa_accept_all test_dfa_2 test_regex_2 [
  "a";
  "b";
  "c";
  "e";
  "f";
  "g";
  "]";
  "-";
] 

let () = expect_dfa_reject_all test_dfa_2 test_regex_2 [
  "d";
  "h";
]

let () = print_endline "test 2 passed!"

let test_nfa_3 = parse_regex_string "a*b"
let test_dfa_3 = construct_dfa test_nfa_3

let () = expect_dfa_accept_all test_dfa_3 "a*b" [
  "b";
  "ab";
  "aaaaaab";
  "aaaaaaaaaaaaab";
]

let () = expect_dfa_reject_all test_dfa_3 "a*b" [
  "";
  "aba";
  "abb";
  "c";
  "ac";
  "cb"
]

let () = print_endline "test 3 passed!"

let test_regex_4 = "dog|cat"
let test_nfa_4 = parse_regex_string test_regex_4
let test_dfa_4 = construct_dfa test_nfa_4

let () = expect_dfa_accept_all test_dfa_4 test_regex_4 [
  "dog";
  "cat";
]


let () = expect_dfa_reject_all test_dfa_4 test_regex_4 [
  "dog|cat";
  "";
  "dogcat"
]

let () = print_endline "test 4 passed!"

let test_regex_5 = "dog\\|cat"
let test_nfa_5 = parse_regex_string test_regex_5
let test_dfa_5 = construct_dfa test_nfa_5

let () = expect_dfa_accept_all test_dfa_5 test_regex_5 [
  "dog|cat";
]

let () = expect_dfa_reject_all test_dfa_5 test_regex_5 [
  "dog";
  "";
  "cat"
]

let () = print_endline "test 5 passed!"

let test_regex_7 = "a(c|d)|e"
let test_nfa_7 = parse_regex_string test_regex_7
let test_dfa_7 = construct_dfa test_nfa_7

let () = expect_dfa_accept_all test_dfa_7 test_regex_7 [
  "ac";
  "ad";
  "e";
]

let () = expect_dfa_reject_all test_dfa_7 test_regex_7 [
  "ae";
  "a";
  "";
]
 
let () = print_endline "test 6 passed!"

let test_regex_6 = "ab(c|d)([e-g]|(hi.[t-x]*z)*)|56346"
let test_nfa_6 = parse_regex_string test_regex_6
let test_dfa_6 = construct_dfa test_nfa_6

let () = expect_dfa_accept_all test_dfa_6 test_regex_6 [
  "56346";
  "abdf";
  "abc";
  "abchiqz";
  "abchiptuvwxtwxz";
  "abdhiptuvwxtwxzhiptwxtuvz";
]

let () = expect_dfa_reject_all test_dfa_6 test_regex_6 [
  "5634";
  "";
  "adf";
  "abcl";
  "abchiz";
  "abchiqyz";
  "abdghiptuvwxtwxzhiptwxtuvz";
]

let () = print_endline "test 7 passed!"

let test_regex_8 = "((((c))))"
let test_nfa_8 = parse_regex_string test_regex_8
let test_dfa_8 = construct_dfa test_nfa_8

let () = expect_dfa_accept_all test_dfa_8 test_regex_8 [
  "c";
]

let () = expect_dfa_reject_all test_dfa_8 test_regex_8 [
  ")";
  "";
  "(";
]

let () = print_endline "test 8 passed!"

let test_regex_9 = "(\\\\|\\n|\\r|\\t| )*"
let test_nfa_9 = parse_regex_string test_regex_9
let test_dfa_9 = construct_dfa test_nfa_9

let () = expect_dfa_accept_all test_dfa_9 test_regex_9 [
  " ";
  "\\";
  "\n";
  "\r";
  "\t";
  "\n\t\r\\ ";
  "";
]

let () = expect_dfa_reject_all test_dfa_9 test_regex_9 [
  "a";
  "|";
  ")";
  "(";
]

let () = print_endline "test 9 passed!"


let test_regex_10 = "(a?b?c?d?\\?)*"
let test_dfa_10 = construct_dfa (parse_regex_string test_regex_10)

let () = expect_dfa_accept_all test_dfa_10 test_regex_10 [
  "";
  "?";
  "ac?";
  "abcd?";
  "?abcd???";
  "?a?b?c?d?";
  "????";
]

let () = expect_dfa_reject_all test_dfa_10 test_regex_10 [
  "a";
  "aabcd?";
  "a?b";
]

let () = print_endline "test 10 passed!"


let test_regex_11 = "(p([abd]*[efg]*)*)?"
let test_dfa_11 = construct_dfa (parse_regex_string test_regex_11)

let () = expect_dfa_accept_all test_dfa_11 test_regex_11 [
  "";
  "p";
  "pefgabd";
  "paaaaaabbbdddeeeefffgggabdefg";
  "pabdabdabd"
]

let () = expect_dfa_reject_all test_dfa_11 test_regex_11 [
  "a";
  "p?";
  "ppabdefgabd";
]


let () = print_endline "test 11 passed!"

let () = print_endline "Regex->NFA tests passed!"



let ident_regex = "([a-zA-Z_])([a-zA-Z0-9_])*"
let int_regex = "-?[0-9][0-9]*"
let whitespace_regex = "(|\\n|\\r|\\t| )*"

let tokenizer = build_tokenizer [("IF", "if"); ("THEN", "then"); ("ELSE", "else");] [("IDENT", ident_regex); ("INTEGER", int_regex); ("WHITESPACE", whitespace_regex);]


let test_string_1 = " if -3 is greater than        5 then print 3 else print    asdf 5 "

let token_list_1 = tokenize_string tokenizer test_string_1
let () = print_endline (token_list_to_string token_list_1)

let () = print_endline "Tokenizer test passed!"
