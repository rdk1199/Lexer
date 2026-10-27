open Lexer.Acceptor

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
