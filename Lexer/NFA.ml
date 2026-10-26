
(*NFA = Non-deterministic finite automata - for string matching*)
(*
  These NFA functions are largely just implemented as a curiosity - in practice
  they would be quite slow, at least for more nontrivial regexes, as they require a lot of
  recursive backtracking.
  Furthermore, because we use functions as opposed to the usual automata representation,
  we cannot apply the usual optimization algorithms as there is no underlying state machine!
  (Technically these aren't NFAs at all, just functions that mimic them)
*)

type nfa = {
  (** The acceptor is a function that takes a string, indices i <= j, and returns true
    iff the substring string[i...j] is accepted by this NFA
    Rules for acceptor : 
    If string is empty, i , j are ignored
    If j < i, then we treat like the empty substring
  *)
  acceptor : string -> int -> int -> bool
}

(**Union - accepts any string which would be accepted by either nfa1 or nfa2*)
let nfa_union nfa1 nfa2 =
  {
    acceptor = (fun str i  j -> 
      nfa1.acceptor str i j ||
      nfa2.acceptor str i j  
    );
  }

(**Concatenation - accepts any string of the form AB where nfa1 accepts A and nfa2 accepts B*)
let nfa_concatenate nfa1 nfa2 =
  {
    acceptor = (fun str i j ->
      (**for k = i-1 to j, check if nfa1 accepts str[i...k] and nfa2 accepts str[k + 1...j]*)
      let rec rec_acceptor str k =
        if k > j then
          false
        else if (nfa1.acceptor str i k && nfa2.acceptor str (k+1) j) then
          true
        else
          rec_acceptor str (k+1)
      in
      rec_acceptor str (i-1)
    );
  }

(**Kleene star operation - accepts 0 or more copies of any string accepted by the input nfa*)
let nfa_kleene_star nfa =
  (** The runtime on this could be horrendous!, but sub-exponential?*)
  (** This could be improved through some kind of dynamic programming scheme*)
  let rec kleene_star_acceptor str i j = 
    (**Always accept the empty substring*)
      if (String.length str) = 0 || (j < i) then
        true
      else
        let rec rec_acceptor str k =
          if k > j then
            false
          else if (nfa.acceptor str i k && kleene_star_acceptor str (k + 1) j) then
            true
          else
            rec_acceptor str (k+1)
        in
        (**There's no point in starting at i-1 since the empty string can appear infinitely often*)
        rec_acceptor str i
  in
  {
    acceptor = kleene_star_acceptor   
  }

let nfa_accepts_string nfa str =
  nfa.acceptor str 0 (String.length str - 1)

(**NFA that accepts only the single character c*)
let single_char_nfa c =
  {
    (**Maybe don't need bounds checking*)
    acceptor = (fun str i j ->
      i = j && i >= 0 && String.length str > i && str.[i] = c
    );
  }

(**NFA that only accepts the string target_str*)
let string_nfa target_str = 
  {
    acceptor = (fun str i j ->
      j - i + 1 == String.length target_str &&
      i >= 0 &&
      j < String.length str &&
      String.sub str i (j - i + 1) = target_str 
    );
  }
