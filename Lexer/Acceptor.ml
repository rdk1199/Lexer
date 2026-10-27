
(*
  An acceptor is a function that takes a string and returns a bool - the standard NFA operations of union, concatenation, and Kleene star can be applied to them
  More of a curiosity, really, as these implementations would be horrendously slow in practice
*)

type acceptor =  (string -> int -> int -> bool)


(**Union - accepts any string which would be accepted by either acceptor1 or acceptor2*)
let acceptor_union acceptor1 acceptor2 =
    (fun str i  j -> 
      acceptor1 str i j ||
      acceptor2 str i j  
    )

(**Concatenation - accepts any string of the form AB where acceptor1 accepts A and acceptor2 accepts B*)
let acceptor_concatenate acceptor1 acceptor2 =
  (fun str i j ->
    (**for k = i-1 to j, check if acceptor1 accepts str[i...k] and acceptor2 accepts str[k + 1...j]*)
    let rec rec_acceptor str k =
      if k > j then
        false
      else if (acceptor1 str i k && acceptor2 str (k+1) j) then
        true
      else
        rec_acceptor str (k+1)
    in
    rec_acceptor str (i-1)
  )
  

(**Kleene star operation - accepts 0 or more copies of any string accepted by the input acceptor*)
let acceptor_kleene_star acceptor =
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
          else if (acceptor str i k && kleene_star_acceptor str (k + 1) j) then
            true
          else
            rec_acceptor str (k+1)
        in
        (**There's no point in starting at i-1 since the empty string can appear infinitely often*)
        rec_acceptor str i
  in
  kleene_star_acceptor   

let acceptor_accepts_string acceptor str =
  acceptor str 0 (String.length str - 1)

(**acceptor that accepts only the single character c*)
let single_char_acceptor c =
  (**Maybe don't need bounds checking*)
  (fun str i j ->
    i = j && i >= 0 && String.length str > i && str.[i] = c
  )

(**acceptor that only accepts the string target_str*)
let string_acceptor target_str = 
  (fun str i j ->
      j - i + 1 == String.length target_str &&
      i >= 0 &&
      j < String.length str &&
      String.sub str i (j - i + 1) = target_str 
  )

