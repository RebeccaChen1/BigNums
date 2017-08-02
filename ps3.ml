(*
			 CS 51 Problem Set 2
			   Bignums and RSA
			     Spring 2017
*)

(*======================================================================
Section 1: Bignums 

In this problem set, as with the previous one, you may express your
solution to a particular problem in terms of another function from a
previous problem. Furthermore, you may use functions from the List
module where appropriate.
......................................................................*)

type bignum = {neg: bool; coeffs: int list} ;;

(* The base for representing bignums. Your code should work for any
   reasonable value of base, not just the initial value we've set it
   to. *)
let base = 1000 ;;
  
(*......................................................................
Problem 1: Negation
......................................................................*)
  
let negate (b : bignum) : bignum =
  if b.coeffs = [] then b
  else {neg = not b.neg ; coeffs = b.coeffs} ;;

(*......................................................................
Problem 2: Comparing bignums
......................................................................*)  
  
let equal (b1 : bignum) (b2 : bignum) : bool =
  b1 = b2 ;;

let less (b1 : bignum) (b2 : bignum) : bool =
  let rec check (x: int list) (y: int list) : bool =
    if List.length x < List.length y then true 
    else if List.length x > List.length y  then false 
    else
    match x, y with
    | hx::tx, hy::ty -> if hx > hy then false
                        else if hx = hy then check tx ty
                        else true
    | _ -> false in 
    (* if b1 is positive and b2 is negative *)
    if not b1.neg && b2.neg then false
    (* if b1 is negative and b2 is positive *)
    else if b1.neg && not b2.neg then true
    (* if both nums are negative  the greater absolute value is smaller *)
    else if (b1.neg && b2.neg) then check b2.coeffs b1.coeffs
    (* if both nums are positive  the greater num is greater *)
    else check b1.coeffs b2.coeffs ;;

let greater (b1 : bignum) (b2 : bignum) : bool =
  not ((equal b1 b2) || (less b1 b2)) ;;

(*......................................................................
Problem 3: Converting to and from bignums
......................................................................*)

let fromInt (n : int) : bignum =
  let neg = n < 0 in 
  let rec fromInt_inner (x: int) (a: int list) : int list =
    if x <> 0 then 
      fromInt_inner (x / base) (abs(x mod base) :: a)
    else a in
  {neg = neg; coeffs = fromInt_inner n []} ;;



let toInt (b : bignum) : int option =
  (* multiplies head with the base, n being the index of the head *)
  let toInt_inner : int list -> int = 
    List.fold_left (fun a x -> x + a * base) 0 in
  (* checks if num is greater than the max or less than the min *)
  if greater b (fromInt max_int) || less b (fromInt min_int) then None
  (* makes final number neg or pos *) 
  else if b.neg then Some (-1 * toInt_inner b.coeffs) 
  else Some (toInt_inner b.coeffs) ;;

(*......................................................................
Helpful functions (not to be used in problems 1 to 3)
......................................................................*)

(* stripzeroes -- Removes zero coefficients from the beginning of the
   coefficients part of a bignum representation *)
let rec stripzeroes (b : int list) : int list =
  match b with
  | 0 :: t -> stripzeroes t
  | _ -> b ;;

(* clean -- Removes zero coefficients from the beginning of a bignum
   representation *)
let clean (b : bignum) : bignum =
  {neg = b.neg; coeffs = stripzeroes b.coeffs} ;;

(* randbignum -- Returns a random bignum from 0 to bound (inclusive).
   You might use this to help randomly test functions. *)
let randbignum (bound: bignum) =
  let randbase = List.map (fun _ -> Random.int base) in
  let rec randbignum_rec (bound: int list) =
    match bound with
    | [] -> []
    | h::t -> let r = Random.int (h+1) in
              r::((if r = h then randbignum_rec else randbase) t)
  in {neg = false; coeffs = stripzeroes (randbignum_rec bound.coeffs)} ;;
       
(* explode -- Splits a string into a list of its characters. *)
let rec explode (s : string) : char list =
  let len = String.length s in
  if len = 0 then []
  else s.[0] :: explode (String.sub s 1 (len - 1)) ;;

(* implode -- Condenses a list of characters into a string. *)
let rec implode (cs : char list) : string =
  match cs with
  | [] -> ""
  | c :: t -> String.make 1 c ^ implode t ;;
					  
(* take_first -- Returns the first n elements of list l (or the whole
   list if too short) *)
let rec take_first (l : 'a list) (n : int) : 'a list =
  match l with
  | [] -> []
  | h :: t -> if n <= 0 then [] else h :: take_first t (n - 1) ;;

(* split -- Returns a pair (first n elements of lst, rest of elements
   of lst) *)
let rec split lst n =
  if n = 0 then ([], lst)
  else match lst with
  | [] -> ([], [])
  | h :: t -> let (lst1, lst2) = split t (n - 1) in
              (h :: lst1, lst2) ;;

(* intlog -- Returns the floor of the base 10 log of an integer *)
let intlog (base : int) : int =
  int_of_float (log10 (float_of_int base)) ;;

(* fromString -- Converts a string representing an integer to a
   bignum. Assumes the base is a power of 10. *)
let fromString (s : string) : bignum =
  let rec fromString_rec (cs : char list) : int list =
    if cs = [] then [] else
    let (chars_to_convert, rest) = split cs (intlog base) in
    let string_to_convert = implode (List.rev chars_to_convert) in
    int_of_string string_to_convert :: fromString_rec rest
  in
  match explode s with
  | [] -> fromInt 0
  | h :: t ->
      if h = '-' || h = '~' then
        {neg = true; coeffs = (List.rev (fromString_rec (List.rev t)))}
      else {neg = false;
            coeffs = (stripzeroes (List.rev (fromString_rec (List.rev (h :: t)))))}

(* toString -- Converts a bignum to its string representation.
   Returns a string beginning with ~ for negative integers. Assumes
   the base is a power of 10. *)
let toString (b : bignum) : string =
  let rec pad_with_zeroes_left (s : string) (len : int) =
    if String.length s >= len then s
    else "0" ^ pad_with_zeroes_left s (len - 1) in
  let rec stripstrzeroes (s : string) (c : char) =
    if String.length s = 0 then
      "0"
    else if String.get s 0 = '0' then
      stripstrzeroes (String.sub s 1 (String.length s - 1)) c
    else s in
  let rec coeffs_to_string (coeffs : int list) : string =
    match coeffs with
    | [] -> ""
    | h :: t -> pad_with_zeroes_left (string_of_int h) (intlog base)
                ^ coeffs_to_string t in
  let stripped = stripzeroes b.coeffs in
  if List.length stripped = 0 then "0"
  else let from_coeffs = stripstrzeroes (coeffs_to_string stripped) '0' in
       if b.neg then "~" ^ from_coeffs else from_coeffs ;;

(*......................................................................
Arithmetic functions
......................................................................*)

(* plus_pos -- Returns a bignum representing b1 + b2.  Assumes that b1
   + b2 > 0. *)
let plus_pos (b1 : bignum) (b2 : bignum) : bignum =
  let pair_from_carry (carry : int) =
    if carry = 0 then (false, [])
    else if carry = 1 then (false, [1])
    else (true, [1])
  in
  let rec plus_with_carry (neg1, coeffs1) (neg2, coeffs2) (carry : int)
    : bool * int list =
    match (coeffs1, coeffs2) with
    | ([], []) -> pair_from_carry carry
    | ([], _) ->
        if carry = 0 then (neg2, coeffs2)
        else plus_with_carry (neg2, coeffs2) (pair_from_carry carry) 0
    | (_, []) ->
        if carry = 0 then (neg1, coeffs1)
        else plus_with_carry (neg1, coeffs1) (pair_from_carry carry) 0
    | (h1 :: t1, h2 :: t2) ->
        let (sign1, sign2) =
            ((if neg1 then -1 else 1), (if neg2 then -1 else 1)) in
        let result = h1 * sign1 + h2 * sign2 + carry in
        if result < 0 then
          let (negres, coeffsres) =
              plus_with_carry (neg1, t1) (neg2, t2) (-1)
          in (negres, result + base :: coeffsres)
        else if result >= base then
          let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) 1
          in (negres, result - base :: coeffsres)
        else
          let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) 0
          in (negres, result :: coeffsres)
  in
  let (negres, coeffsres) =
      plus_with_carry (b1.neg, List.rev b1.coeffs)
                      (b2.neg, List.rev b2.coeffs)
                      0
  in {neg = negres; coeffs = stripzeroes (List.rev coeffsres)} ;;

(*......................................................................
Problem 4

The "plus" function returns a bignum representing b1 + b2. However,
it does NOT make the same assumption as plus_pos. 

Hint: How can you use plus_pos to implement plus? Make sure that
your implementation preserves the bignum invariant.
......................................................................
*)

let plus (b1 : bignum) (b2 : bignum) : bignum =
  (* checks abs value of nums then adds the positive 
     greater value to the negative lesser value *)
  let plus_neg b1 b2 = 
    if greater b1 (negate b2) then plus_pos b1 b2
    else negate (plus_pos (negate b1) (negate b2)) in 
  match b1.neg, b2.neg with
  | true, true -> plus_neg b1 b2 
  | true, false -> plus_neg b2 b1
  | false, true -> plus_neg b1 b2
  | false, false -> plus_pos b1 b2  ;;


(*......................................................................
Problem 5

The function "times" returns a bignum representing b1 * b2. When
approaching this problem, consider how you were first taught
multiplication:

      543 
    x 242
    -----
     1086
  + 21720 <--- Note that a zero is appended after the partial product
 + 108600 <--- Note that two zeroes are appended after the partial product
 --------
= 131,406  

When approaching this problem, it is advisable to break the problem
down into simpler, easier-to-implement sub-problems. That way, if a
bug arises in your code, you can test each helper function
individually rather than having to test all of it at once.

You may assume positivity in some of your helper functions if it 
simplifies the code, as long as the invariant is preserved. 
......................................................................*)

let times ({neg = neg1; coeffs = num1} : bignum) 
          ({neg = neg2; coeffs = num2} : bignum) 
        : bignum =
  (* multiplies first list by the head of second list *)
  let rec list_times_int  (lst: int list) 
                          (x: int) 
                          (c: int) 
                          (z: int) 
                        : int list = 
    (* adds zeros to front of list depending on the place *)
    if z > 0 then 0 :: list_times_int lst x c (z - 1)
    else
    match lst with 
    | h::t -> let ans = (h * x) + c in 
              ((ans mod base) :: list_times_int t x (ans / base) 0)
    (* carries over residual that is greater than base *)
    | [] -> if c > 0 then [c] else [] in 
  (* expects two reversed lists; adds each multiplied list together *)
  let rec times_inner (a: int list) (mult: int list) (z: int) : bignum =
    match mult with 
    | h::t -> plus ({neg = false; coeffs = List.rev (list_times_int a h 0 z)})
                   (times_inner a t (z + 1))  
    | [] -> {neg = false; coeffs = []} in 
  (* positive if signs are equal, negative if they are different *)
  if neg1 = neg2 then times_inner (List.rev num1) (List.rev num2) 0
  else negate (times_inner (List.rev num1) (List.rev num2) 0)  ;;

(*======================================================================
Section 2: Challenge Problem - RSA Cryptosystem

A reminder: these problems are not required and are only for 
your karmic edification. You should feel free to do these after 
you've done your best work on your primary part of the problem set.

Found below is support code for RSA. Hint: each part of this 
problem can be implemented in approximately one line of code. 
......................................................................*)

(* divsing -- Divides a bignum by a single digit, that is, returns a
   bignum representing b/n, where n is an integer less than base *)
let divsing (b : int list) (n : int) : int list * int =
  let rec divsing_rec (b : int list) (r : int) : int list * int =
    match b with
    | [] -> [], r
    | h :: t ->
        let dividend = r * base + h in
        let quot = dividend / n in
        let (q, r) = divsing_rec t (dividend-quot * n) in
        (quot :: q, r)
  in
  match b with
  | [] -> [], 0
  | [a] -> [a / n], a mod n
  | h1 :: h2 :: t -> if h1 < n then divsing_rec (h1 * base + h2 ::t) 0
                     else divsing_rec b 0 ;;

(* divmod -- Returns a pair (floor of b1/b2, b1 mod b2), both bignums *)
let divmod (b1 : bignum) (b2 : bignum): bignum * bignum =
  let rec divmod_rec m n (psum : bignum) : bignum * bignum =
    if less m n then (psum, m)
    else
      let mc = m.coeffs in
      let nc = n.coeffs in
      match nc with
      | [] -> failwith "Division by zero"
      | ns :: _ -> let (p, _) =
          if ns + 1 = base then
            (take_first mc (List.length mc - List.length nc), 0)
          else
            let den = ns + 1 in
            let num = take_first mc (List.length mc - List.length nc + 1) in
            divsing num den
        in
        let bp = clean {neg = false; coeffs = p} in
        let p2 = clean (if equal bp (fromInt 0) then fromInt 1 else bp) in
        divmod_rec (clean (plus m (negate (times n p2))))
                   (clean n)
                   (clean (plus psum p2))
  in
  divmod_rec (clean b1) (clean b2) (fromInt 0) ;;

(* expmod -- Returns b to the power of e mod m *)
let rec expmod (b : bignum) (e : bignum) (m : bignum) : bignum =
  if equal e (fromInt 0) then fromInt 1
  else if equal e (fromInt 1) then
    snd (divmod (clean b) (clean m))
  else
    let (q, r) = divmod (clean e) (fromInt 2) in
    let res = expmod (clean b) q (clean m) in
    let (_, x) = divmod (times (times res res) (expmod (clean b) r (clean m)))
                        (clean m) in
    {neg = x.neg; coeffs = stripzeroes x.coeffs} ;;

(* exponent -- Returns b to the power of e *)
let rec exponent (b : bignum) (e : bignum) : bignum =
  if equal (clean e) (fromInt 0) then fromInt 1
  else if equal (clean e) (fromInt 1) then clean b
  else
    let (q, r) = divmod (clean e) (fromInt 2) in
    let res = exponent (clean b) q in
    let exp = (times (times res res) (exponent (clean b) r))
    in {neg = exp.neg; coeffs = stripzeroes exp.coeffs} ;;

(* isPrime -- Returns true if n is prime, false otherwise. *)
let isPrime (n : bignum) : bool =
  let rec miller_rabin (k : int) (d : bignum) (s : int) : bool =
    if k < 0 then true else
    let rec square (r : int) (x : bignum) =
      if r >= s then false else
      let x = expmod x (fromInt 2) n in

        if equal x (fromInt 1) then false
        else if equal x (plus n (fromInt (-1))) then miller_rabin (k-1) d s
        else square (r + 1) x
    in
    let a = plus (randbignum (plus n (fromInt (-4)))) (fromInt 2) in
    let x = expmod a d n in
      if equal x (fromInt 1) || equal x (plus n (fromInt (-1))) then
        miller_rabin (k - 1) d s
      else square 1 x 
  in
    (* Factor powers of 2 to return (d, s) such that n=(2^s)*d *)
  let rec factor (n : bignum) (s : int) =
    let (q, r) = divmod n (fromInt 2) in
      if equal r (fromInt 0) then factor q (s + 1) else (n, s)
  in
  let (_, r) = divmod n (fromInt 2) in
    if equal r (fromInt 0) then false else
      let (d, s) = factor (plus n (fromInt (-1))) 0 in
        miller_rabin 20 d s ;;

(* euclid -- Returns (s, t, g) such that g is gcd(m, d) and s*m + t*d = g *)
let rec euclid (m : bignum) (d : bignum) : bignum * bignum * bignum =
  if equal d (fromInt 0) then (fromInt 1, fromInt 0, m)
  else
    let (q, r) = divmod m d in
    let (s, t, g) = euclid d r in
      (clean t, clean (plus s (negate (times q t))), clean g) ;;

(* generateRandomPrime -- Generate a random prime number between min
   and max-1 (inclusive) *)
let rec generateRandomPrime (min : bignum) (max: bignum) : bignum =
  let rand = plus (randbignum (plus max (negate min))) min in
    if isPrime rand then rand else generateRandomPrime min max ;;


(*......................................................................
Code for encrypting and decrypting messages using RSA 
*)

(* generateKeyPair -- Generate a random RSA key pair, returned as (e,
   d, n).  p and q will be between 2^n and 2^(n+1).  Recall that (n,
   e) is the public key, and (n, d) is the private key. *)
  
let rec generateKeyPair (r : bignum) : bignum * bignum * bignum =
  let c1 = fromInt 1 in
  let c2 = fromInt 2 in
  let p = generateRandomPrime (exponent c2 r) (exponent c2 (plus r c1)) in
  let q = generateRandomPrime (exponent c2 r) (exponent c2 (plus r c1)) in
  let m = times (plus p (negate c1)) (plus q (negate c1)) in
  let rec selectPair () =
    let e = generateRandomPrime (exponent c2 r) (exponent c2 (plus r c1)) in
    let (_, d, g) = euclid m e in
    let d = if d.neg then plus d m else d in
      if equal g c1 then (clean e, clean d, clean (times p q))
      else selectPair ()
  in
    if equal p q then generateKeyPair r else selectPair () ;;

(*......................................................................
Challenge Problem 6: Encrypting and decrypting bignums
......................................................................*)

(* encryptDecryptBignum -- Encrypt or decrypt a bignum. To encrypt,
   pass in the arguments n e s. To decrypt, pass in the inputs n d
   s. *)
let encryptDecryptBignum (n : bignum) (e : bignum) (s : bignum) : bignum =
  failwith "encryptDecryptBignum not implemented" ;;

(* charsToBignums -- Pack a list of chars as a list of bignums, with m
   chars to a bignum. *)
let rec charsToBignums (lst : char list) (m : int) : bignum list =
  let rec encchars lst =
    match lst with
    | [] -> (fromInt 0)
    | c :: t -> clean (plus (times (encchars t) (fromInt 256))
                            (fromInt (int_of_char c)))
  in
    match lst with
    | [] -> []
    | _ -> let (enclist, rest) = split lst m in
           encchars enclist :: charsToBignums rest m

(* bignumsToChars -- Unpack a list of bignums into chars (reverse of
   charsToBignums) *)
let rec bignumsToChars (lst : bignum list) : char list =
  let rec decbignum b =
    if equal b (fromInt 0) then []
    else let (q, r) = divmod b (fromInt 256) in
      match toInt r with
      | None -> failwith "bignumsToChars: representation invariant broken"
      | Some ir -> char_of_int ir :: decbignum q
  in
    match lst with
    | [] -> []
    | b :: t -> decbignum b @ bignumsToChars t

(* bytesInKey -- Return the number of bytes required to represent an
   RSA modulus. *)
let bytesInKey (n : bignum) =
  int_of_float (float_of_int (List.length (stripzeroes n.coeffs) - 1)
                *. log10 (float_of_int base) /. (log10 2. *. 8.))

(* Encrypts or decrypts a list of bignums using RSA. To encrypt, pass
   in n e lst. To decrypt, pass in n d lst. *)
let rec encDecBignumList (n : bignum) (e : bignum) (lst : bignum list) =
  match lst with
  | [] -> []
  | h :: t -> encryptDecryptBignum n e h :: encDecBignumList n e t

(*......................................................................
Challenge Problem 7: Encrypting and decrypting strings
......................................................................*)					

let encrypt (n : bignum) (e : bignum) (s : string) =
  failwith "encrypt not implemented" ;;

(* 
Decrypt an encrypted message (list of bignums) to produce the
original string. 
*)
let decrypt (n : bignum) (d : bignum) (m : bignum list) =
  failwith "decrypt not implemented" ;;

(*======================================================================
Section 3: challenge Problem - Faster Multiplication 
......................................................................*)

(*......................................................................
Challenge Problem 8: Faster bignum multiplication

The function "times_faster" returns a bignum representing b1 * b2. 
......................................................................
*)

let times_faster (b1 : bignum) (b2 : bignum) : bignum =
  failwith "times_faster not implemented" ;;

(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete.  We care
about your responses and will use them to help guide us in creating
future assignments.
......................................................................*)

let minutes_spent_on_pset () : int = 720 ;;
