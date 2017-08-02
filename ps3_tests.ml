
open Ps3 ;;
  
(* Sample negate tests: more exhaustive testing for all other functions
 * is required. (An example of such testing is test_equals below).
 * We only allow the positive representation of 0 *)

let test () =
  assert(negate {neg = false; coeffs = []} = {neg = false; coeffs = []});
  assert(negate {neg = true; coeffs = [1; 2]} = {neg = false; coeffs = [1; 2]});

  assert(equal {neg = true; coeffs = [1; 2]} {neg = true; coeffs = [1; 2]} = true);
  assert(equal {neg = false; coeffs = [1; 2]} {neg = true; coeffs = [1; 2]} = false);
  assert(equal {neg = true; coeffs = [1; 2]} {neg = true; coeffs = [1; 2]} = true);
  assert(equal {neg = false; coeffs = [1; 2]} {neg = false; coeffs = [12; 2]} = false);
  assert(equal {neg = true; coeffs = [654;321]} {neg = true; coeffs = [654;321]} = true);

  assert(less {neg = false; coeffs = [1; 2]} {neg = true; coeffs = [1; 2]} = false);
  assert(less {neg = true; coeffs = [1; 2]} {neg = false; coeffs = [1; 2]} = true);
  assert(less {neg = false; coeffs = [1; 2]} {neg = false; coeffs = [1]} = false);
  assert(less {neg = false; coeffs = [1]} {neg = false; coeffs = [1; 2]} = true);
  assert(less {neg = true; coeffs = [1; 2]} {neg = true; coeffs = [1]} = true);
  assert(less {neg = true; coeffs = [1]} {neg = true; coeffs = [1; 2]} = false);
  assert(less {neg = true; coeffs = [1; 3]} {neg = true; coeffs = [1; 2]} = true);
  assert(less {neg = true; coeffs = [1; 2]} {neg = true; coeffs = [1; 3]} = false);
  assert(less {neg = false; coeffs = [1; 3]} {neg = false; coeffs = [1; 2]} = false);
  assert(less {neg = false; coeffs = [1; 2]} {neg = false; coeffs = [1; 3]} = true);
  assert(less {neg = true; coeffs = [1; 2]} {neg = true; coeffs = [1; 2]} = false);

  assert(greater {neg = false; coeffs = [1; 2]} {neg = true; coeffs = [1; 2]} = true);
  assert(greater {neg = true; coeffs = [1; 2]} {neg = false; coeffs = [1; 2]} = false);
  assert(greater {neg = false; coeffs = [1; 2]} {neg = false; coeffs = [1]} = true);
  assert(greater {neg = false; coeffs = [1]} {neg = false; coeffs = [1; 2]} = false);
  assert(greater {neg = true; coeffs = [1; 2]} {neg = true; coeffs = [1]} = false);
  assert(greater {neg = true; coeffs = [1]} {neg = true; coeffs = [1; 2]} = true);
  assert(greater {neg = true; coeffs = [1; 3]} {neg = true; coeffs = [1; 2]} = false);
  assert(greater {neg = true; coeffs = [1; 2]} {neg = true; coeffs = [1; 3]} = true);
  assert(greater {neg = false; coeffs = [1; 3]} {neg = false; coeffs = [1; 2]} = true);
  assert(greater {neg = false; coeffs = [1; 2]} {neg = false; coeffs = [1; 3]} = false);
  assert(greater {neg = true; coeffs = [1; 2]} {neg = true; coeffs = [1; 2]} = false);

  assert(fromInt 123456 = {neg = false; coeffs = [123; 456]});
  assert(fromInt (-123456) = {neg = true; coeffs = [123; 456]});
  assert(fromInt 123 = {neg = false; coeffs = [123]});
  assert(fromInt 0 = {neg = false; coeffs = []});

  assert(toInt {neg = false; coeffs = [4; 611; 686; 018; 427; 387; 904]} = None);
  assert(toInt {neg = true; coeffs = [4; 611; 686; 018; 427; 387; 905]} = None);
  assert(toInt {neg = false; coeffs = [123; 456]} = Some 123456);
  assert(toInt {neg = true; coeffs = [123; 456]} = Some (-123456));
  assert(toInt {neg = false; coeffs = []} = Some 0);
 
  assert(plus {neg = false; coeffs = [1]} {neg = false; coeffs = [2]} = {neg = false; coeffs = [3]});
  assert(plus {neg = true; coeffs = [1]} {neg = false; coeffs = [2]} = {neg = false; coeffs = [1]});
  assert(plus {neg = false; coeffs = [1]} {neg = true; coeffs = [2]} = {neg = true; coeffs = [1]});
  assert(plus {neg = true; coeffs = [1]} {neg = true; coeffs = [2]} = {neg = true; coeffs = [3]});
  assert(plus {neg = false; coeffs = [1]} {neg = true; coeffs = [1]} = {neg = false; coeffs = []});

  assert(times {neg = false; coeffs = [1; 2]} {neg = false; coeffs = [1; 2]} = {neg = false; coeffs = [1; 4; 4]});
  assert(times {neg = true; coeffs = [1; 2]} {neg = false; coeffs = [1; 2]} = {neg = true; coeffs = [1; 4; 4]});
  assert(times {neg = true; coeffs = [1; 2]} {neg = true; coeffs = [1; 2]} = {neg = false; coeffs = [1; 4; 4]});
  assert(times {neg = false; coeffs = [11; 22]} {neg = false; coeffs = [11; 22]} = {neg = false; coeffs = [121; 484; 484]});
  assert(times {neg = false; coeffs = []} {neg = true; coeffs = [123; 456; 789]} = {neg = false; coeffs = []});
  assert(times {neg = false; coeffs = [12; 0; 0; 0; 0]} {neg = true; coeffs = [1]} = {neg = true; coeffs = [12; 0; 0; 0; 0]})

;;
	         
(* Some advice on automated test ing:

   Here is an automated testing function that checks every pair of
   integers between count and max to verify that the bignum
   representations of count and max are the same if and only if count
   and max are.

   Use this function to help you catch potential edge cases. While
   this kind of automated testing is helpful, it is still important
   for you to think about what cases may be difficult for your
   algorithm. Also, think about what inputs to run the testing
   function on. If you're having trouble isolating a bug, you can try
   printing out which values cause an assert failure.

   You may find that adding this sort of testing function for other
   functions is useful.  *)
	      
let rec test_equal (count : int) (max : int) : unit =
  if count > max then ()
  else
    let _ = assert(equal (fromInt count) (fromInt max) = (count = max)) in
    test_equal (count + 1) max ;;

(* Examples of using the automated testing function *)

let () = test_equal (-10000) 10000 ;;
let () = test_equal 10000 (-10000) ;;
let () = test_equal (-10000) 9999 ;;


test () ;;

print_endline "All tests passed.";;
