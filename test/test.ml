open Example.Lib
open Alcotest

let tail_of_a_list () =
  check (option string) "returns the last element of a list" (Some "d")
    (last [ "a"; "b"; "c"; "d" ])

let tail_of_a_list_2 () =
  check (option string) "returns None for empty list" None (last [])

let last_two =
  let check = check (option (pair string string)) in
  let case_0 () =
    check "returns last two elemnts of the list"
      (Some ("c", "d"))
      (last_two [ "a"; "b"; "c"; "d" ])
  in
  let case_1 () = check "" None (last_two [ "a" ]) in
  [
    test_case "returns last two elements of the list" `Quick case_0;
    test_case "returns None for the list with single element" `Quick case_1;
  ]

let nth =
  let check = check (option string) in
  let case_0 () = check "" (Some "c") (nth [ "a"; "b"; "c"; "d"; "e" ] 2) in
  let case_1 () = check "" None (nth [ "a" ] 2) in
  [
    test_case "returns 2nd element of the list" `Quick case_0;
    test_case "returns None when out of index" `Quick case_1;
  ]

let length =
  let check = check int in
  let case_0 () = check "" 3 (length [ "a"; "b"; "c" ]) in
  let case_1 () = check "" 0 (length []) in
  [
    test_case "returns 3 for the list which has 3 elements" `Quick case_0;
    test_case "returns 0 for the list which has no elements" `Quick case_1;
  ]

let reverse =
  let check = check (list string) in
  let case () = check "" [ "c"; "b"; "a" ] (reverse [ "a"; "b"; "c" ]) in
  [ test_case "returns reversed list" `Quick case ]

let palindrome =
  let check = check bool in
  let case_0 () =
    check "booa" true (is_palindrome [ "x"; "a"; "m"; "a"; "x" ])
  in
  let case_1 () = check "" false (is_palindrome [ "a"; "b" ]) in
  [
    test_case "returns true for palindrome list" `Quick case_0;
    test_case "returns false for non-palindrome list" `Quick case_1;
  ]

let () =
  Alcotest.run "Example.Lib"
    [
      ( "last",
        [
          test_case "returns the last element of a list" `Quick tail_of_a_list;
          test_case "returns None for empty list" `Quick tail_of_a_list_2;
        ] );
      ("last_two", last_two);
      ("nth", nth);
      ("length", length);
      ("reverse", reverse);
      ("palindrome", palindrome);
    ]
