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

let () =
  Alcotest.run "Example.Lib"
    [
      ( "last",
        [
          test_case "returns the last element of a list" `Quick tail_of_a_list;
          test_case "returns None for empty list" `Quick tail_of_a_list_2;
        ] );
      ("last_two", last_two);
    ]
