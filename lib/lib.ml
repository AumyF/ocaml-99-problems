let rec last = function [] -> None | [ l ] -> Some l | _ :: t -> last t

let rec last_two list =
  match list with [] -> None | [ a; b ] -> Some (a, b) | _ :: t -> last_two t

let rec nth list n =
  match list with
  | [] -> None
  | h :: t -> if n = 0 then Some h else nth t (n - 1)

let length list =
  let rec inner n = function [] -> n | _ :: t -> inner (n + 1) t in
  inner 0 list

let reverse list =
  let rec inner list acc =
    match list with [] -> acc | h :: t -> inner t (h :: acc)
  in
  inner list []

let is_palindrome list = list = reverse list
