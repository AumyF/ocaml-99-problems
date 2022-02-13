let rec last = function [] -> None | [ l ] -> Some l | _ :: t -> last t

let rec last_two list =
  match list with [] -> None | [ a; b ] -> Some (a, b) | _ :: t -> last_two t
