open Core

let range (i, j) = j - i + 1
let overlap (a0, a1) (b0, b1) =
  let l = max a0 b0 in
  let r = min a1 b1 in
  if r < l then 0 else r - l + 1

let halve s ~on =
  match String.split s ~on:on with
  | a::b::[] -> Some((a, b))
  | _ -> None 
;;

let parse line = 
  let parsed = Option.map (halve line ~on:',') ~f:(Tuple2.map ~f:(fun e ->
    Option.map (halve e ~on:'-') ~f:(Tuple2.map ~f:(int_of_string))
  )) 
  in
  match parsed with
  | Some((Some((a0, a1)), Some((b0, b1)))) -> ((a0, a1), (b0, b1))
  | _ -> failwith "Invalid line"
;;

let () =
  let input = Clap.mandatory_string () in
  Clap.close ();

  let pairs = In_channel.read_lines input
    |> List.map ~f:(parse)
  in

  let subsets = pairs
    |> List.filter ~f:(fun (a, b) -> min (range a) (range b) = overlap a b)
    |> List.length
  in

  let overlaps = pairs
    |> List.filter ~f:(fun (a, b) -> overlap a b > 0)
    |> List.length
  in

  Printf.printf "Subsets: %d\n" subsets;
  Printf.printf "Overlaps: %d\n" overlaps;

  ()