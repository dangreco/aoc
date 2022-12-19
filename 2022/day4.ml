open Core

let range (i, j) = j - i + 1
let overlap (a0, a1) (b0, b1) =
  let l = max a0 b0 in
  let r = min a1 b1 in
  if r < l then 0 else r - l + 1

let parse_range s =
  match String.split s ~on:'-' with
  | [s; e] -> (Int.of_string s, Int.of_string e)
  | _ -> failwith "Invalid range"
;;

let parse line = 
  match String.split line ~on:',' with
  | [elf1; elf2] -> (parse_range elf1, parse_range elf2)
  | _ -> failwith "Invalid range"

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