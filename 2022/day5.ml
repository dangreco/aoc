open Core

let rec ( -- ) i j = if i > j then [] else i :: i + 1 -- j
let inv f x = not (f x)
let index l ~predicate = List.findi_exn l ~f:(fun _ a -> predicate a) ;;
let dsp = Array.iter ~f:(fun s -> Printf.printf "%c" (Stack.pop_exn s)) ;;
let cpy = Array.map ~f:Stack.copy ;;

let parse_initial lines =
  lines
  |> List.drop_last_exn |> List.map ~f:String.to_list
  |> List.transpose_exn |> List.filteri ~f:(fun i _ -> i mod 4 = 1)
  |> List.map ~f:(List.filter ~f:(inv Char.is_whitespace))
  |> List.map ~f:Stack.of_list
  |> List.to_array
;;

let parse_commands = List.map ~f:(fun line ->
  line
  |> String.split ~on:' '
  |> List.map ~f:int_of_string_opt
  |> List.filter_opt
  |> function [amt; src; dst] -> (amt, src, dst) | _ -> assert false
)
;;

let parse lines =
  let (i, _) = (index lines ~predicate:String.is_empty) in
  lines
  |> List.filter ~f:(inv String.is_empty)
  |> fun l -> List.split_n l i
  |> fun (a, b) -> (parse_initial a, parse_commands b)
;;

let move stacks ~amt ~src ~dst ~rev =
  let op = if rev then List.rev else (fun l -> l) in
  let src = Array.nget stacks (src - 1) in
  let dst = Array.nget stacks (dst - 1) in
  1 -- amt
    |> List.map ~f:(fun _ -> Stack.pop_exn src)
    |> op
    |> List.iter ~f:(Stack.push dst)
;;

let () =
  let input = Clap.mandatory_string () in
  Clap.close ();

  let (stacks, commands) = In_channel.read_lines input
    |> parse
  in

  (* Part One *)
  let s1 = stacks |> cpy in
  List.iter commands ~f:(fun (amt, src, dst) -> move s1 ~amt ~src ~dst ~rev:false);
  dsp s1;
  
  printf "\n";

  (* Part Two *)
  let s2 = stacks |> cpy in
  List.iter commands ~f:(fun (amt, src, dst) -> move s2 ~amt ~src ~dst ~rev:true);
  dsp s2;
  
  ()