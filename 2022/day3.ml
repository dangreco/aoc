open Core 

let priority c = 
  match c with
  | 'A'..'Z' -> Char.to_int c - 38
  | 'a'..'z' -> Char.to_int c - 96
  | _ -> failwith "Invalid item"
;;

let intxn ls ~equal = 
  let rec inner ls acc =
    match (ls, acc) with
    | ([], _) -> acc
    | (l::ls', []) -> inner ls' l
    | (l::ls', acc) -> inner ls' (List.filter acc ~f:(List.mem l ~equal:equal))
  in inner ls [] 

let () =
  let input = Clap.mandatory_string () in
  Clap.close ();

  let packs = In_channel.read_lines input
    |> List.map ~f: (fun l -> String.to_list l |> List.map ~f: priority)
  in

  let sum1 = packs
    |> List.map ~f: (
         fun l -> List.split_n l (List.length l / 2)
         |> fun (c1, c2) -> intxn [c1; c2] ~equal:(=)
         |> List.hd_exn
       ) 
    |> List.fold ~init:0 ~f:(+)
  in

  let sum2 = packs
    |> List.chunks_of ~length:3
    |> List.map ~f:(fun l -> intxn l ~equal:(=) |> List.hd_exn)
    |> List.fold ~init:0 ~f:(+)
  in

  Printf.printf "Sum 1: %d\n" sum1;
  Printf.printf "Sum 2: %d\n" sum2;

  ()