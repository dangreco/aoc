open Core

let () =
  let input = Clap.mandatory_string () in
  let top = Clap.default_int 3 in
  Clap.close ();

  In_channel.read_lines input
    |> List.map ~f: int_of_string_opt
    |> List.fold ~init: [0] ~f: (
        fun l n ->
          match (l, n) with
          | (x::xs, Some(n)) -> (x+n)::xs
          | (l, None) -> 0::l
          | _ -> failwith "Shouldn't fail"
      )
    |> List.sort ~compare: (fun x y -> y - x)
    |> fun l -> List.take l top
    |> fun l -> (l, List.fold l ~init: 0 ~f:(+))
    |> fun (top, sum) -> 
      Printf.printf "Sum: %d\n" sum;
      List.iter top ~f:(Printf.printf "%d\n")
