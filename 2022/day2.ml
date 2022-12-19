open Core

let wrap n = ((n mod 3) + 3) mod 3;;

let of_char = function
  | 'A' | 'X' -> 0
  | 'B' | 'Y' -> 1
  | 'C' | 'Z' -> 2
  | _ -> failwith "Invalid character"
;;

let score = function
  | (a, b) when a = b -> 3
  | (a, b) when wrap (a + 1) = b -> 6
  | _ -> 0
;;

let choose = function
  | (0, a) -> wrap (a - 1)
  | (1, a) -> a
  | (2, a) -> wrap (a + 1)
  | _ -> failwith "Invalid" 
;;

let () =
  let input = Clap.mandatory_string () in
  Clap.close ();

  In_channel.read_lines input
    |> List.map ~f: (fun s -> (of_char (String.get s 0), of_char (String.get s 2)))
    |> List.fold ~init: (0, 0) ~f: (
      fun (s1, s2) (l, r) -> 
        let r' = choose (r, l) in
        (
          s1 + (r + 1) + score (l, r), 
          s2 + (r' + 1) + score (l, r')
        )
    )
    |> fun (s1, s2) -> 
      Printf.printf "Method 1: %d\nMethod 2: %d\n" s1 s2 ;
      ()
