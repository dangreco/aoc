open Core

module CharSet = Set.Make(Char) ;;

let map_none ~f = function Some(x) -> Some(x) | None -> f () ;; 

let sld l ~n ~init ~f =
  let rec sld' l i k idx =
    match l with
    | [] -> k i
    | l when List.length l < n -> k i
    | _::tl -> sld' tl (f i idx (List.slice l 0 n)) k (idx + 1)
  in sld' l init (fun x -> x) 0
;;

let is_start l =
  CharSet.of_list l
  |> CharSet.length
  |> fun n -> List.length l = n
;;

let find_start l ~n = 
  l |> sld ~n ~init:None ~f:(fun a i l -> 
    map_none a ~f:(fun () -> 
      if is_start l then 
        Some(i + n) 
      else 
        a
    )
  )

let () =
  let signal = Clap.mandatory_string ()
    |> fun i -> Clap.close (); i
    |> In_channel.read_all
    |> String.to_list
  in

  [4; 14]
  |> List.iter ~f:(fun n -> 
       find_start signal ~n
       |> Option.value_exn
       |> Printf.printf "%d\n"
     )
