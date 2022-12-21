open Core

let flip f x y = f y x ;;
let break list ~f = List.partition_tf list ~f:(fun a -> not (f a))

type item = File of string * int | Dir of string * item list;;
type crumb = Crumb of string * item list * item list;;
type zipper = Zipper of item * crumb list;;   

module Fs = struct

  let mkfs = Zipper (Dir ("/", []), [])

  let name = function
    | File (name, _) -> name
    | Dir (name, _) -> name

  let is_file = function
    | File _ -> true
    | _ -> false

  let is_dir = function
    | Dir _ -> true
    | _ -> false

  let up = function 
    | Zipper (node, Crumb (name, ls, rs)::bs) -> Zipper (Dir (name, ls @ [node] @ rs), bs)
    | x -> x

  let down target = function
    | Zipper (Dir (dir, items), bs) as cursor ->
      begin
        match (break items ~f:(fun i -> String.equal (name i) target)) with
        | (ls, item::rs) -> Zipper (item, Crumb (dir, ls, rs)::bs)
        | _ -> cursor
      end
    | cursor -> cursor

  let touch item = function
    | Zipper (Dir (dir, items), bs) -> Zipper (Dir (dir, item::items), bs)
    | cursor -> cursor

  let root fs = 
    let rec root' = function 
      | Zipper (_, []) as root -> root
      | cursor -> root' (up cursor) 
    in root' fs
    
  let fold fs ~init ~f =
    let rec fold' i = function
    | Zipper (File _ as node, _) -> f i node
    | Zipper (Dir (_, items) as node, _) as c ->
      List.fold
        items
        ~init:(f i node)
        ~f:(fun a node -> fold' a (down (name node) c))
    in fold' init fs

  let sizeof node =
    let rec sizeof' a = function
      | File (_, s) -> a + s
      | Dir (_, items) -> List.fold items ~init:a ~f:(fun a -> sizeof' a)
    in
    sizeof' 0 node 

end

let parse lines = 
  let rec parse' fs = function
  | [] -> fs
  | line::lines' -> 
    begin
      let fs' = 
        match (String.split line ~on:' ') with
        | ["$"; "cd"; "/"] -> Fs.root fs
        | ["$"; "cd"; ".."] -> Fs.up fs
        | ["$"; "cd"; target] -> Fs.down target fs
        | ["dir"; name] -> Fs.touch (Dir(name, [])) fs
        | [size; name] when not (String.equal size "$") -> Fs.touch (File(name, Int.of_string size)) fs
        | _ -> fs
      in
      parse' fs' lines'
    end
  in parse' Fs.mkfs lines

let () =
  let (Zipper (root, _)) as fs = 
    Clap.mandatory_string ()
    |> fun i -> Clap.close (); i
    |> In_channel.read_lines
    |> parse
    |> Fs.root
  in

  assert (String.equal (Fs.name root) "/");

  (* Part 1 *)
  Fs.fold 
    fs 
    ~init:0 
    ~f:(fun a node -> let s = Fs.sizeof node in if (Fs.is_dir node) && s <= 100000 then a + s else a) 
    |> Printf.printf "%d\n" ;
  
  (* Part 2 *)
  let used = Fs.sizeof root in
  let unused = 70000000 - used in
  let delta = 30000000 - unused in
  
  Fs.fold 
    fs 
    ~init:Int.max_value
    ~f:(fun a node -> 
      if Fs.is_dir node then
        let s = Fs.sizeof node in
        if s >= delta && s < a then s else a
      else
        a
    ) 
    |> Printf.printf "%d\n" ;


  ()