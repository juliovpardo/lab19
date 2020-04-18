(* Customer account identifiers *)
type id = int ;;

(* Maps (dictionaries) with integer keys *)
module IntMap = Map.Make (struct
                            type t = int
                            let compare : int -> int -> int = compare
                          end) ;;

(* The account database itself, a reference to a map from ints to
pairs of name and balance, initially the empty map. *)
let db : (string * int) IntMap.t ref = ref IntMap.empty ;;

let create (id : id) (name : string) : unit =
  db := IntMap.add id (name, 0) !db ;;

let find (id : id) : string * int =
  IntMap.find id !db ;;

let exists (id : id) : bool =
  IntMap.exists (fun key _value -> key = id) !db ;;

let balance (id : id) : int =
  let _name, bal = find id
  in bal ;;

let name (id : id) : string =
  let name, _bal = find id
  in name ;;

let update (id : id) (value : int) : unit =
  let nam = name id
  in db := IntMap.add id (nam, value) !db ;;

let close (id : id) : unit =
  db := IntMap.remove id !db ;;

let dump () =
 !db
 |> IntMap.iter (fun i (nam, bal) ->
                                  Printf.printf "[%d] %s -> %d\n" i nam bal) ;;
