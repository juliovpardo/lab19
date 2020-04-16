(*
                Component Behaviors of an ATM Machine

The functions here represent the component behaviors that an ATM
machine can take, including: prompting for and acquiring from the
customer some information (choosing an action or entering an account
id or an amount); presenting information to the customer; dispensing
cash.

Implementation of these behaviors is likely to require some database
of accounts, each with an id number, a customer name, and a current
balance.
 *)

(* Customer account identifiers *)
type id = int

(* Possible actions that an ATM customer can perform *)
type action =
  | Balance           (* balance inquiry *)
  | Withdraw of int   (* withdraw an amount *)
  | Deposit of int    (* deposit an amount *)
  | Next              (* finish this customer and move on to the next one *)
  | Finished          (* shut down the ATM and exit entirely *)
;;

(*....................................................................
  Initializing database of accounts
*)

(* A specification of a customer name and initial balance for
   initializing the account database *)
type account_spec = {name : string; id : id; balance : int} ;;

(* initialize accts -- Establishes a database of accounts, each with a
   name, aribtrary id, and balance. The names and balances are
   initialized as per the `accts` provided. *)
val initialize : account_spec list -> unit ;;

(*....................................................................
  Acquiring information from the customer
*)

(* acquire_id () -- Requests from the ATM customer and returns an id
   (akin to entering one's ATM card), by prompting for an id number
   and reading an id from stdin. *)
val acquire_id : unit -> id ;;

(* acquire_amount () -- Requests from the ATM customer and returns an
   amount by prompting for an amount and reading an int from stdin. *)
val acquire_amount : unit -> int ;;

(* acquire_act () -- Requests from the user and returns an action to
   be performed, as a value of type action *)
val acquire_act : unit -> action ;;

(*....................................................................
  Querying and updating the account database

  These functions all raise Not_found if there is no account with the
  given id.
*)

let rec get_account (id : id) (lst : account_spec list) =
  match lst with
  | [] -> Not_found
  | h :: t -> if id = h.id then h
              else get_account id tl ;;

(* get_balance id -- Returns the balance for the customer account with
   the given id. *)
let get_balance (id : id) : int =
  (get_account id REPLACE_WITH_WHATEVER_THE_DATABASE_IS_CALLED).balance ;;

(* get_name id -- Returns the name associated with the customer
   account with the given id. *)
let get_name (id : id) : string =
  (get_account id REPLACE_WITH_WHATEVER_THE_DATABASE_IS_CALLED).name ;;

(* update_balance id amount -- Modifies the balance of the customer
   account with the given id, setting it to the given amount. *)
let update_balance (id : id) (amt : int) : unit =
  (* Todo *) () ;;

(*....................................................................
  Presenting information and cash to the customer
*)

(* present_message message -- Presents to the customer (on stdout) the
   given message followed by a newline. *)
let present_message (msg : string) : unit =
  print_string msg ^ "\n" ;;

(* deliver_cash amount -- Dispenses the given amount of cash to the
   customer (really just prints to stdout a message to that
   effect). *)
let deliver_cash (amt : int) : unit =
  present_message ("Here's your cash: " ^
                   if amt < 20 then
                     string_of_int amt
                   else
                     (let rec deliver_cash_helper amt =
                        if (amt - 20) > 0 then
                          "[20 @ 20]" ^ deliver_cash_helper (amt - 20)
                        else
                          " and " ^ string_of_int amt ^ " more" in
                      deliver_cash_helper amt)) ;;
