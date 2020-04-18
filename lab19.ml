(*
                             CS51 Lab 19
                    Synthesis -- The ATM Emulator
 *)

(* Make use of the ATM component behaviors *)
module ATM = ATMcomponents ;;
open Printf ;;

(* Exceptions for: *)
(* ... moving on to the next customer *)
exception ATMNext ;;
(* ... exiting the ATM machine emulation *)
exception ATMFinished ;;

(* atm initial -- Emulate an ATM communicating over `stdin` and
   `stdout` with customers until finished. The provided `initial` is
   an initial set of accounts to use. *)
let atm (initial : ATM.account_spec list) : unit =

  (* prime the set of accounts, assigning each an id and recording
     name and balance *)
  ATM.initialize initial;

  try
    (* process customers until finished *)
    while true do

      (* prompt for the customer's id and provide welcome *)
      let id = ATM.acquire_id () in
      ATM.present_message ("Welcome " ^ (ATM.get_name id));

      try
        (* perform actions on behalf of the selected customer *)
        while true do
          let act = ATM.acquire_act () in

          match act with

          (* balance inquiry *)
          | Balance ->
             ATM.present_message
               (sprintf "Current balance: %d" (ATM.get_balance id))

          (* withdrawal *)
          | Withdraw amount ->
             let bal = ATM.get_balance id in
             if amount > bal then
               ATM.present_message (sprintf "Insufficient funds: %d" bal)
             else
               (ATM.update_balance id (bal - amount);
                ATM.deliver_cash amount;
                ATM.present_message
                  (sprintf "New balance: %d" (ATM.get_balance id)))

          (* deposit *)
          | Deposit amount ->
             let bal = ATM.get_balance id in
             ATM.update_balance id (bal + amount);
             ATM.present_message
               (sprintf "New balance: %d" (ATM.get_balance id))

          (* done with this customer; move on to the next *)
          | Next ->
             ATM.present_message "So long.\n";
             raise ATMNext

          (* exit the ATM emulation *)
          | Finished ->
             ATM.present_message "Exiting the ATM emulation";
             raise ATMFinished
        done
      with ATMNext -> ()

    done
  with ATMFinished -> () ;;

(* Some initial accounts *)
let init : ATM.account_spec list =
  [ {id = 314159; name = "Emily"; balance = 100};
    {id = 271828; name = "Hannah"; balance = 400};
    {id = 141421; name = "Jacob"; balance = 0};
    {id = 161803; name = "Michael"; balance = 220}
  ] ;;

(* Fire up the ATM *)
let _ = atm init ;;
