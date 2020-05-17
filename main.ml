
open GlobalRequests;;
open EqualPolicy;;
open ProportionalPolicy;;
open PageErrorRateControlPolicy;;

let processes_count=5;;
let frames_count=10*5;;

(*
let print_request request=Printf.printf "{time : %d; process_index : %d; page : %d}\n" request.time request.process_index request.page ;;
List.iter print_request (global_requests 5);;
*)

let evaluate_policy policy_constructor=
   let policy=(policy_constructor (global_requests 123 processes_count) frames_count processes_count)
   in policy#run ;
      policy#print ;
   Printf.printf "\n"
in
evaluate_policy (new equal_policy) ;
evaluate_policy (new proportional_policy) ;
evaluate_policy (fun requests frames_count processes_count -> new page_error_rate_control_policy requests frames_count processes_count 20 3 5 10)