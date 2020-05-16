
open GlobalRequests;;
open ProportionalPolicy;;

let processes_count=5;;
let frames_count=20;;

(*
let print_request request=Printf.printf "{time : %d; process_index : %d; page : %d}\n" request.time request.process_index request.page ;;
List.iter print_request (global_requests 5);;
*)
let policy=new proportional_policy (global_requests 123 processes_count) frames_count processes_count
in policy#run ;
   policy#print