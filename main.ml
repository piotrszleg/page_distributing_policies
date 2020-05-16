
open GlobalRequests;;
open EqualPolicy;;

let processes_count=5;;
let frames_count=20;;

(*
let print_request request=Printf.printf "{time : %d; process_index : %d; page : %d}\n" request.time request.process_index request.page ;;
List.iter print_request (global_requests 5);;
*)
Printf.printf "%d"
  (new equal_policy (global_requests 123 processes_count) frames_count processes_count)#run