open Requests;;

type request = { 
  time : int; 
  process_index : int; 
  page : int; 
};;

let rec merged_process_requests requests_settings processes_count=
  match processes_count with
  0 ->  []
  | processes_count -> 
    (List.mapi
      (fun index request -> {time=index; process_index=(processes_count-1); page=request})
      (generate_requests requests_settings))
    @(merged_process_requests requests_settings (processes_count - 1) )
    ;;

let compare_requests request1 request2 = request1.time-request2.time ;;
let global_requests seed processes_count requests_settings=
  Random.init seed ;
  List.sort compare_requests (merged_process_requests requests_settings processes_count) ;;