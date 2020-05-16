open Requests;;

type request = { 
  time : int; 
  process_index : int; 
  page : int; 
};;

let rec merged_process_requests = function
  0 ->  []
  | processes_count -> 
    (List.mapi
      (fun index request -> {time=index; process_index=(processes_count-1); page=request})
      (generate_requests {start=2; end_=5} 2 {start=3; end_=5 } 10))
    @(merged_process_requests (processes_count - 1) )
    ;;

let compare_requests request1 request2 = request1.time-request2.time ;;
let global_requests seed processes_count=
  Random.init seed ;
  List.sort compare_requests (merged_process_requests processes_count) ;;