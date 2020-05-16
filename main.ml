open Requests;;
open Utility;;
open Process;;

let processes_count=3;;
let frames_count=5;;

type request = { 
  time : int; 
  process_index : int; 
  page : int; 
};;

let rec generate_global_requests = function
  0 ->  []
  | processes_count -> 
    (List.mapi
      (fun index request -> {time=index; process_index=processes_count; page=request})
      (generate_requests 123 {start=2; end_=5} 2 {start=3; end_=5 } 10))
    @(generate_global_requests (processes_count - 1) )
    ;;

let compare_requests request1 request2 = request1.time-request2.time ;;
let global_requests=List.sort compare_requests (generate_global_requests processes_count) ;;
(* let print_request request=Printf.printf "{time : %d; process_index : %d; page : %d}\n" request.time request.process_index request.page ;;
List.iter print_request global_requests *)

let processes=list_of frames_count (fun () -> new process frames_count)
;;
let last_request=List.fold_left 
  (fun time request-> if request.time>time then request.time else time)
  0
  global_requests
;;

for t=0 to last_request do
  let requests=List.filter 
    (fun request->request.time==t)
    global_requests 
  in List.iter 
      (fun request->(List.nth processes request.process_index)#push_request request.page)
      requests ;
      List.iter 
      (fun process->process#update false)
      processes
done;;