open Utility;;
open Process;;
open GlobalRequests;;

class equal_policy requests frames_count processes_count =
  let frames_per_process=frames_count/processes_count
  in object(self)
    val processes=list_of processes_count (fun () -> new process frames_per_process)
    val requests=requests

    method update time=
      let requests_at_time=List.filter 
          (fun request->request.time==time)
          requests 
        in  List.iter 
              (fun request->(List.nth processes request.process_index)#push_request request.page)
              requests_at_time ;
            List.iter 
              (fun process->process#update false)
              processes

    method run=
      let last_request=List.fold_left 
        (fun time request-> if request.time>time then request.time else time)
        0
        requests
      in for time=0 to last_request do
        self#update time
      done ;
      List.fold_left 
        (fun rest process ->(rest+process#page_faults))
        0
        processes
  end
;;