open GlobalRequests;;

class policy requests frames_count processes =
  object(self)
    val processes=processes
    val requests=requests
    val frames_count=frames_count

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
      

    method print=
      Printf.printf "total page faults %d"
  end
;;