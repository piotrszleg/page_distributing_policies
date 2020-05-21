open GlobalRequests;;

class policy requests frames_count processes =
  object(self)
    val processes=processes
    val requests=requests
    val frames_count=frames_count
    val mutable time=0

    method update=
      let requests_for_process process_index process=List.filter 
          (fun request->request.time==process#time && request.process_index=process_index)
          requests
      in let should_continue=ref false
      in List.iteri
           (fun process_index process->
             if process#is_running then
               List.iter
               (fun request->
                 (process#push_request request.page;
                 should_continue:=true))
               (requests_for_process process_index process)
               else should_continue:=true)(* wait for paused process *)
           processes ;
         List.iter 
           (fun process->process#update false)
           processes ;
        time<-time+1 ;
        !should_continue

    method run=
      while self#update do () done

    method name="policy"
      
    method print=
      Printf.printf "%s\n" self#name ;
      let total_page_faults=List.fold_left 
        (fun sum process->sum+process#page_faults)
        0
        processes
      in Printf.printf "total page faults %d\n" total_page_faults
  end
;;