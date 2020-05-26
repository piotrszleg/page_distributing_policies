open ProportionalPolicy;;
(*open Printf;;*)

class page_error_rate_control_policy requests frames_count processes_count process_sizes delta_t lower upper stop =
  object(self)
    inherit proportional_policy requests frames_count processes_count process_sizes
    as super

    val mutable last_page_faults=
        (List.init processes_count (fun _ -> 0))
    val mutable free_frames=0

    method redistribute_frames=
      try
        while free_frames>0 do
          let process=List.find
            (fun process->not process#is_running)
            processes
            in (process#add_frame ;
                process#resume ;
                free_frames<-free_frames-1)
        done
      with Not_found->()

    method stop_process process=
      free_frames<-free_frames+process#frames_count ;
      self#redistribute_frames ;
      process#stop

    method update_processes=
      List.iter2
      (fun process last_page_faults->
        let e=process#page_faults-last_page_faults
        in  (*printf "e=%d, running=%b\t" e (process#is_running);*)
          (if e<lower && process#frames_count>1 then
            (process#remove_frame ;
            free_frames<-free_frames+1 ;
            self#redistribute_frames)
          else if e>upper && free_frames>0 then
            (process#add_frame ;
            free_frames<-free_frames-1)
          else if e>stop then 
            self#stop_process process ;
        )
      )
      processes last_page_faults ;
      last_page_faults<-
        List.map (fun process->process#page_faults)
        processes (*;
      printf "\n"*)

    method! update=
      if (time mod delta_t)=0 then 
        self#update_processes ;
      super#update

    method! name="page_error_rate_control_policy"
  end
;;