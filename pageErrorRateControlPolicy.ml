open ProportionalPolicy;;

class page_error_rate_control_policy requests frames_count processes_count delta_t lower upper stop_height =
  object(self)
    inherit proportional_policy requests frames_count processes_count
    as super

    val mutable last_page_faults=
        (List.init processes_count (fun _ -> 0))
    val mutable free_frames=0

    method redistribute_frame=
      try
        let process=List.find
          (fun process->not process#is_running)
          processes
          in (process#add_frame ;
              process#resume ;
              free_frames<-free_frames-1)
      with Not_found->()

    method update_processes=
      for i= 0 to processes_count-1 do 
        let process = (List.nth processes i)
        in let e=process#page_faults-(List.nth last_page_faults i)
        in (if e<lower && process#frames_count>1 then
            ((process#remove_frame) ;
            free_frames<-free_frames+1 ;
            self#redistribute_frame)
          else if e>upper && free_frames>0 then
            ((process#add_frame) ;
            free_frames<-free_frames-1)
          else if e>stop_height then 
            (process#stop))
      done ;
      last_page_faults<-
        List.init processes_count 
        (fun i -> (List.nth processes i)#page_faults)

    method! update=
      if (time mod delta_t)=0 then 
        self#update_processes ;
      super#update

    method! name="page_error_rate_control_policy"
  end
;;