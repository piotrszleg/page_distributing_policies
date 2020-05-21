open ProportionalPolicy;;

class zone_model requests frames_count processes_count delta_t =
  object(self)
    inherit proportional_policy requests frames_count processes_count
    as super

    val mutable last_visited_pages=
        (List.init processes_count (fun _ -> 0))
    val mutable free_frames=0

    method redistributeFrame=
      try
        let process=List.find
          (fun process->not process#is_running)
          processes
          in (process#add_frame ;
              process#resume ;
              free_frames<-free_frames-1)
      with Not_found->()

    (* stop the process with smallest wss *)
    method sorting_startegy wss1 wss2=wss2-wss1

    method sorted_processes wss=
      (* sorts processes according to their wss using sorting_strategy method *)
      let pairs=(List.map2
        (fun wss process->(wss, process))
        wss processes)
      in let sorted_pairs=(List.sort
        (fun (wss1, _) (wss2, _)->self#sorting_startegy wss1 wss2)
        pairs)
      in (List.map 
        (fun (_, process)->process)
      sorted_pairs)

    method stop_processes wss needed_D=
      let currently_needed=ref needed_D
      in List.iter2
        (fun process wss->
        if process#is_running && !currently_needed>0 then 
          process#stop ;
          currently_needed:=!currently_needed-wss)
        (self#sorted_processes wss) wss

    method update_processes=
      let wss=List.map2
      (fun process process_last_visited_pages->process#visited_pages-process_last_visited_pages)
      processes last_visited_pages 
      in let d=List.fold_left (+) 0 wss
      in 
        (if d>frames_count then
            self#stop_processes wss (d-frames_count)
         else if d<frames_count then
            self#redistributeFrame ) ;
        List.iter2
        (fun process wss->
          if process#is_running then 
          process#set_frames_count wss)
        processes wss ;
      last_visited_pages<-
        List.init processes_count 
        (fun i -> (List.nth processes i)#visited_pages)

    method! update=
      if (time mod delta_t)=0 then 
        self#update_processes ;
      super#update

    method! name="zone_model"
  end
;;