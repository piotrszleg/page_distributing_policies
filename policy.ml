open GlobalRequests;;
open Yojson;;
open Printf;;

let process_requests requests index=
    List.fold_left 
      (fun counter request -> 
        if request.process_index==index 
          then counter+1 
          else counter)
      0
      requests;;


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

    method print_process index (process:Process.process)=
      printf "process\t%d:\n" index ;
      printf "requests\t%d\n" (process_requests requests index) ;
      printf "frames count\t%d\n" process#frames_count ;
      printf "page faults\t%d\n---\n" process#page_faults

    method process_to_json index process=
      [ 
        `List [`String "process_index"; `Int index];
        `List [`String "requests"; `Int (process_requests requests index)];
        `List [`String "frames count"; `Int process#frames_count];
        `List [`String "page faults"; `Int process#page_faults];
      ]
      
    method print=
      printf "%s\n" self#name ;
      let total_page_faults=(List.fold_left 
        (fun sum process->sum+process#page_faults)
        0
        processes)
      in (List.iteri
        self#print_process
        processes) ;
      printf "total page faults %d\n" total_page_faults

    method processes_to_json=
      let processes_json=ref ([]: Basic.t list)
      in (List.iteri
        (fun index process->processes_json:=(!processes_json)@(self#process_to_json index process))
        processes) ;
        !processes_json

    method table_json:Basic.t=
      let processes_json=self#processes_to_json
      in let total_page_faults=List.fold_left 
        (fun sum process->sum+process#page_faults)
        0
        processes
      in
        `Assoc [
          ("type", `String "table");
          ("highlighted_rows", (`List [`String "process_index"; `String "name"]));
          ("cells", `List ([
            `List [`String "name"; `String self#name];
            `List [`String "total page faults"; `Int total_page_faults]
          ]@processes_json))
        ]
  end
;;