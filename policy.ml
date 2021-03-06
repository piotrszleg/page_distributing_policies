open Requests;;
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

let rotate list=
  List.mapi
    (fun index _->
      (List.map
      (fun row->List.nth row index)
      list)
    )
  (List.hd list)

  (* array[0].map((val, index) => array.map(row => row[index]).reverse()); *)

type page_fault = { 
  occurence_time : int; 
  process : int;
};;

class policy requests frames_count processes =
  object(self)
    val frames_count=frames_count
    val mutable time=0
    val mutable processes_frames_count=([]:int list list)
    val mutable page_faults=([]:page_fault list)
    val process_ends=
      List.mapi
      (fun process_index _->
        List.fold_left
        (fun max_time request->
          if request.process_index==process_index && request.time>max_time 
          then request.time 
          else max_time)
        0
        requests)
      processes
    val processes=processes
    val requests=requests
    val mutable stopped=0

    method update=
      let requests_for_process process_index process=List.filter 
          (fun request->request.time==process#time && request.process_index==process_index)
          requests
      in List.iteri
           (fun process_index process->
             if process#is_running && (process#frames_count>0) then
               List.iter
               (fun request->process#push_request request.page)
               (requests_for_process process_index process))
           processes ;
         List.iter 
          (fun process->process#update false)
          processes ;
         processes_frames_count<-
           (List.map
           (fun process->if process#is_running then process#frames_count else 0)
           processes)
           ::processes_frames_count;
         List.iteri
          (fun process_index process->
            if process#had_page_fault then
              page_faults<-{occurence_time=time; process=process_index}::page_faults)
          processes ;
        time<-time+1 ;
        let should_continue=ref false;
        in (List.iter2
          (fun process process_end->
            if process#time<process_end then should_continue:=true)
          processes process_ends;
          !should_continue)
        

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
        `List [`String "trashing"; `Int process#in_trashing];
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

    method frames_count_plot_json:Basic.t=
      let rotated=(rotate processes_frames_count)
      in let converted=
        List.map
        (fun row->`List
          (List.map
          (fun element->`Int element)
          row)
        )
        rotated
      in `Assoc [
          ("type", `String "line");
          ("xAxis", `String "time");
          ("yAxis", `String "frames");
          ("data", `List converted)
          ]

    method page_faults_plot_json:Basic.t=
      let converted=
        List.map
        (fun page_fault->`Assoc [
          ("x", `Int page_fault.occurence_time);
          ("y", `Int page_fault.process)
        ])
        page_faults
      in `Assoc [
          ("type", `String "scatter");
          ("xAxis", `String "time");
          ("yAxis", `String "process");
          ("data", `List [
            `Assoc [
              "label", `String "page faults" ;
              "borderColor", `String "#f09090";
              "backgroundColor", `String "#f09090";
              "data",`List converted]])
          ]

    method table_json:Basic.t=
      let processes_json=self#processes_to_json
      in let total_page_faults=List.fold_left 
        (fun sum process->sum+process#page_faults)
        0
        processes
      in let total_trashing=List.fold_left 
        (fun sum process->sum+process#in_trashing)
        0
        processes
      in
        `Assoc [
          ("type", `String "table");
          ("highlighted_rows", (`List [`String "process_index"; `String "name"]));
          ("cells", `List ([
            `List [`String "name"; `String self#name];
            `List [`String "total page faults"; `Int total_page_faults];
            `List [`String "total trashing"; `Int total_trashing];
            `List [`String "process was stopped"; `Int stopped]
          ]@processes_json))
        ]
  end
;;