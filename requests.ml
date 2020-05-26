type range = {
  start : int;
  end_ : int;
};;

type requests_settings={
  phases_count : int;
  phase_range : int;
  phases_distance:range;
  requests_per_phase : range;
  process_sizes : int list;
};;

let random from to_=from+(Random.int (to_-from) ) ;;
let random_in_range range=random range.start range.end_ ;;

let generate_phases phases_count phase_range process_memory  =
  List.init phases_count 
    (fun _ ->  random phase_range (process_memory-phase_range))

let generate_requests_in_phase phase phase_range requests_per_phase =
  List.init (random_in_range requests_per_phase)
    (fun _ -> (phase+(random (-phase_range) phase_range)))

type request = { 
  time : int; 
  process_index : int; 
  page : int; 
};;

let connect_lists=List.fold_left (@) []

let generate_requests requests_settings offset process_size=
    let phases=(generate_phases requests_settings.phases_count requests_settings.phase_range process_size)
    in let current_time=ref 0
    in let phases_requests=
      List.map
      (fun phase->(
        current_time:=!current_time+(random_in_range requests_settings.phases_distance) ;
        let requests_in_phase=generate_requests_in_phase phase requests_settings.phase_range requests_settings.requests_per_phase
        in 
          List.map
          (fun request->
          let result={
              time=(!current_time);
              process_index=0;
              page=offset+request;
          } in begin
            current_time:=!current_time+1;
            result
          end )
        requests_in_phase))
      phases
    in connect_lists phases_requests

let merged_process_requests requests_settings processes_count=
  let result=ref []
  in let current_offset=ref 0
  in for i=0 to processes_count-1 do
    let process_size=(List.nth requests_settings.process_sizes i)
    in (result:=(!result)@(List.map
      (fun request -> {process_index=i; time=(request.time); page=(request.page)})
      (generate_requests requests_settings !current_offset process_size));
      current_offset:=(!current_offset)+process_size)
  done;
  !result
  ;;

let compare_requests request1 request2 = request1.time-request2.time ;;
let global_requests seed processes_count requests_settings=
  Random.init seed ;
  List.sort compare_requests (merged_process_requests requests_settings processes_count) ;;