open Utility;;

type range = {
  start : int;
  end_ : int;
};;

type requests_settings={
  phases_count : int;
  phase_range : int;
  phases_distance:range;
  requests_per_phase : range;
  disc_size : int;
};;

let random from to_=from+(Random.int (to_-from) ) ;;
let random_in_range range=random range.start range.end_ ;;

let generate_phases phases_count phase_range disc_size  =
  list_of phases_count 
    (fun () ->  random phase_range (disc_size-phase_range))

let generate_requests_in_phase phase phase_range requests_per_phase =
  (list_of (random_in_range requests_per_phase)
    (fun () -> (phase+(random (-phase_range) phase_range))))

type request = { 
  time : int; 
  process_index : int; 
  page : int; 
};;

let connect_lists=List.fold_left (@) []

let generate_requests requests_settings =
    let phases=(generate_phases requests_settings.phases_count requests_settings.phase_range requests_settings.disc_size)
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
              page=request;
          } in begin
            current_time:=!current_time+1;
            result
          end )
        requests_in_phase))
      phases
    in connect_lists phases_requests

let rec merged_process_requests requests_settings processes_count=
  match processes_count with
  0 ->  []
  | processes_count -> 
    (List.map
      (fun request -> {process_index=(processes_count-1); time=(request.time); page=(request.page)})
      (generate_requests requests_settings))
    @(merged_process_requests requests_settings (processes_count - 1) )
    ;;

let compare_requests request1 request2 = request1.time-request2.time ;;
let global_requests seed processes_count requests_settings=
  Random.init seed ;
  List.sort compare_requests (merged_process_requests requests_settings processes_count) ;;