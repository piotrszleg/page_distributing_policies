open Utility;;

type range = {
  start : int;
  end_ : int;
};;

type requests_settings={
  phases_count : int;
  phase_range : int;
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

let generate_requests requests_settings =
    List.fold_left 
      (fun rest phase ->
        rest@(generate_requests_in_phase phase requests_settings.phase_range requests_settings.requests_per_phase))
      []
      (generate_phases requests_settings.phases_count requests_settings.phase_range requests_settings.disc_size)