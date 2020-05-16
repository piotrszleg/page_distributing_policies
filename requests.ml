open Utility;;

type range = {
  start : int;
  end_ : int;
};;

let random from to_=from+(Random.int (to_-from) ) ;;
let random_in_range range=random range.start range.end_ ;;

let generate_phases phases_count_range phase_range disc_size  =
  let phases_count = random_in_range phases_count_range
  in list_of phases_count 
    (fun () ->  random phase_range (disc_size-phase_range))

let generate_requests_in_phase phase phase_range requests_per_phase =
  (list_of (random_in_range requests_per_phase)
    (fun () -> (phase+(random (-phase_range) phase_range))))

let generate_requests phases_count_range phase_range requests_per_phase disc_size =
    List.fold_left 
      (fun rest phase ->
        rest@(generate_requests_in_phase phase phase_range requests_per_phase))
      []
      (generate_phases phases_count_range phase_range disc_size)