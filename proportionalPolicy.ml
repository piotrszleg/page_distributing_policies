open Process;;
open GlobalRequests;;
open Policy;;

class proportional_policy requests frames_count processes_count =
  let requests_count = List.length requests
  in let process_requests process_index=
    List.fold_left 
      (fun counter request -> 
        if request.process_index==process_index 
          then counter+1 
          else counter)
      0
      requests
  in let frames_count process=
    let ratio=(float_of_int (process_requests process))/.(float_of_int requests_count)
    in int_of_float (ratio*.(float_of_int frames_count))
  in object
    inherit policy requests frames_count (List.init 
      processes_count 
      (fun index -> new process (frames_count index)))
  end
;;