open Utility;;
open Process;;
open Policy;;
open Printf;;

class equal_policy requests frames_count processes_count =
  let frames_per_process=frames_count/processes_count
  in object
    inherit policy requests frames_count 
            (list_of processes_count (fun () -> new process frames_per_process)) 
    as super
    method! print=
      printf "equal policy\n" ;
      super#print
  end
;;