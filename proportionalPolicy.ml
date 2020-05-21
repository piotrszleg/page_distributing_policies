open Process;;
open GlobalRequests;;
open Policy;;
open Printf;;

let max_index list=
  let rec max_index_recursive list index =
    match list with
      [] -> 0,0
      | head::rest->
        let (max_value, max_index)=(max_index_recursive rest (index+1))
        in if head>max_value 
           then head, index
           else max_value, max_index
  in let (_, index) = max_index_recursive list 0
  in index
  ;;

(* divide value into array according to weights 
   so that array's elements sum up to the value *)
let divide_weighted value weights=
  let divided=
    List.init (List.length weights)
    (fun index -> let weight=List.nth weights index
                  in int_of_float (weight*.(float_of_int value)))
  in let rest=ref (value-(List.fold_left (+) 0 divided))
  (* create new list where the rest is added to biggest element *)
  in List.init (List.length divided)
    (fun index -> 
      (List.nth divided index) + 
      if !rest>0 
      then (rest:=!rest-1; 1) 
      else 0)
  ;;

class proportional_policy requests frames_count processes_count =
  let process_requests index=
    List.fold_left 
      (fun counter request -> 
        if request.process_index==index 
          then counter+1 
          else counter)
      0
      requests
  in let weights=
    List.init processes_count
    (fun index -> (float_of_int (process_requests index))/.(float_of_int (List.length requests)) )
  in let process_frames_count=divide_weighted frames_count weights

  in object(self)
    inherit policy requests frames_count (List.init 
      processes_count 
      (fun index -> new process (List.nth process_frames_count index)))
    as super

    method print_process index=
      printf "process\t%d:\n" index ;
      printf "requests\t%d\n" (process_requests index) ;
      let process=List.nth processes index
      in printf "frames count\t%d\n" process#frames_count ;
         printf "page faults\t%d\n---\n" process#page_faults

    method! name="proportional policy"
    
    method! print=
      super#print ;
      for i=0 to (List.length processes-1) do
        self#print_process i
      done ;
  end
;;