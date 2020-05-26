open Process;;
open Policy;;

let divide number parts=
  let parts=List.init parts
  (fun _->number/parts)
  in let sum=List.fold_left (+) 0 parts
  in let rest=ref (number-sum)
  in 
  if (!rest)>0
  then 
    List.map
    (fun part->
    if (!rest)>0 
    then begin 
      rest:=(!rest)-1;
      part+1
    end  
    else part)
    parts
  else parts

class equal_policy requests frames_count processes_count =
  let processes=
  List.map
  (fun frames->new process frames)
  (divide frames_count processes_count)
  in object
    inherit policy requests frames_count processes
            
    method! name="equal policy"
  end
;;