open Process
open Printf

let requests=[1; 2; 3; 4; 1; 2; 5; 1; 2; 3; 4; 5];;
let frames_count=4;;

let process=new process frames_count;;
List.iteri (fun time page -> 
  printf "time: %d\n" time ;
  printf "request: %d\n" page ;
  process#push_request page ;
  process#update true )
  requests