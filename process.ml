open Printf;;
open Utility;;

type frame= {
  mutable page : int;
  mutable counter : int;
  mutable changed : bool;
};;

let empty_frame ()={page=(-1); counter=0; changed=false};;

class process frames_count =
  object(self)
    val mutable frames = list_of frames_count empty_frame
    val mutable page_faults = 0
    val mutable had_page_fault = false

    method find_page page = 
      List.find (fun frame -> frame.page==page) frames

    method find_empty = 
      List.find (fun frame -> (frame.page==(-1))) frames

    method find_lru = 
      let lru frame1 frame2 =
        (if (frame1.counter>frame2.counter)
        then frame1
        else frame2)
      in List.fold_left 
        lru
        (List.hd frames)
        frames

    method print = 
      List.iteri 
        ( fun i frame -> (printf "frame %i: %i, %i\n" i frame.page frame.counter) ) 
        frames; 
      printf "page fault: %b\n" had_page_fault ;
      printf "\n"

    method push_request page =
      try let frame=self#find_page page (* find page among frames *)
          in (frame.counter <- 0) ; (* reset counter if page in frame was needed *)
          frame.changed<-true
      with Not_found ->
        try let frame=self#find_empty (* change empty frame's content to page *)
            in (frame.page<-page) ; 
               (frame.counter <- 0) ;
               page_faults<-(page_faults+1) ;
               had_page_fault<-true ;
               frame.changed<-true
        with Not_found ->
          let frame=self#find_lru (* find least recently used frame and replace it *)
          in (frame.page<-page) ;
             (frame.counter <- 0) ;
             page_faults<-(page_faults+1) ;
             had_page_fault<-true ;
             frame.changed<-true

    method update print =
      let update_frame frame=
          ((if frame.page != -1 && not frame.changed then
            frame.counter<-frame.counter+1) ;
          (if frame.changed then frame.changed <- false))
          in List.iter update_frame frames ;
      if print then self#print ;
      had_page_fault<-false

    method add_frame =
      frames <- (empty_frame ()) :: frames

    method remove_frame = 
      let lru = self#find_lru
      in frames <- List.filter ((!=) lru) frames
        
  end
;;
