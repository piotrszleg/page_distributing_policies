open Printf;;
open Utility;;

type frame= {
  mutable page : int;
  mutable counter : int;
  mutable changed : bool;
};;

let empty_frame ()={page=(-1); counter=0; changed=false};;

class trashing_observer to_start to_end=
  object(self)
    val mutable is_trashing=false
    val mutable start_counter=0
    val mutable end_counter=0
    val mutable in_trashing=0

    method update_trashing page_fault=
      if not page_fault 
      then if end_counter==to_end 
           then is_trashing<-false
           else end_counter<-end_counter+1
      else begin
          in_trashing<-in_trashing+1 ;
          end_counter<-0
      end

    method update_not_trashing page_fault=
      if page_fault 
      then if start_counter==to_start
           then is_trashing<-true
           else start_counter<-start_counter+1
      else begin
          start_counter<-0
      end

    method update page_fault=
      if is_trashing 
      then self#update_trashing page_fault
      else self#update_not_trashing page_fault

    method in_trashing=in_trashing
  end
;;

class process frames_count =
  object(self)
    val mutable frames = list_of frames_count empty_frame
    val mutable page_faults = 0
    val mutable had_page_fault = false
    val mutable had_page_fault_previously=false
    val mutable visited_pages= 0
    val mutable running=true
    val mutable time=0
    val trashing_observer=new trashing_observer 1 2

    method in_trashing=trashing_observer#in_trashing

    method time=time

    method frames_count=List.length frames

    method page_faults=page_faults

    method had_page_fault=had_page_fault_previously

    method visited_pages=visited_pages

    method find_page page = 
      List.find (fun frame -> frame.page==page) frames

    method find_empty = 
      List.find (fun frame -> (frame.page==(-1))) frames

    method stop=
      (frames<-[];
      running<-false)

    method resume=running<-true

    method is_running=running

    method find_lru =
      if frames==[] then raise Not_found ;
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
      visited_pages<-visited_pages+1 ;
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
      (trashing_observer#update had_page_fault);
      had_page_fault_previously<-had_page_fault ;
      had_page_fault<-false ;
      if running then time<-time+1

    method set_frames_count count=
      let difference=count-self#frames_count
      in if difference>0 then
        (frames <- (list_of difference empty_frame) @ frames ;
        assert (count==self#frames_count))
      else if difference<0 then
        (for _=1 to -difference do
          self#remove_frame
        done ;
        assert (count==self#frames_count))

    method add_frame =
      frames <- (empty_frame ()) :: frames

    method remove_frame = 
      try
        let lru = self#find_lru
        in frames <- List.filter ((!=) lru) frames
      with Not_found ->()
        
  end
;;
