let list_of count f=
  let modified_f _=f () (*f takes () as argument and not int as in List.init *)
  in List.init count modified_f
  ;;

type range = {
  start : int;
  end_ : int;
};;

let random from to_=from+(Random.int (to_-from) ) ;;
let random_in_range range=random range.start range.end_ ;;


let random_array range count=
   List.init
   count
   (fun _->random_in_range range)