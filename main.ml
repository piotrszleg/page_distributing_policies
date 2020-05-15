open Requests;;
open Printf;;

let ()=List.iter (printf "%d ") (generate_requests 123 {start=2; end_=5} 2 {start=3; end_=5 } 10) 