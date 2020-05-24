
open GlobalRequests;;
open EqualPolicy;;
open ProportionalPolicy;;
open PageErrorRateControlPolicy;;
open Requests;;
open ZoneModel;;
open Yojson;;
open Printf;;

let json_output_file="output/data.js"

let processes_count=5;;
let frames_count=10*5;;

(*
let print_request request=Printf.printf "{time : %d; process_index : %d; page : %d}\n" request.time request.process_index request.page ;;
List.iter print_request (global_requests 5);;
*)

let request_settings={
  phases_count=100;
  phase_range=3;
  requests_per_phase={start=2; end_=20 };
  disc_size=100;
};;

let json_output=ref ([]:Basic.t list);;

let evaluate_policy policy_constructor=
   let policy=(policy_constructor (global_requests 123 processes_count request_settings) frames_count processes_count)
   in policy#run ;
      policy#print ;
      json_output:=policy#table_json::!json_output;
   printf "\n"
in let delta_t=20
in
evaluate_policy (new equal_policy) ;
evaluate_policy (new proportional_policy) ;
evaluate_policy (fun requests frames_count processes_count -> new page_error_rate_control_policy requests frames_count processes_count delta_t 3 5 10) ;
evaluate_policy (fun requests frames_count processes_count -> new zone_model requests frames_count processes_count delta_t (delta_t/2) ) ;
let file = open_out json_output_file
in fprintf file "data="; Basic.pretty_to_channel file (`List !json_output)