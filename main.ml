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
  phases_count=20;
  phase_range=3;
  phases_distance={start=5; end_=10 };
  requests_per_phase={start=2; end_=20 };
  disc_size=100;
};;

let json_output=ref ([]:Basic.t list);;

let requests=global_requests 123 processes_count request_settings;;

let make_header text=`Assoc [
   "type", `String "header";
   "text", `String text
];;

let requests_as_json_points=
   List.map
   (fun request->`Assoc [
      "x", `Int request.time;
      "y", `Int request.process_index
   ])
   requests
in let requests_plot=`Assoc [
   ("type", `String "scatter");
   ("xAxis", `String "time");
   ("yAxis", `String "process");
   ("data", `List [
      `Assoc [
         "label", `String "request"; 
         "borderColor", `String "#99c2ff";
         "backgroundColor", `String "#99c2ff";
         "data", `List requests_as_json_points]] )
]
in json_output:=requests_plot::(make_header "requests")::!json_output;

let evaluate_policy policy_constructor=
   let policy=(policy_constructor requests frames_count processes_count)
   in policy#run ;
      policy#print ;
      json_output:=
      policy#table_json
      ::policy#page_faults_plot_json
      ::policy#frames_count_plot_json
      ::(make_header policy#name)
      ::!json_output;
   printf "\n"
in let delta_t=20
in
evaluate_policy (new equal_policy) ;
evaluate_policy (new proportional_policy) ;
evaluate_policy (fun requests frames_count processes_count -> new page_error_rate_control_policy requests frames_count processes_count delta_t 3 5 10) ;
evaluate_policy (fun requests frames_count processes_count -> new zone_model requests frames_count processes_count 12 3 ) ;
let file = open_out json_output_file
in fprintf file "data="; Basic.pretty_to_channel file (`List (List.rev !json_output))