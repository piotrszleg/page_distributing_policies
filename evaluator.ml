open EqualPolicy;;
open ProportionalPolicy;;
open PageErrorRateControlPolicy;;
open Requests;;
open ZoneModel;;
open Yojson;;
open Printf;;
open Utility;;

type evaluation_settings={
  page_error_rate_control_policy_delta_t:int;
  page_error_rate_control_policy_lower:int;
  page_error_rate_control_policy_upper:int;
  page_error_rate_control_policy_stop:int;
  page_error_rate_control_policy_resume:int;
  zone_model_delta_t:int;
  zone_model_c:int;
  
  frames_count:int;
  processes_count:int;
  process_sizes:range;

  make_plots:bool;
};;

let json_output_file="output/data.js";;

let make_header text=`Assoc [
  "type", `String "header";
  "text", `String text
];; 
let requests_as_json_points requests=
  List.map
  (fun request->`Assoc [
      "x", `Int request.time;
      "y", `Int request.process_index
  ])
  requests;;
let requests_plot requests=`Assoc [
  ("type", `String "scatter");
  ("xAxis", `String "time");
  ("yAxis", `String "process");
  ("data", `List [
      `Assoc [
        "label", `String "request"; 
        "borderColor", `String "#99c2ff";
        "backgroundColor", `String "#99c2ff";
        "data", `List (requests_as_json_points requests)]] )
];;
let evaluate_policies evaluation_settings requests_settings=
  let json_output=ref ([]:Basic.t list)
  in let process_sizes=random_array evaluation_settings.process_sizes evaluation_settings.processes_count
  in let processes_count=evaluation_settings.processes_count
  in let frames_count=evaluation_settings.frames_count
  in let requests=global_requests 123 processes_count process_sizes requests_settings
  in 
     if evaluation_settings.make_plots 
     then json_output:=(requests_plot requests)::(make_header "requests")::!json_output;
     let evaluate_policy policy=
        policy#run ;
        policy#print ;
        json_output:=(make_header policy#name)::!json_output;
        if evaluation_settings.make_plots
        then json_output:=policy#page_faults_plot_json::policy#frames_count_plot_json::!json_output;
        json_output:=policy#table_json::!json_output;
    printf "\n"
  in
    evaluate_policy (new equal_policy requests frames_count processes_count) ;
    evaluate_policy (new proportional_policy requests frames_count processes_count process_sizes) ;
    evaluate_policy (
      new page_error_rate_control_policy 
      requests frames_count processes_count process_sizes 
      evaluation_settings.page_error_rate_control_policy_delta_t 
      evaluation_settings.page_error_rate_control_policy_lower 
      evaluation_settings.page_error_rate_control_policy_upper
      evaluation_settings.page_error_rate_control_policy_stop
      evaluation_settings.page_error_rate_control_policy_resume) ;
    evaluate_policy (new zone_model 
      requests frames_count processes_count process_sizes 
      evaluation_settings.zone_model_delta_t evaluation_settings.zone_model_c ) ;
  let file = open_out json_output_file
  in fprintf file "data="; Basic.pretty_to_channel file (`List (List.rev !json_output))
