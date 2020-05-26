
open Evaluator;;
open Utility;;
open Requests;;

let preset=1;;
let ()=
match preset with
0->
   evaluate_policies 
   { page_error_rate_control_policy_delta_t=30;
   page_error_rate_control_policy_lower=3;
   page_error_rate_control_policy_upper=5;
   page_error_rate_control_policy_stop=10;
   page_error_rate_control_policy_resume=5;
   zone_model_delta_t=20;
   zone_model_c=10;
   
   frames_count=90;
   processes_count=5;
   process_sizes={start=10; end_=100};
   
   make_plots=true }
   { phases_count={start=12; end_=20};
   phase_range=3;
   phases_distance={start=5; end_=10 };
   requests_per_phase={start=2; end_=20 }; }
| 1->
   evaluate_policies 
   { page_error_rate_control_policy_delta_t=60;
   page_error_rate_control_policy_lower=3;
   page_error_rate_control_policy_upper=5;
   page_error_rate_control_policy_stop=10;
   page_error_rate_control_policy_resume=5;
   zone_model_delta_t=40;
   zone_model_c=20;
   
   frames_count=200;
   processes_count=10;
   process_sizes={start=10; end_=100};
   
   make_plots=true }
   { phases_count={start=30; end_=50};
   phase_range=3;
   phases_distance={start=10; end_=30 };
   requests_per_phase={start=2; end_=20 }; }
| 2->
   evaluate_policies 
   { page_error_rate_control_policy_delta_t=30;
   page_error_rate_control_policy_lower=3;
   page_error_rate_control_policy_upper=5;
   page_error_rate_control_policy_stop=10;
   page_error_rate_control_policy_resume=5;
   zone_model_delta_t=20;
   zone_model_c=10;
   
   frames_count=200;
   processes_count=20;
   process_sizes={start=50; end_=200};
   
   make_plots=false }
   { phases_count={start=12; end_=20};
   phase_range=3;
   phases_distance={start=5; end_=10 };
   requests_per_phase={start=2; end_=20 }; }
| _-> Printf.printf "Unknown preset\n"