open Obc_main
open Read_file

let rec  int_to_nat n =
if n<=0 then O else S (int_to_nat (n-1));;

let rec nat_to_int n=
match n with 
O->0
|S x->1+nat_to_int x;;

(*function to create an initial sensor signal*)
let snsr_sig_init():snsr_signal={

  sig_val=int_to_nat 0;
  sig_type=Good_s;
};;

 (*function to create sensor output*)
let snsr_out s_id=
{
 
  snsr_reading=snsr_sig_init();
  snsr_id=s_id;
} ;;

(*function to create sensor data*)
let snr_data s_id=
{
 
s_output=snsr_out (int_to_nat s_id);
s_status={iso_status=Not_isolated;
 miscomp_status=Not_miscomparing;
 risky_count=O}
} ;;




(*function to build sensor data list*)
let build_s_data_list n d=
List.init n (fun s_id->
  List.init d(fun d1->
    snr_data (s_id+1)
    )
  )|>List.flatten ;;

(*function to build voter state*)
let build_voter_state (n:int) (d:int):voter_state=
let dl=build_s_data_list n d in
match dl with 
[]->failwith "empty sensor data list"
|f::_->{
  s_data_lst=dl;
  voter_output=snsr_out (int_to_nat 1);
  voter_validity=Valid;
  output_age=int_to_nat 0;
  presrvd_data=f
};;

(*function to build voter state unit*)
let build_v_state_unit (n:int) (d:int) (ax_id:int):voter_state_unit={
  axis=int_to_nat ax_id;
  delta=int_to_nat 2;
  vs=build_voter_state n d


};;

(*function to make an initial obc*)
let obc_init n_sensor dim_count:obc=
  let v_l=List.init dim_count(fun i->let ax_id=i+1 in
  build_v_state_unit n_sensor dim_count ax_id 
  ) in
{
  v_lst=v_l;
  obc_risk_count=int_to_nat 0;
  switch_flag=Valid_obc
};;


  (*function to print validity status*)
let print_validity_flag h=match h with 
Valid_obc->print_endline" Validity Status: Valid OBC\n"|
Invalid_obc->print_endline "Validity Status: Invalid OBC\n";;




(*function to print output age*)
let print_op_age n=
Printf.printf "Output age: %d\n" n;;



(*function to print snsr_output*)
let print_snsr_op (sp:snsr_output)=
print_endline"Sensor Output\n";
Printf.printf "  Sensor ID: %d \n"(nat_to_int sp.snsr_id) ;
Printf.printf "  Signal Value: %d\n"(nat_to_int sp.snsr_reading.sig_val);
match sp.snsr_reading.sig_type with 
Good_s->print_endline "  Signal health: Good \n"|
Bad_s->print_endline "  Signal health: Bad\n" ;;




(*function to print snsr_data*)
let  print_snsr_data (pvd:snsr_data)=
print_snsr_op (pvd.s_output);
(*printing snsr_status*)
(match pvd.s_status.iso_status with
Isolated->print_endline "  Isolation status: Isolated\n"|
Not_isolated->print_endline "  Isolation status: Not Isolated\n" );

(match pvd.s_status.miscomp_status with 
Miscomparing->print_endline "  Miscomparing status: Miscomparing \n "|
Not_miscomparing->print_endline "  Miscomparing status: Not miscomparing\n"|
Maybe_miscomparing->print_endline "  Miscomparing status: Maybe miscomapring\n");
Printf.printf "  Risky count: %d\n" (nat_to_int pvd.s_status.risky_count);
print_endline"   ---------\n";;





(*function to print sensor data list*)
let print_snsr_data_list (d:snsr_data list)=
print_endline"   ----------\n";
List.iter print_snsr_data d;;



(*function to print validity*)
let print_v_valid v =
  match v with 
  Valid->print_endline " Validity Status: Valid\n"|
  Un_id->print_endline " Validity Status: Un_id\n"|
  Not_valid->print_endline " Validity Status: Not_valid\n";;

 


(*function to print voter state*)
let print_voter_state  (v:voter_state)=
print_v_valid v.voter_validity;
print_op_age (nat_to_int v.output_age);
print_snsr_op v.voter_output;
print_snsr_data v.presrvd_data;
print_snsr_data_list v.s_data_lst;;




(*function to print voter state unit *)
let print_voter_state_unit (vsa:voter_state_unit)=
Printf.printf " Axis ID: %d\n"(nat_to_int vsa.axis);
Printf.printf " Delta: %d\n" (nat_to_int vsa.delta);
print_voter_state vsa.vs;;



(*function to print voter state unit list*)
let print_voter_state_unit_list (vl:voter_state_unit list)=
List.iter print_voter_state_unit vl;;

(*function to print obc*)
let print_obc (o:obc)=
print_validity_flag o.switch_flag;
Printf.printf"OBC Risk Count: %d\n"(nat_to_int o.obc_risk_count);
print_endline"";
print_endline "Voter State Unit:\n ";
print_voter_state_unit_list o.v_lst;;





(*---------------------------------------------------------------------------------------------------------------------------------------------------*)


let print_validity_flag_new h=match h with 
Valid_obc->Printf.printf"%-2s""V  "|
Invalid_obc->Printf.printf "%-2s""IV  ";;


let print_op_age_new n=
Printf.printf "%2d| " n;;

let print_snsr_op_new (sp:snsr_output)=
Printf.printf "%d | "(nat_to_int sp.snsr_id) ;
Printf.printf "%2d | "(nat_to_int sp.snsr_reading.sig_val);
match sp.snsr_reading.sig_type with 
Good_s->Printf.printf "G | "|
Bad_s->Printf.printf"B | " ;;



let  print_snsr_data_new  (pvd:snsr_data)=
print_snsr_op_new (pvd.s_output);
(*printing snsr_status*)
(match pvd.s_status.iso_status with
Isolated->Printf.printf"%-2s""I | "|
Not_isolated->Printf.printf"%-2s" "NI| " );

(match pvd.s_status.miscomp_status with 
Miscomparing->Printf.printf"%-2s""M | "|
Not_miscomparing->Printf.printf "%-2s""NM| "|
Maybe_miscomparing->Printf.printf"%-2s""MM| ");
Printf.printf "%d| " (nat_to_int pvd.s_status.risky_count);;


let print_snsr_data_list_new (d:snsr_data list)=
List.iter print_snsr_data_new d;;

 let print_v_valid_new v =
  match v with 
  Valid->Printf.printf"%-2s""V | "|
  Un_id->Printf.printf "%-2s""UI| "|
  Not_valid->Printf.printf "%-2s""NV| ";;

let print_voter_state_new (v:voter_state)=
print_v_valid_new v.voter_validity;
print_op_age_new (nat_to_int v.output_age);
print_snsr_op_new v.voter_output;
print_snsr_data_list_new v.s_data_lst;;


let print_voter_state_unit_new  (vsa:voter_state_unit)=
Printf.printf " %d |"(nat_to_int vsa.axis);
print_voter_state_new vsa.vs;
print_endline"";;

let print_voter_state_unit_list_new (vl:voter_state_unit list)=
List.iter print_voter_state_unit_new vl;;

let print_new_obc (o:obc)=
print_endline"";
print_validity_flag_new o.switch_flag;
Printf.printf"| %d\n"(nat_to_int o.obc_risk_count);
print_voter_state_unit_list_new o.v_lst;;






(*function to apply obc_main on unit_output list*)
let rec obc_cycles old unit_list =
  match unit_list with
  | [] -> old                                 
  | cycle :: rest ->

      let res = obc_main old cycle in  
      print_new_obc res ;
        print_endline"";   
      print_endline"";
      print_endline "";
      flush stdout;         
      obc_cycles res rest ;;
    
let ()=
  print_endline "Enter the input filename:";
  let filename = read_line () in
let (n,d,u_list)=read_file filename in
print_endline "Validity Status Obc_Risky_Count Axis_ID Voter_Validity Output_Age  Voter_output[Snsr_ID Sig_Val Sig_Health] Voter_s_data_list[S_ID Sig_Val Sig_Health Iso_status Miscomp_status Risky_Count]";
ignore(obc_cycles (obc_init n d) u_list);;



