open Obc_main


let rec  int_to_nat n =
if n<=0 then O else S (int_to_nat (n-1));;

let rec nat_to_int n=
match n with 
O->0
|S x->1+nat_to_int x;;

let  bool_of_string b =
match String.lowercase_ascii b with 
"true"|"T"->true|
"false"|"F"->false
|_->failwith("Invalid bool "^b);;

let a_health_bool b= if b then Good_acq else Bad_acq;;
let b_health_bool b=if b then Good_bs else Bad_bs;;
let sig_condition b=if b then Good_s else Bad_s;;
 



let output_structure (dim_count:int) (input_values:string) :unit_output=  
let new_list=String.split_on_char '|' input_values|>List.map String.trim in

match new_list with 
[]->failwith "Empty sensor data"
| id :: dim_and_health ->
    let unit_id = int_to_nat (int_of_string id) in
    let rec dim_part (axis_id : int) (count : int) (lst : string list) (acc : snsr_output_axis list) : (snsr_output_axis list * string list) =
      if count = 0 then (List.rev acc, lst)
      else
        match lst with
        | value :: flag :: rest ->
            let snsr_sig : snsr_signal =
              {
                sig_val = int_to_nat (int_of_string value);
                sig_type = sig_condition (bool_of_string flag);
              }
            in
            let snsr_op : snsr_output =
              { snsr_reading = snsr_sig; snsr_id = unit_id }
            in
            let snsr_output_ax : snsr_output_axis =
              { a_id = int_to_nat axis_id; snsr_out = snsr_op }
            in
            dim_part (axis_id + 1) (count - 1) rest (snsr_output_ax :: acc)
        | _ -> failwith "Not enough data given."
    in
    let snsr_reading, health_part =
      dim_part 1 dim_count dim_and_health []
    in
    (match health_part with
    | h1 :: h2 :: [] ->
        {
          uid = unit_id;
          readings = snsr_reading;
          a_health = a_health_bool (bool_of_string h1);
          b_health = b_health_bool (bool_of_string h2);
        }
    | _ -> failwith "Expected a_health and b_health flags");;




let read_file (filename : string):int*int*unit_output list list =
  let ic = open_in filename in
  let rec read_lines acc =
    match input_line ic with
    | line -> read_lines (line :: acc)
    | exception End_of_file -> List.rev acc
  in
  let lines = read_lines [] in
  close_in ic;
  match lines with
  | sensor_line :: dim_line :: rest_lines ->
      let sensor_count = int_of_string sensor_line in
      let dim_count = int_of_string dim_line in
      
      let data_str = String.concat "\n" rest_lines in

      (* Split into cycle blocks separated by '#' *)
      let cycle_blocks =
        data_str
        |> String.split_on_char '#'
        |> List.map String.trim
        |> List.filter (fun s -> s <> "")
      in

      (* For each block: split into sensor values separated by ';' *)
      let cycles =
        List.map
          (fun block ->
             block
             |> String.split_on_char ';'
             |> List.map String.trim
             |> List.filter (fun s -> s <> "")
             |> List.map( output_structure dim_count)
          )
          cycle_blocks
      in

      (sensor_count, dim_count, cycles)

  | _ ->
      failwith "File format error: expected at least 2 header lines";;



