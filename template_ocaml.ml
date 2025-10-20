type minion = {owner:int ; x:int ; y:int ; load:int ; hp:int ; capacity:int ; atk:int} ;;

let ptr = open_in "mapData.txt" in

let mapSize = input_line ptr |> int_of_string in
let (map : (string * int * int) array array) = Array.init mapSize (fun i ->
  input_line ptr
    |> String.split_on_char ' '
    |> List.map (String.split_on_char ',')
    |> List.map (function | (t::r::m::[]) -> (t,int_of_string r,int_of_string m) | _ -> failwith "invalid 1")
    |> Array.of_list
) in
let nMinions = input_line ptr |> int_of_string in
let minions = Array.init nMinions (fun i ->
  input_line ptr
    |> String.split_on_char ','
    |> List.map int_of_string
    |> (function | (o::x::y::l::h::c::a::[]) -> {owner=o ; x=x ; y=y ; load=l ; hp=h ; capacity=c ; atk=a} | _ -> failwith "invalid 2")
) in
let myID, myResources = input_line ptr |> String.split_on_char ' ' |> List.map int_of_string |> (function | (a::b::[]) -> a,b | _ -> failwith "invalid 3") in
let baseX, baseY = input_line ptr |> String.split_on_char ' ' |> List.map int_of_string |> (function | (a::b::[]) -> a,b | _ -> failwith "invalid 4") in
let curTurn,maxTurn = input_line ptr |> String.split_on_char ' ' |> List.map int_of_string |> (function | (a::b::[]) -> a,b | _ -> failwith "invalid 5") in
(* let randomEvents = input_line ptr
  |> String.split_on_char ' '
  |> List.map (String.split_on_char ',')
  |> List.map (function |a::b::[] -> (int_of_string a, int_of_string b) | _ -> (-1, -1))
  |> List.filter (fun (a,b) -> a <> -1 || b <> -1)
  |> Array.of_list 
in *)

close_in ptr;

(*
TYPE_MUR = 'W'
TYPE_NORMAL = 'R'
TYPE_DASH = 'D'
TYPE_SHIELD = 'S'
TYPE_FORCE = 'F'
TYPE_MIDAS = 'M'
TYPE_VITESSE = 'P'
*)

let ptrOut = open_out "answer.txt" in

let execute_instruction instruction fmt =
  Printf.fprintf ptrOut instruction fmt;
in

let spawn_minion (hp:int) (size:int) (attack:int) =
  if 2*hp + 2*size +attack > 25 then failwith "Stats exceed 25"
  else execute_instruction "CREATE %d %d %d\n" hp size attack;
in

let move_minion (x: int) (y:int) (xd: int) (yd: int) =
  execute_instruction "%d %d %d %d\n" x y xd yd
in

let spawn_initial_gechar () =
  if curTurn = 0 then (spawn_minion 1 9 0)
  (* else if curTurn = 1 then (move_minion baseX baseY (baseX+1) baseY; spawn_minion 1 9 0; )
  else if curTurn = 2 then move_minion baseX baseY baseX (baseY+1) *)
in

if curTurn = 0 then begin
  spawn_initial_gechar ();
end;

let extract (m:minion) = move_minion m.x m.y m.x m.y in

let sign a = if a > 0 then 1 else -1 in

let move_towards (m:minion) x y =
  if x <> m.x then move_minion m.x m.y (m.x + sign (x - m.x)) m.y
  else move_minion m.x m.y m.x (m.y + sign (y - m.y));
in

let floyd_warshall sx sy =
  let lm = (Array.length map) in 
  let distances = Array.make_matrix (lm*lm) (lm*lm) infinity in 
  let paths = Array.make_matrix (lm*lm) (lm*lm) (-1,-1) in 
  for j = 0 to lm*lm-1 do
    let x = j/lm in
    let y = j mod lm in
    if y > 0 then (
      let t,_,_ =  map.(x).(y-1) in
      if t <> "W" then distances.(j).(x*lm+(y-1)) <- 1.;
    );
    if x > 0 then (
      let t,_,_ =  map.(x-1).(y) in
      if t <> "W" then distances.(j).((x-1)*lm+y) <- 1.;
    );
    if y < lm-1 then (
      let t,_,_ =  map.(x).(y+1) in
      if t <> "W" then distances.(j).(x*lm+(y+1)) <- 1.; 
    );
    if x < lm-1 then (
      let t,_,_ =  map.(x+1).(y) in
      if t <> "W" then distances.(j).((x+1)*lm+y) <- 1.; 
    );
    for k = 0 to lm*lm-1 do 
      if distances.(lm*sx+sy).(k) +. distances.(k).(j) < distances.(lm*sx + sy).(j) then begin
        distances.(lm*sx + sy).(j) <- distances.(lm*sx+sy).(k) +. distances.(k).(j);
        paths.(lm*sx + sy).(j) <- if paths.(lm*sx + sy).(j) = (-1,-1) then (
           Printf.printf "updated path with %d %d\n" (j/lm) (j mod lm);
           (j / lm, j mod lm)
        ) else paths.(lm*sx + sy).(j)
      end
    done
  done; 
  (distances,paths)
in

Array.iter (fun minion -> (
  if minion.load = minion.capacity then 
    move_towards minion baseX baseY
  else begin
    (* let distances = Array.init (Array.length map) (fun i -> Array.init (Array.length map.(0)) (fun j -> (abs (i - minion.x)) + (abs (j - minion.y)))) in *)
    let distances, paths = floyd_warshall minion.x minion.y in
    let gains = Array.init (Array.length map) (fun i -> Array.init (Array.length map.(0)) (fun j -> 
      let t,r,_ = map.(i).(j) in
      if t == "W" then 0
      else min (minion.capacity - minion.load) r)) in 
    let lm = Array.length map in
    let ratio = Array.init (Array.length map) (fun i -> Array.init (Array.length map.(0)) (fun j -> (float_of_int (gains.(i).(j))) /. (distances.(minion.x*lm+minion.y).(i*lm+j)+.1.))) in  
    let x = ref 0 in
    let y = ref 0 in
    let m = ref (-1.0 *. infinity) in
    for i = 0 to lm-1 do
      for j = 0 to lm-1 do
        if ratio.(i).(j) > !m then begin
          m := ratio.(i).(j);
          x := i;
          y := j;
        end;
      done; 
    done;
    let _,rd,_ = map.(!x).(!y) in
    let next_x, next_y = paths.(lm*minion.x + minion.y).(!x*lm + !y) in
    Printf.printf "x: %d y:%d px: %d py: %d\n" !x !y next_x next_y;
    if !x = minion.x && !y = minion.y then extract minion
    else move_towards minion next_x next_y
  end
)) minions;

close_out ptrOut