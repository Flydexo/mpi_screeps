module DynArray = struct
  type 'a t = {mutable length:int;mutable maxlength:int;mutable array:'a array};;
  let get dyn i = dyn.array.(i)

  let set dyn i e = 
    dyn.array.(i) <- e

  let len dyn = dyn.length

  let append dyn e d = 
    if dyn.length = dyn.maxlength then begin
      dyn.maxlength <- dyn.maxlength*2;
      let new_array = Array.make 0 d in
      Array.blit dyn.array 0 new_array 0 dyn.maxlength;
      dyn.array <- new_array;
    end;
    set dyn dyn.length e;
    dyn.length <- dyn.length+1

  let pop dyn =
    dyn.length <- dyn.length-1;
    let r = get dyn dyn.length in
    if dyn.length <= dyn.maxlength/4 then begin
      dyn.maxlength <- dyn.maxlength/2;
      let new_array = Array.sub dyn.array 0 dyn.length in
      dyn.array <- new_array;
    end;
    r
  
end

module PQueue = struct
  type ('a, 'b) t = ('a*'b) DynArray.t;;

  let create length d = DynArray.{length=0;maxlength= length*2;array=(Array.make (length*2) d)}

  let swap q i j =
    let tmp = DynArray.get q i in
    DynArray.set q i (DynArray.get q j);
    DynArray.set q j tmp

  let rec percolate_up q i = 
    if i = 0 then ();
    let p = (i-1)/2 in
    let _,pp = DynArray.get q p in
    let _,pi = DynArray.get q i in 
    if pp > pi then begin
      swap q p i;
      percolate_up q p;
    end

  let min_q q i j = 
    let _,pi = DynArray.get q i in
    let _, pj = DynArray.get q j in
    if pi < pj then i
    else j

  let rec percolate_down q i = 
    if 2*i+1 >= DynArray.len q || 2*i+2 >= DynArray.len q then ();
    let min_child = min_q q (2*i+1) (2*i+2) in
    let _,pi = DynArray.get q i in
    let _,pc = DynArray.get q min_child in
    if pi > pc then begin
      swap q i min_child;
      percolate_down q min_child;
      ()
    end
    else ()

  let add q e d = 
    DynArray.append q e d;
    percolate_up q (DynArray.len q - 1)

  let extract_min q =
    swap q 0 (DynArray.len q - 1);
    let a,_ = DynArray.pop q in
    percolate_down q 0;
    a

end

type minion = {owner:int ; x:int ; y:int ; load:int ; hp:int ; capacity:int ; atk:int} ;;
type coord = {x:int;y:int};;

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

(* let floyd_warshall () =
  let lm = (Array.length map) in 
  let distances = Array.make_matrix (lm*lm) (lm*lm) infinity in 
  let paths = Array.make_matrix (lm*lm) (lm*lm) (-1,-1) in 
  for i = 0 to lm*lm-1 do
    let x = i/lm in
      let y = i mod lm in
      if y > 0 then (
        let t,_,_ =  map.(x).(y-1) in
        if t <> "W" then (
          distances.(i).(x*lm+(y-1)) <- 1.;
          paths.(i).(x*lm+(y-1)) <- (x, y-1)
        )
      );
      if x > 0 then (
        let t,_,_ =  map.(x-1).(y) in
        if t <> "W" then (
          distances.(i).((x-1)*lm+y) <- 1.;
          paths.(i).((x-1)*lm+y) <- (x-1, y)
        )
      );
      if y < lm-1 then (
        let t,_,_ =  map.(x).(y+1) in
        if t <> "W" then (
          distances.(i).(x*lm+(y+1)) <- 1.;
          paths.(i).(x*lm+(y+1)) <- (x, y+1)
        )
      );
      if x < lm-1 then (
        let t,_,_ =  map.(x+1).(y) in
        if t <> "W" then (
          distances.(i).((x+1)*lm+y) <- 1.; 
          paths.(i).((x+1)*lm+y) <- (x+1, y)
        )
      );
    for j = 0 to lm*lm-1 do 
      for k = 0 to lm*lm-1 do 
        if distances.(i).(k) +. distances.(k).(j) < distances.(i).(j) then begin
          distances.(i).(j) <- distances.(i).(k) +. distances.(k).(j);
          paths.(i).(j) <- paths.(i).(k);
        end
      done
    done
  done;
  (distances,paths)
in *)

let manhattan a b = (abs (a.x - b.x)) + (abs (a.y - b.y)) in

let rec a_star (source:coord) (destination:coord) = 
  let q = PQueue.create 10 ({x=-1;y=-1},infinity) in 
  if source = destination then source
  else 
    let t, _, _ = map.(source.x).(source.y) in
    if t = "W" then source
    else 
      PQueue.add q (source, (manhattan source destination));
      while DynArray.len q <> 0 do
        let x,y = PQueue.extract_min q in
        if x > 0 then
          if y > 0 then begin
            let t,_,_ = map.(x-1).(y-1) in
            if t <> "W" then
              PQueue.add q ({x=x-1;y=y-1}, manhattan ({x=x-1;y=y-1}) (destination))
            else ()
          end;
          else then
            let 
      done;

Array.iter (fun minion -> (
  if minion.load = minion.capacity then 
    move_towards minion baseX baseY
  else begin
    let distances = Array.init (Array.length map) (fun i -> Array.init (Array.length map.(0)) (fun j -> (abs (i - minion.x)) + (abs (j - minion.y)))) in
    (* let distances, paths = floyd_warshall () in *)
    let gains = Array.init (Array.length map) (fun i -> Array.init (Array.length map.(0)) (fun j -> 
      let t,r,_ = map.(i).(j) in
      if t == "W" then 0
      else min (minion.capacity - minion.load) r)) in 
    let lm = Array.length map in
    let ratio = Array.init (Array.length map) (fun i -> Array.init (Array.length map.(0)) (fun j -> (float_of_int (gains.(i).(j))) /. (float_of_int distances.(i).(j)+.1.))) in  
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
    (* for i = 0 to lm*lm-1 do
      for j = 0 to lm*lm-1 do
        let x,y = paths.(i).(j) in
        Printf.printf "x: %d y: %d\n" x y;
      done
    done; *)
    let next_x, next_y = paths.(lm*minion.x + minion.y).(!x*lm + !y) in
    Printf.printf "x: %d y:%d px: %d py: %d\n" !x !y next_x next_y;
    if !x = minion.x && !y = minion.y then extract minion
    else move_towards minion next_x next_y
  end
)) minions;

close_out ptrOut