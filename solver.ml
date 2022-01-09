type available = { loc : int * int; possible : int list }

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
type state = { problem : Model.problem; current_grid : int option Model.grid; curr_row : int; curr_column : int; curr_n: int }

let print_state (state : state) : unit =
  Model.print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of Model.solution | Unsolved of state | Fail of state

let initialize_state (problem : Model.problem) : state =
  { current_grid = Model.copy_grid problem.initial_grid; problem; curr_row = 0; curr_column = 0; curr_n = 1 }

let validate_state (state : state) : response =
    (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let unsolved =
      Array.exists (Array.exists Option.is_none) state.current_grid
    in
    if unsolved then match Model.is_valid_solution state.problem state.current_grid with
      | Some false -> Fail state
      | _ -> Unsolved state
    else
      let solution = Model.map_grid Option.get state.current_grid in
      match Model.is_valid_solution state.problem state.current_grid with
      | Some true -> Solved solution
      | _ -> Fail state

let rec branch_state (state : state) : (state * state) option =
  (* TODO: Pripravite funkcijo, ki v trenutnem stanju poišče hipotezo, glede katere
     se je treba odločiti. Če ta obstaja, stanje razveji na dve stanji:
     v prvem predpostavi, da hipoteza velja, v drugem pa ravno obratno.
     Če bo vaš algoritem najprej poizkusil prvo možnost, vam morda pri drugi
     za začetek ni treba zapravljati preveč časa, saj ne bo nujno prišla v poštev. *)
  let new_row_col curr_row curr_col = if curr_col < 8 then (curr_row, curr_col + 1) else (curr_row + 1, 0) in
  let (new_row, new_column) = new_row_col state.curr_row state.curr_column in 
  let new_grid = Model.copy_grid state.current_grid in
  if (state.curr_n > 9) || (state.curr_row > 9) then None else
    match state.current_grid.(state.curr_row).(state.curr_column) with
    | Some n -> branch_state { current_grid = new_grid; problem = state.problem; curr_row = new_row; curr_column = new_column; curr_n = 1 }
    | None -> Some 
    ({ current_grid = Model.copy_grid state.current_grid; problem = state.problem; curr_row = state.curr_row; curr_column = state.curr_column; curr_n = (state.curr_n + 1) },
    { current_grid = (new_grid.(state.curr_row).(state.curr_column) <- Some state.curr_n; new_grid); problem = state.problem; curr_row = new_row; curr_column = new_column; curr_n = 1 })

(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state (state : state) =
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  (* TODO: na tej točki je stanje smiselno počistiti in zožiti možne rešitve *)
  match validate_state state with
  | Solved solution ->
      (* če smo našli rešitev, končamo *)
      Some solution
  | Fail fail ->
      (* prav tako končamo, če smo odkrili, da rešitev ni *)
      None
  | Unsolved state' ->
      (* če še nismo končali, raziščemo stanje, v katerem smo končali *)
      explore_state state'

and explore_state (state : state) =
  (* pri raziskovanju najprej pogledamo, ali lahko trenutno stanje razvejimo *)
  match branch_state state with
  | None ->
      (* če stanja ne moremo razvejiti, ga ne moremo raziskati *)
      None
  | Some (st1, st2) -> (
      (* če stanje lahko razvejimo na dve možnosti, poizkusimo prvo *)
      match solve_state st1 with
      | Some solution ->
          (* če prva možnost vodi do rešitve, do nje vodi tudi prvotno stanje *)
          Some solution
      | None ->
          (* če prva možnost ne vodi do rešitve, raziščemo še drugo možnost *)
          solve_state st2 )

let solve_problem (problem : Model.problem) =
  problem |> initialize_state |> solve_state
