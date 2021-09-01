(* Note: You may introduce new code anywhere in this file. *) 
open Adventure 

(** Raised when the player's answer is of type answer but is incorrect. *)
exception IncorrectAnswer

(** Raised when the player's answer is not type answer or type quit. *)
exception IllegalAnswer

(** Module StringMap stores a map of strings. *)
module StringMap = 
  Map.Make(struct type t = string let compare = Pervasives.compare end);;

(** type t represents current game state by recording the current room,
    the visited rooms, unlocked_rms, identifiers of keys the player has, score, 
    and a room map that stores the items dropped.
*)
type t = {
  current: room_id;
  visited_rms: room_id list;
  unlocked_rms: room_id list;
  keys: string list; 
  score: int;
  rooms_map: (string list) StringMap.t;
}

(** [init_state adv] is the initial state of the game 
    when playing adventure [adv]. 
    In that state the adventurer is currently located in the starting room,
    and they have visited only that room.
    Example: init_state (from_json base_case_2.json) returns 
    {current: "room a"; visited_rms: ["room a"]}
    Requires: adv is of type Adventure.t
*)
let init_state adv =
  let start = start_room adv in 
  {current = start; visited_rms = start::[]; unlocked_rms=[start];
   keys = [room_key adv start]; score = 0; rooms_map = StringMap.empty}


(** [current_room_id st] is the identifier of the room in which the adventurer
    currently is located in state [st]. 
    Example: current_room_id (init_state (from_json base_case_2.json)) 
    returns "room a"
    Requires: st is of type State.t*)
let current_room_id st =
  st.current

(** [visited st] is a set-like list of the room identifiers the adventurer has 
    visited in state [st]. The adventurer has visited a room [rm] if their
    current room location is or has ever been [rm].
    Example: visited (init_state (from_json base_case_2.json)) 
    returns ["room a"]
    Requires: st is of type State.t*)
let visited st =
  st.visited_rms 

(** The type representing the result of an attempted movement. *)
type result = Legal of t | Illegal

(** [next_state ex st adv] returns a Legal result which contains the 
    object of type t containing a record of the new room [r] the exit leads to
    which is stored in the field current as well as room [r] added to the 
    visited_rms field.
    Requires: [ex] is an exit name;
              [st] is a abstract type representing the current game state;
              [adv] is the abstract type representing adventure. *)
let next_state ex st adv: result = 
  let next_r = next_room adv st.current ex in
  Legal {current = next_r; keys = st.keys; score = st.score;
         unlocked_rms = st.unlocked_rms;
         rooms_map = st.rooms_map;
         visited_rms = if (List.mem next_r st.visited_rms) then st.visited_rms 
           else next_r:: st.visited_rms}

(** [unlock adv room st] returns 
    a [Legal] result if [room] is already unlocked;
    a [Legal] result and unlocks [room] 
    if [room] is an exit of the current room and the player has the key to it;
    an [Illegal] result otherwise.
    Requires: [room] is an room identifier;
              [st] is a abstract type representing the current game state;
              [adv] is the abstract type representing adventure. *)
let unlock adv room st : result = 
  if (List.mem room st.unlocked_rms) then Legal st (*state unchanged*)
  else 
  if (List.mem room (next_rooms adv st.current)) then
    if (List.mem (room_key adv room) st.keys) then
      Legal {current = st.current; visited_rms = st.visited_rms; keys = st.keys;
             score = st.score; unlocked_rms = room::st.unlocked_rms; 
             rooms_map=st.rooms_map}
    else Illegal (*don't have the key*)
  else Illegal

(** [lock adv room st] returns 
    a [Legal] result if [room] is already locked;
    a [Legal] result and locks [room] 
    if [room] is an exit of the current room and the player has the key to it;
    an [Illegal] result otherwise. 
    Requires: [room] is an room identifier;
              [st] is a abstract type representing the current game state;
              [adv] is the abstract type representing adventure. *)
let lock adv room st : result =
  if(List.mem room (next_rooms adv st.current)) then (*room is next doors*)
    if( not (List.mem room st.unlocked_rms) ) then Legal st (*room is already locked*)
    else 
    if(List.mem (room_key adv room) st.keys) then 
      let new_rooms = List.filter (fun a -> (a <> room)) st.unlocked_rms in
      Legal {current = st.current; visited_rms = st.visited_rms; keys = st.keys;
             score = st.score; unlocked_rms = new_rooms; rooms_map=st.rooms_map}
    else Illegal
  else Illegal (*room is not next door can't lock it*)

(** [add_score st amt] adds [amt] to the player's state.
    Requires: [st] is a abstract type representing the current game state;
              [amt: int] is the score that needs to be added. *)
let add_score st amt = {current=st.current; visited_rms = st.visited_rms;
                        unlocked_rms = st.unlocked_rms; keys = st.keys; 
                        score = st.score + amt; rooms_map = st.rooms_map}

(* [get_inventory st] returns the keys that the player currently has. 
   Requires: [st] is a abstract type representing the current game state.*)
let get_inventory st = st.keys

(** [print_inventory lst] nicely prints the list of keys [lst].
    Example: print_inventory [] returns "none";
    print_inventory ["a"; "b"] returns "a, b".*)
let rec print_inventory lst = 
  match lst with
  |[] -> "none"
  |h::[] -> h
  |h::t -> h ^ ", " ^ (print_inventory t)

(** [add_key adv st key] adds [key] to the player's state.
    Requires: 
              [adv] is the abstract type representing adventure. 
              [st] is a abstract type representing the current game state;
              [key: string] is the identifier of the key that needs to be added. *)
let add_key adv st key = if (List.mem key st.keys) then st
  else {current = st.current; visited_rms = st.visited_rms; keys = key::st.keys;
        score = st.score + (room_score adv st.current + 1) * 5;
        unlocked_rms = st.unlocked_rms; rooms_map=st.rooms_map} 

(** [drop st item] returns 
    [Legal] result and removes [item] from the inventory list 
      if [item] is in the inventory of the player;
    [Illegal] otherwise. 
    Requires: 
              [st] is a abstract type representing the current game state.
              [item: string] is the identifier of the key that needs to be removed.*)
let drop st item : result = 
  if (List.mem item (get_inventory st)) then (*have the key*)
    let new_keys = (List.filter (fun a -> (a <> item)) st.keys) in
    if(StringMap.mem st.current st.rooms_map) then
      let item_list = StringMap.find st.current st.rooms_map in
      let new_map = StringMap.add st.current (item::item_list) st.rooms_map in
      Legal {current=st.current; visited_rms = st.visited_rms;
             unlocked_rms = st.unlocked_rms; keys = new_keys; score = st.score;
             rooms_map=new_map}
    else
      let new_map = StringMap.add st.current [item] st.rooms_map in
      Legal {current=st.current; visited_rms = st.visited_rms;
             unlocked_rms = st.unlocked_rms; keys = new_keys; score = st.score;
             rooms_map=new_map}
  else Illegal


(** [take st item] returns 
    [Legal] result and adds [item] from the inventory list 
      if [item] is in the inventory of the player;
    [Illegal] otherwise. 
    Requires: 
              [st] is a abstract type representing the current game state.
              [item: string] is the identifier of the key that needs to be removed.*)
let take st item : result =
  if(StringMap.mem st.current st.rooms_map) then
    let item_list = StringMap.find st.current st.rooms_map in
    if(List.mem item item_list) then (*remove*)
      let new_items = (List.filter (fun a -> (a <> item)) item_list) in
      let new_map = StringMap.add st.current new_items st.rooms_map in 
      let new_keys = if(List.mem item st.keys) then st.keys 
        else item::st.keys in
      Legal {current=st.current; visited_rms = st.visited_rms;
             unlocked_rms = st.unlocked_rms; keys = new_keys; score = st.score;
             rooms_map=new_map}
    else
      Illegal
  else
    Illegal

(** [go ex adv st] is [r] if attempting to go through exit [ex] in state 
    [st] and adventure [adv] results in [r].  If [ex] is an exit from the 
    adventurer's current room, then [r] is [Legal st'], where in [st'] the 
    adventurer is now located in the room to which [ex] leads.  Otherwise, 
    the result is [Illegal]. The score of the room is added to player's
    overall score. 
    Requires: [ex] is an exit name;
              [st] is a abstract type representing the current game state;
              [adv] is the abstract type representing adventure. *)
let go ex adv st =
  let exit_list = exits adv (current_room_id st) in 
  if exit_list = [] then Illegal 
  else if (List.mem ex exit_list) then 
    let room = next_room adv st.current ex in
    if (not (List.mem room st.unlocked_rms)) then Illegal else 
    if (List.mem room (visited st)) then (next_state ex st adv)
    else 
      let new_st = add_score st (room_score adv room) in
      (next_state ex new_st adv) 
  else Illegal 

(** Exception raised when the exit is not present from the current room *)
exception Invalidexit

(** [get_t] applies go to [ex adv st] and then returns the new state [t]
    if the result is legal else it raises an Invalidexit exception. 
    Requires: [ex] is an exit name;
              [st] is a abstract type representing the current game state;
              [adv] is the abstract type representing adventure. 
    Raises: [Invalidexit] when exit is not present from the current room.*)
let get_t ex adv st: t = 
  let r = go ex adv st in 
  match r with 
  | Legal(t) -> t
  | Illegal -> raise Invalidexit

(** [get_score st] returns the current score of the player. 
    Requires: [st] is a abstract type representing the current game state;
*)
let get_score st = st.score
