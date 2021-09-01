(** 
   Representation of dynamic adventure state.

   This module represents the state of an adventure as it is being played,
   including the adventurer's current room, the rooms that have been visited,
   and functions that cause the state to change.
*)

(* You are free to add more code here. *)

(**********************************************************************
 * DO NOT CHANGE THIS CODE
 * It is part of the interface the course staff will use to test your 
 * submission.
*)

(** The abstract type of values representing the game state. *)
type t 

(** [init_state a] is the initial state of the game when playing adventure [a]. 
    In that state the adventurer is currently located in the starting room,
    and they have visited only that room. *)
val init_state : Adventure.t -> t

(** [current_room_id st] is the identifier of the room in which the adventurer
    currently is located in state [st]. *)
val current_room_id : t -> string

(** [visited st] is a set-like list of the room identifiers the adventurer has 
    visited in state [st]. The adventurer has visited a room [rm] if their
    current room location is or has ever been [rm]. *)
val visited : t -> string list

(** The type representing the result of an attempted movement. *)
type result = Legal of t | Illegal

(** [go exit adv st] is [r] if attempting to go through exit [exit] in state 
    [st] and adventure [adv] results in [r].  If [exit] is an exit from the 
    adventurer's current room, then [r] is [Legal st'], where in [st'] the 
    adventurer is now located in the room to which [exit] leads.  Otherwise, 
    the result is [Illegal]. 
    Effects: none.  [go] is not permitted to do any printing. *)
val go : Adventure.exit_name -> Adventure.t -> t -> result

(* END DO NOT CHANGE
 **********************************************************************)

(* You are free to add more code here. *)

(** Exception raised when the exit is not present from the current room *)
exception Invalidexit

(** [get_t] applies go to [ex adv st] and then returns the new state [t]
    if the result is legal else it raises an Invalidexit exception. 
    Requires: [ex] is an exit name;
              [st] is a abstract type representing the current game state;
              [adv] is the abstract type representing adventure. 
    Raises: [Invalidexit] when exit is not present from the current room.*)
val get_t : Adventure.exit_name -> Adventure.t -> t -> t  

(** [unlock adv room st] returns 
    a [Legal] result if [room] is already unlocked;
    a [Legal] result and unlocks [room] 
    if [room] is an exit of the current room and the player has the key to it;
    an [Illegal] result otherwise.
    Requires: [room] is an room identifier;
              [st] is a abstract type representing the current game state;
              [adv] is the abstract type representing adventure. *)
val unlock : Adventure.t -> Adventure.room_id -> t -> result

(** [lock adv room st] returns 
    a [Legal] result if [room] is already locked;
    a [Legal] result and locks [room] 
    if [room] is an exit of the current room and the player has the key to it;
    an [Illegal] result otherwise. 
    Requires: [room] is an room identifier;
              [st] is a abstract type representing the current game state;
              [adv] is the abstract type representing adventure. *)
val lock : Adventure.t -> Adventure.room_id -> t -> result

(** [add_score st amt] adds [amt] to the player's state.
    Requires: [st] is a abstract type representing the current game state;
              [amt: int] is the score that needs to be added. *)
val add_score : t -> int -> t

(* [get_inventory st] returns the keys that the player currently has. 
   Requires: [st] is a abstract type representing the current game state.*)
val get_inventory: t -> string list 

(** [get_score st] is gets the current score of state [st].*)
val get_score : t -> int

(** [print_inventory lst] nicely prints the list of keys [lst].
    Example: print_inventory [] returns "none";
    print_inventory ["a"; "b"] returns "a, b".*)
val print_inventory: string list -> string

(** [add_key adv st key] adds key [key] to new state of adventure [adv] if the 
    key is not in the inventory, else return current state [st]*)
val add_key: Adventure.t -> t -> string -> t

(** [drop st item] returns 
    [Legal] result and removes [item] from the inventory list 
      if [item] is in the inventory of the player;
    [Illegal] otherwise. 
    Requires: 
              [st] is a abstract type representing the current game state.
              [item: string] is the identifier of the key that needs to be removed.*)
val drop: t -> string -> result

(** [take st item] returns 
    [Legal] result and adds [item] from the inventory list 
      if [item] is in the inventory of the player;
    [Illegal] otherwise. 
    Requires: 
              [st] is a abstract type representing the current game state.
              [item: string] is the identifier of the key that needs to be removed.*)
val take: t -> string -> result

(** Raised when the player's answer is of type answer but is incorrect. *)
exception IncorrectAnswer

(** Raised when the player's answer is not type answer or type quit. *)
exception IllegalAnswer
