(** 
   Representation of static adventure data.

   This module represents the data stored in adventure files, including
   the rooms and exits.  It handles loading of that data from JSON as well
   as querying the data.
*)

(* You are free to add more code here. *)

(**********************************************************************
 * DO NOT CHANGE THIS CODE
 * It is part of the interface the course staff will use to test your 
 * submission.
*)

(** The abstract type of values representing adventures. *)
type t

(** The type of room identifiers. *)
type room_id = string

(** The type of exit names. *)
type exit_name = string

(** Raised when an unknown room is encountered. *)
exception UnknownRoom of room_id

(** Raised when an unknown exit is encountered. *)
exception UnknownExit of exit_name

(** [from_json j] is the adventure that [j] represents.
    Requires: [j] is a valid JSON adventure representation. *)
val from_json : Yojson.Basic.json -> t

(** [start_room a] is the identifier of the starting room in adventure 
    [a]. *)
val start_room : t -> room_id

(** [room_ids a] is a set-like list of all of the room identifiers in 
    adventure [a]. *)
val room_ids : t -> room_id list

(** [description a r] is the description of room [r] in adventure [a]. 
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]. *)
val description : t -> room_id -> string

(** [exits a r] is a set-like list of all exit names from room [r] in 
    adventure [a].
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]. *)
val exits : t -> room_id -> exit_name list

(** [next_room a r e] is the room to which [e] exits from room [r] in
    adventure [a].  
    Raises [UnknownRoom r] if [r] is not a room identifier in [a].
    Raises [UnknownExit e] if [e] is not an exit from room [r] in [a]. *)
val next_room : t -> room_id -> exit_name -> room_id

(** [next_rooms a r] is a set-like list of all rooms to which there is an exit 
    from [r] in adventure [a]. 
    Raises [UnknownRoom r] if [r] is not a room identifier in [a].*)
val next_rooms : t -> room_id -> room_id list

(* END DO NOT CHANGE
 **********************************************************************)

(* You are free to add more code here. *)

(** The abstract type representing a trivia question. It contains a question, 
    a list of answers, and a correct answer to the question.
    Preconditions: 
      all rooms in an adventure game have the same number of trivia questions.
      the options cannot be empty. *)
type trivia 

(** [treasure room a] returns the identifier of the treasure room in adventure 
    [a]*)
val treasure_room : t -> room_id 

(** [help a] returns a helpful message in the adventure [a].*)
val help : t -> string

(** [incorrects a] is the maximum number of questions in one trivia game the 
    player can get incorrect before dying in adventure [a].*)
val incorrects: t -> int 

(** [room_score a r] is the score of the room [r] of adventure [a]*)
val room_score : t -> room_id -> int 

(**[room_key a r] is the key identifier for a room [r] in adventure [a]*)
val room_key: t -> room_id -> string 

(**[prize_key a r] is the key identifier of other rooms after winning trivia
   in a room [r] in adventure [a] *) 
val prize_key: t -> room_id -> string 

(**[prize_room_key_des a r] is the key [k] description*)
val prize_key_des: t -> room_id -> string 

(** [trivia_list a r] is the list of trivias for room [r] in adventure [a]*)
val trivia_list : t -> room_id -> trivia list 

(** [print_question tr] is the question in a trivia [tr]*)
val print_question : trivia -> string 

(** [options tr] is the options in a trivia [tr]*)
val options : trivia -> string list

(** [print_options lst] nicely prints a list of options*)
val print_options : string list -> string 

(** [correct_answers tr] is the correct answer in a trivia [tr]*)
val correct_answer : trivia -> string

(** [prize_key_des a r] is the prize key description after winning a key in 
    room [r] of aventure [a]*) 
val prize_key_des: t -> room_id -> string

