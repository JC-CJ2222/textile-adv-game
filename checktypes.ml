module type AdventureSig = sig
  type t
  type room_id = string
  type exit_name = string
  exception UnknownRoom of room_id
  exception UnknownExit of exit_name
  val from_json : Yojson.Basic.json -> t
  val start_room : t -> room_id
  val room_ids : t -> room_id list
  val description : t -> room_id -> string
  val exits : t -> room_id -> exit_name list
  val next_room : t -> room_id -> exit_name -> room_id
  val next_rooms : t -> room_id -> room_id list
end

module AdventureCheck : AdventureSig = Adventure

module type CommandSig = sig
  type object_phrase = string list
  type command = 
    | Go of object_phrase
    | Score 
    | Take of object_phrase 
    | Drop of object_phrase
    | Inventory 
    | Trivia
    | Answer of object_phrase
    | Unlock of object_phrase
    | Lock of object_phrase
    | Help
    | Quit
  exception Empty
  exception Malformed
  val parse : string -> command
end

module CommandCheck : CommandSig = Command

module type StateSig = sig
  type t 
  val init_state : Adventure.t -> t
  val current_room_id : t -> string
  val visited : t -> string list
  type result = Legal of t | Illegal
  val go : Adventure.exit_name -> Adventure.t -> t -> result
end

module StateCheck : StateSig = State

module type AuthorsSig = sig
  val hours_worked : int list
end

module AuthorsCheck : AuthorsSig = Authors
