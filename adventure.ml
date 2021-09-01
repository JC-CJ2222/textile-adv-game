(* Note: You may introduce new code anywhere in this file. *) 

(** The type of room identifier*)
type room_id = string

(** The type of exit names.*)
type exit_name = string

(** Raised when an unknown room is encountered. *)
exception UnknownRoom of room_id

(** Raised when an unknown exit is encountered. *)
exception UnknownExit of exit_name


(** The abstract type representing a trivia question. It contains a question, 
    a list of answers, and a correct answer to the question.
    Preconditions: 
      all rooms in an adventure game have the same number of trivia questions.
      the options cannot be empty. *)
type trivia = {
  question: string; 
  options: string list; 
  answer: string; 
} 

(** The type of room exit: room exit contain exit name and the next rooms' 
    room identifier.*)
type room_exit = {
  name: exit_name;
  next_room: room_id;
}

(** The type of key: key contain key_type and a key des. 
    Each key has a score. It depends on the room it is stored. 
    The score of the key is 5 * (score_of_the_room + 1). *) 
type key = {
  key_type: string;
  key_des: string; 
}

(** The type of room: room contain a room identifier, a description, a list of 
    room exists, score, trivia, a room key, and a prize key.*)
type room = {
  id: room_id;
  description: string;
  exits: room_exit list;
  score: int;
  trivias: trivia list; 
  room_key: key; 
  prize_key : key; 
}

(** The abstract type of values representing adventures: [t] contains a 
    list of rooms, a starting room identifier, treasure room identifier, help,
    upper bound for wrong answers.*)
type t = {
  rooms: room list;
  start: room_id;
  treasure: room_id;  
  help: string;  
  incorrect: int; 
}

open Yojson.Basic.Util

(** [find_room_helper] recursively finds the room in a room list, using the 
    room identifer.
    Example: find_room_helper [ho plaza; clock tower] "ho plaza" 
    finds the room ho plaza which has id, description, and exits.  
    Requires: [rooms] is a list of rooms. 
              [room] is the room identifer. 
    Raises: [UnknownRoom] when an unknown room is encountered.*)
let rec find_room_helper (rooms: room list) (room: room_id): room = 
  match rooms with
  |[]    -> raise (UnknownRoom room)
  | h::t -> if h.id = room then h else
      find_room_helper t room

(** [find_room] uses [find_room_helper] to find the room in adventure. 
    Example: find_room (from_json lonely_room.json) ("the room") returns the 
    (the room: id, description, exits) from adventure lonely_room's rooms.
    Requires: [adv] is the adventure.
              [room] is the room identifer. *)
let find_room (adv: t) (room: room_id): room = 
  find_room_helper adv.rooms room

(** [exits_name_helper] recursively puts the exit names into a list.
    Requires:  *) 
let rec exits_names_helper = function 
  |[]     -> []
  | h::t  -> h.name::(exits_names_helper t) 

(** [exit_names] uses [exits_name_helper] to make a exist name list given the 
    adventure and the current room.
    Example: exit_names (from_json lonely_room.json) "the room" returns [].
    Requires: [adv] is the abstract type representing adventure. 
              [room] is the room identifier. *)
let exits_names (adv:t) (room: room_id): exit_name list =
  let cur_room = find_room adv room in
  exits_names_helper cur_room.exits

(** [find_exit_helper] recursively finds a exit room in a list of exit rooms,
    using the exit name.
    Example: find_exit_helper [ho plaza; cornell] "ho plaza" returns the 
    room_exit ho plaza with name and next room. 
    Requires: [exits] is a list of room exits.
              [ex] is the exist room identifier.
    Raises: [UnknownExit] when unknown exit is encountered.*)
let rec find_exit_helper (exits:room_exit list) (ex: exit_name): room_exit = 
  match exits with
  |[]    -> raise (UnknownExit ex)
  | h::t -> if h.name = ex then h else
      find_exit_helper t ex

(** [find_exit] returns the exit room given the current room's exit room list 
    and the exit name in adventure. 
    Example: find_exit (from_json ho_plaza.json) "ho plaza" "clock tower" 
    returns the room clock tower with an id, description, and exits.
    Requires: [adv] is the abstract type representing adventure. 
              [room] is the room identifer of the current room. 
              [ex] is the exit name.*)
let find_exit (adv:t) (room:room_id) (ex:exit_name): room_exit= 
  let cur_room = find_room adv room in
  find_exit_helper cur_room.exits ex

(** [exits_from_json] is a helper function that takes 
    association list of exits and return room_exit list. 
    Note it doesn't actually take json object but a list.
    Requires: [Yojson.Basic.t list] is a association list of the json file.*)
let rec exits_from_json = function
  | []   -> []
  | h::t -> 
    let exit_name =  h |> member "name" |> to_string in
    let to_room = h |> member "room id" |> to_string in 
    let cur_exit = {name = exit_name; next_room = to_room} in
    cur_exit::(exits_from_json t)


(** [options_from_room] is a helper function that takes 
    the associated list of opt (option) and returns opt (option) list. 
    Note it doesn't actually take json object but a list. 
    Requires: [Yojson.Basic.t list] is a association list of the json file.*)
let rec options_from_json = function  
  | [] -> []
  | h :: t -> 
    let opt = h |> to_string in opt :: (options_from_json t) 

(** [trivias_from_room] is a helper function that takes 
    the associated list of trivia and returns trivia list. 
    Note it doesn't actually take json object but a list. 
    Requires: [Yojson.Basic.t list] is a association list of the json file.*)
let rec trivias_from_json = function   
  | [] -> [] 
  | h :: t ->
    let question = h |> member "question" |> to_string in 
    let options = h |> member "options" |> to_list |> options_from_json in 
    let cor_ans = h |> member "answer" |> to_string in 
    let a_tr = {question = question; options = options; answer = cor_ans} in 
    a_tr :: (trivias_from_json t)


(** [trivias_from_room] is a helper function that takes 
    a json object and returns key.*)
let key_from_json h = 
  let key_id = h |> member "key type" |> to_string in 
  let key_des = h |> member "key des" |> to_string in  
  { key_type = key_id; key_des = key_des}

(** [rooms_from_json] is a helper function that takes
    association list of rooms and return exit list
    Note it doesn't actually take json object but a list.
    Requires: [Yojson.Basic.t list] is a association list of the json file. *)
let rec rooms_from_json = function
  | []   -> []
  | h::t ->
    let id = h |> member "id" |> to_string in
    let description = h |> member "description" |> to_string in
    let exits = h |> member "exits" |> to_list |>exits_from_json in
    let score = h |> member "score" |> to_int in 
    let trivias = h |> member "trivias" |> to_list |>trivias_from_json in 
    let room_key = h |> member "room key" |> key_from_json in
    let prize_key = h |> member "prize key" |> key_from_json in  
    let cur_room = {id = id; description = description; exits = exits; score = score; 
                    trivias = trivias; room_key = room_key; prize_key = prize_key } 
    in cur_room::(rooms_from_json t)

(** [from_json] takes json file and returns adv_json from the json file.
    Example: from_json lonely_room.json returns an adventure for lonely_room.
    Requires: [json] is a json file.*)
let from_json (json: Yojson.Basic.json) : t = 
  let rooms_json_list = json |> member "rooms" |> to_list in
  let rooms = rooms_from_json rooms_json_list in
  let init_room = json |> member "start room" |> to_string in 
  let pls_help = json |> member "help" |> to_string in 
  let final_dest = json |> member "treasure room" |> to_string in 
  let num_incor = json |> member "incorrect" |> to_int in 
  {rooms = rooms; start = init_room; help = pls_help; treasure = final_dest; 
   incorrect = num_incor}

(** [start_room] takes adv_json and return the room identifer of starting room. 
    Example: start_room (from_json ho_plaza.json) returns "ho plaza".
    Requires: [adv] is the abstract type representing adventure. *)
let start_room (adv:t): room_id =
  adv.start

(** [room_ids_helper] takes a list of room 
    and returns a list of room identifiers.
    Requires: [room list] is a list of rooms with id, descripiton, and exits*)
let rec room_ids_helper = function 
  | [] -> []
  | h::t -> h.id::(room_ids_helper t)

(** [room_ids] uses [room_ids_helper], takes adv_json and return a list of 
    rooms identifiers.
    Example: room_ids (from_json ho_plaza.json) 
    returns ["ho plaza"; "health";...].  
    Requires: [adv] is the abstract type representing adventure.*)
let room_ids (adv:t):room_id list = 
  room_ids_helper adv.rooms

(** [description] takes adv_json and a room identifier, and returns the room's 
    description.
    Example: description (from_json lonely_room.json) "the room" returns 
    "A very lonely room". 
    Requires: [adv] is the abstract type representing adventure.
              [room] is a room identifier.
    Raises: [UnknownRoom] when an unknown room is encountered.*)
let description (adv:t) (room:room_id): string =
  (find_room adv room).description

(** [exits] takes adv_json and a room identifier, and return the room's exits.
    Example: exits (from_json lonely_room.json) "the room" returns []. 
    Requires: [adv] is the abstract type representing adventure.
              [room] is a room identifier.
    Raises: [UnknownRoom] when an unknown room is encountered.*)
let exits (adv:t) (room:room_id): exit_name list = 
  exits_names adv room

(** [next_room] takes adv_json, a room identifier, and an exit, 
    and return the new room.
    Example: next_room (from_json ho_plaza.json) "ho plaza" "southwest" 
    returns "health". 
    Requires: [adv] is the abstract type representing adventure.
              [room] is a room identifier.
              [ex] is a exit name. 
    Raises: [UnknownRoom] when an unknown room is encountered.
            [UnknownExit] when an unknown exit is encountered.*)
let next_room (adv:t) (room: room_id) (ex: exit_name): room_id =
  (find_exit adv room ex).next_room

(** [remove_duplicates] takes a list and returns a list withou duplicates.
    Example: remove_duplicates ["ho plaza"; "ho plaza"] returns ["ho plaza"]. 
    Requires: ['a list] is a list. *)
let rec remove_duplicates = function
  |[]     -> []
  | h::t  -> if List.exists (fun s -> s = h) t then remove_duplicates t
    else h::(remove_duplicates t)

(** [next_rooms] takes adv_json and a room identifier, and return the possible 
    next rooms. 
    Example: exits (from_json ho_plaza.json) "health" returns ["ho plaza"]
    Requires:  [adv] is the abstract type representing adventure.
              [room] is a room identifier. 
    Raises: [UnknownRoom] when an unknown room is encountered.*)
let next_rooms (adv:t) (room:room_id): room_id list =
  let exits = exits_names adv room in
  remove_duplicates (List.map (next_room adv room) exits)


(* ADDED FUNCTIONS FOR A3 BELOW *)

(** [treasure_room adv] returns the treasure room identifier of adventure [adv].
    Example: treasure room (from_json our_cornell.json) returns "duffield" 
    Requires [adv] is the abstract type representing adventure. *)
let treasure_room (adv: t) : room_id = 
  adv.treasure

(** [help adv] returns a helpful message of adventure [adv].
    Example: help (from_json small_adventure.json) returns the helpful message. 
    Requires: [adv] is the abstract type representing adventure. *)
let help (adv: t) : string = 
  adv.help 

(** [incorrects adv] returns the upper bound for number of incorrect answers 
    of a player before they dies in adventure [adv].
    Example: incorrects (from_json our_cornell.json) returns 4 
    Requires: [adv] is the abstract type representing adventure. *)
let incorrects (adv: t) : int= 
  adv.incorrect  

(** [room_score adv room] returns the score associated to entering the room [room] 
    in adventure [adv].
    Example: room_score (from_json our_cornell.json) "day hall" returns 1
    Requires:  [adv] is the abstract type representing adventure.
              [room] is a room identifier. *)
let room_score (adv:t) (room: room_id) : int = 
  (find_room adv room).score

(** [room_key adv room] returns the key identified to the room [room] in adventure [adv]. 
    Example: room_key (from_json our_cornell.json) "day hall" returns "day hall"
    Requires:  [adv] is the abstract type representing adventure.
              [room] is a room identifier. *)
let room_key (adv : t) (room: room_id) : string = 
  ((find_room adv room).room_key).key_type

(** [prize_key adv room] returns the a prize key after wining trivia in room [room]
    in adventure [adv]. 
    Example: room_score (from_json our_cornell.json) "day hall" returns "north"
    Requires:  [adv] is the abstract type representing adventure.
              [room] is a room identifier. *)
let prize_key (adv : t) (room: room_id) : string = 
  ((find_room adv room).prize_key).key_type

(** [prize_key_ds adv room] returns the a prize key's description after wining trivia 
    in room [room] in adventure [adv]. It should give a helpful message such as
    "This key unlocks KEY_TYPE. Here are the next rooms. But this key may not open 
    all rooms."
    Requires:  [adv] is the abstract type representing adventure.
              [room] is a room identifier. *)
let prize_key_des (adv: t) (room: room_id) : string = 
  ((find_room adv room).prize_key).key_des 

(** [trivia_list adv room] returns the trivia list of room [room] in adventure [adv]. 
    Requires:  [adv] is the abstract type representing adventure.
              [room] is a room identifier. *)
let trivia_list (adv: t) (room: room_id) : trivia list = 
  (find_room adv room).trivias 

(** [print_question tr] prints the trivia [tr] question in a nice format. 
    Example: the first trivia question of our_cornell.json is 
    "What is the first letter of the Alphabet?"
    Requires:  [tr] is a trivia *)
let print_question (tr: trivia) : string = 
  tr.question 

(** [options tr] is list of options for the trivia [tr] . 
    Example: the first trivia options of our_cornell.json is 
    returns ["A : A"; "B : B", "C : C", "D : D"]
    Requires:  [tr] is a trivia *)
let options (tr: trivia) = 
  tr.options

(** [print_option lst] prints the list of options [lst] in a nice format. 
    Example: "A : A"; "B : B", "C : C", "D : D" 
    Requires:  [lst] is a trivia option list. *)
let rec print_options (lst: string list) = 
  match lst with
  |[] -> ""
  |h::t -> h ^ "; " ^ print_options t

(** [correct_answer tr] is the correct answer for the trivia [tr].  
    Example: the first trivia correct answer of our_cornell.json is "A"
    Requires:  [tr] is a trivia option list.*)
let correct_answer (tr: trivia) = 
  tr.answer



