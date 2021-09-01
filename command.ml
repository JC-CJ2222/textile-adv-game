(** The type [object_phrase] represents the object phrase that can be part 
    of a player command. *)
type object_phrase = string list

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
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


(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [remove_spaces] removes the space elements in the input list.
    		  Example: 
    			remove_spaces ["go"; ""; ""; "clock"; ""; ""; "tower"; ""; ""] 
    			returns ["go"; "clock"; "tower"]
    			Requires: the input is a string list
    			Raises: Error if input list does not match precondition *)
let rec remove_spaces = function
  | [] -> []
  | h::t -> if h = "" then remove_spaces t
    else h::(remove_spaces t)

(** [parse str] parses a player's input into a [command].
    			Example: parse "go clock tower" returns Go ["clock";"tower"].
    			Requires: [str] is an valid string: contains only alphanumeric 
          (A-Z, a-z, 0-9) and space characters (only ASCII character code 32; 
          not tabs or newlines, etc...)
    			Raises: [Empty] is the string is empty.
            [Malformed] when the string does not contain 
            the right verbs defined in type command,
             or verb "go", "take", "drop", "answer" and "unlock" is followed by 
             an empty object phrase,
             or the rest of the verbs is followed by non-empty object phrase*)
let parse str =
  if str = "" then raise Empty else
    let lst = String.split_on_char ' ' str in
    let newlst = remove_spaces lst in
    match newlst with
    | h::[] -> (match [h] with 
        | ["quit"] -> Quit 
        | ["help"] -> Help
        | ["trivia"] -> Trivia
        | ["inventory"] -> Inventory
        | ["score"] -> Score
        | _ -> raise Malformed)
    | h::t -> (match [h] with 
        | ["go"] -> Go(t)
        | ["take"] -> Take(t) 
        |["drop"] -> Drop(t)
        | ["answer"] -> Answer(t)
        | ["unlock"] -> Unlock(t)
        | ["lock"]   -> Lock(t)
        | _ -> raise Malformed)
    | _  -> raise Malformed


