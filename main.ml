open Adventure
open State
open Command


(** [trivia_step adv st tr_lst inc] 
    displays a trivia question to the player, and then prompts the player 
    to answer the trivia question. 
    They have already answered [inc] number of questions wrong. If [inc] is 
    greater or equal to the maximum number of incorrect answers allowed in 
    adventure [adv], then the player dies.
    The user's input must be type [Answer(t)], [Score], [Help] or [Quit].
    If the player answers, the check whether the answer is correct, and update
    the number of incorrect if needed.
    If all questions are answered, check the number of incorrect. If the player 
    passes, then they get the key. 
    [trivia_step] and [normal_step] are mutually recursive.*)
let rec trivia_step adv st (tr_lst: trivia list) (inc: int) : unit=
  match tr_lst with 
  |[] -> if inc > incorrects adv then 
      (print_endline "Sorry, you have answered too many questions wrong. 
      You died. The game is over."; exit 0)
    else 
      (let new_state = st |> current_room_id |> prize_key adv |> add_key adv st in
       print_endline ("Congratulations! You just got a new key.");
       print_endline (prize_key_des adv (current_room_id st));
       print_endline ("The key's score is "
                      ^ string_of_int(((room_score adv (current_room_id st))
                                       + 1) * 5) ^ ".");
       print_endline ("Now, you can go to the next room with the 'go' command. 
              Or you can type 'inventory' to see all the keys you currently have.");
       normal_step adv new_state)
  |h::t -> 
    (** step 1: display the trivia question. *)
    print_endline ("Question: " ^ (print_question h));
    print_endline ("Options: " ^ (h |> options |> print_options));
    print_string "> ";
    print_endline ("Please answer the question by typing 'answer' and the letter
  of your choice. Please note that both upper and lower cases are accepted. 
  If you give more than " ^ 
                   (string_of_int (incorrects adv)) ^ " questions in one room 
  incorrect, then you would die immediately.");
    print_string "> ";
    (** step 2: handles the input. *)
    match read_line () with
    | exception End_of_file -> ()
    | command_name -> 
      match (parse command_name) with  
      |exception Empty -> print_endline "You can't have an empty answer. 
      Please enter a valid answer like 'answer B'.";
        trivia_step adv st tr_lst inc
      |exception Malformed -> print_endline "The answer you entered is invalid.
      Please enter a valid answer like 'answer B'.";
        trivia_step adv st tr_lst inc
      | Inventory | Trivia  ->
        print_endline "Please enter a valid answer like 'answer B'. 
      If you need help, please enter 'help'.";
        trivia_step adv st tr_lst inc
      | Lock(t) | Unlock(t) | Go(t) | Drop(t) | Take(t)->         
        print_endline "Please enter a valid answer like 'answer B'. 
      If you need help, please enter 'help'.";
        trivia_step adv st tr_lst inc
      | Help -> 
        print_endline (help adv); 
        print_endline "> "; 
        trivia_step adv st tr_lst inc
      | Score -> 
        print_endline ("Your current score is " ^ 
                       string_of_int (get_score st));
        trivia_step adv st tr_lst inc
      | Answer(s) -> 
        if (String.lowercase_ascii (correct_answer h) = String.lowercase_ascii 
              (List.hd s)) 
        then (print_endline "Great, you're right!"; (trivia_step adv st t inc))
        else print_endline "Sorry, you're wrong!"; 
        (trivia_step adv st t (inc + 1))
      | Quit -> print_endline "Farewell and goodbye my adventurer";
        exit 0

(** [normal_step adv st] 
    prompts the player to input a command, and executes the command. 
    If the new command is valid and not quit, execute the new command.
    If the new command is quit, quit the game.
    If the new command is empty, malformed, or illegal, prompt the user to 
    input a new command.
    Valid command includes [go], [inventory], [score], [drop], [take], [lock],
     [unlock], [trivia], [answer], and [help].

    Requires: [adv] is a valid adventure game;
              [st] is the current state of the player;
    Examples: if the opened file is "ho_plaza.json", then
              "" is an empty command;
              "skjhdiuy" and "Go ho plaza" are malformed commands;
              "go Duffield" is an illegal command 
              (because there's no exit Duffield).
    [trivia_step] and [normal_step] are mutually recursive.*)
and normal_step adv st =
  if (current_room_id st) = (treasure_room adv) then 
    (print_endline "Congratulation! You win this game! The game will 
  automatically quit."; exit 0)
  else 
    ((** step 1: display the description of the current room *)
      let msg =   if( let prize = st |> current_room_id |> prize_key adv 
                      in List.mem prize (get_inventory st) ) 
        then  "You already have the prize for the current room!\n"
        else "" in
      print_string msg;
      print_string ((description adv (current_room_id st)) ^ "\n");  
      print_endline ("The room's score is "
                     ^ string_of_int((room_score adv (current_room_id st))) ^ ".");
      print_endline "Check your new score by with 'score'.";
      (** step 2: prompt for a new command*)
      print_endline "Please enter a new command. A valid command may start with 
      'go', 'inventory', 'score', 'drop', 'take', 'lock', 'unlock', 'trivia', 
      'answer', 'help', or 'quit'.";
      print_string "> ";
      (** step 3: check and execute the new command.*)
      match read_line () with
      | exception End_of_file -> ()
      | command_name -> 
        match (parse command_name) with  
        |exception Empty -> print_endline "You can't have an empty command. 
      Please enter a valid command like 'go clock tower'. If you need help, 
      please enter 'help'.";
          normal_step adv st
        |exception Malformed -> print_endline "The command you entered is invalid.
      Please enter a valid command like 'go clock tower'. If you need help, 
      please enter 'help'.";
          normal_step adv st 
        | Quit -> print_endline "Farewell and goodbye my adventurer.";
          exit 0
        | Inventory -> 
          print_endline ("The keys you have right now are " ^ 
                         print_inventory (get_inventory st) ^ "."); 
          normal_step adv st
        | Score -> 
          print_endline ("Your current score is " ^ 
                         string_of_int (get_score st));
          normal_step adv st
        | Help -> print_endline (help adv); 
          normal_step adv st
        | Trivia ->   
          print_endline ("Welcome to " ^ current_room_id st ^ ". You must answer the 
      following questions in order to move on and get the key.");
          print_string "> ";
          let trivia_list = st |> current_room_id |> trivia_list adv in
          trivia_step adv st trivia_list 0
        | Answer(t) -> print_endline "You're not answering a trivia question. 
      Please enter a valid command. If you need help, please enter 'help'.";
        | Take(t) -> 
          (match (take st (String.concat " " t)) with 
           | Illegal -> print_endline "The item is not in the room"; normal_step adv st
           | Legal(s) -> 
             print_endline ("You just take the key " 
                            ^ String.concat " " t ^ ". " ) ;
             normal_step adv s
          )
        | Drop(t) -> 
          (match (drop st (String.concat " " t)) with 
           | Illegal -> print_endline "You don't have the item so you can't drop it!";
             print_endline "Also, why are you dropping a key? That's dumb!"; 
             normal_step adv st
           | Legal(s) -> print_endline ("You just dropped the key " 
                                        ^ String.concat " " t ^ ". " );
             normal_step adv s
          )
        | Lock(t) -> 
          (match (lock adv (String.concat " " t) st) with
           | Illegal -> print_endline "You can't lock the room right now."; 
             normal_step adv st
           | Legal(s) -> 
             print_endline ("You just locked " ^ (String.concat " " t)); 
             normal_step adv s
          )
        | Unlock(t) -> 
          (match (unlock adv (String.concat " " t) st) with
           | Illegal -> print_endline (String.concat " " t); 
             print_endline "You can't unlock this room."; normal_step adv st
           | Legal(s) ->
             print_endline ("Congratulations! " ^ (String.concat " " t) ^ 
                            " is unlocked now. "); 
             print_endline "Recall where you can go to once this room is 
             locked? Now try to 'go' to these rooms.";
             print_endline (prize_key_des adv (current_room_id st));
             normal_step adv s)
        | Go (t) -> 
          match (go (String.concat " " t) adv st) with 
          |Illegal -> 
            print_endline "It is not an exit or you have not unlocked this room. 
          Try again. If confused, type 'help' for a helpful message."; 
            normal_step adv st
          |Legal(s) -> 
            normal_step adv s
    )

(** [play_game f] takes a string and converts into an adventure and plays it.
    Example: play_game "ho_plaza.json" executes the adventure ho_plaza. 
    Requires: [f] is a string representing the adventure file.
    Raises: file not found if invalid file name provided. *)
let rec play_game f =
  try
    let j = Yojson.Basic.from_file f in 
    let adv = from_json j in
    print_endline "Instructions:"; 
    print_endline "> The [goal] of the game is to visit rooms in the adventure and
     find the [treasure room]."; 
    print_endline "> You would get some unknown scores for each room you enter. 
    Each room stores a [key] and several [exits]. Each key would give you access
    to more rooms, which would finally lead you to the treasure room."; 
    print_endline "> However, to get the keys, you need to answer several [trivia] 
    questions in each room. You should be careful when answering these questions
    , because you would die if you answer too many questions wrong."; 
    print_endline "> The trivia questions are pass/fail. You don't get scores for
    answering question correct or you don't get penalized (with scores) for 
    answering them wrong. However, you do get a random score for getting a new
     key."; 
    print_endline "> Upon entering a room, you should read its descriptions, and 
    then type 'trivia' to start the trivia questions. You should use the 'take' 
    and 'drop' commands to add or remove keys to your 'inventory'. You could 
    also 'lock' and 'unlock' rooms. However, you can only 'lock' a room if it is
     already unlocked, and 'unlock' a room if it is locked."; 
    print_endline "> Hopefully you have learned how to play the game. If you're 
    still in doubts, you could type 'help' for helpful message anytime during
     the game.";
    print_endline "> ";
    normal_step adv (init_state adv)
  with e -> 
    print_endline "File not found. Please enter a valid file like ho_plaza.json.";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | file_name -> play_game file_name

(** [main ()] prompts for the game to play, then starts it.
    Example: Typing ho_plaza.json into the game interface 
    prints the following:
    You are on Ho Plaza. Cornell Health is to the southwest. 
    The chimes are playing a concert in the clock tower. 
    Someone tries to hand you a quartercard, but you avoid them.
    Requires: the file name is in the directory and is valid json.*)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Text Adventure Game engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()
