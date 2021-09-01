open OUnit2
open Adventure
open Command
open State

(********************************************************************
   Here are some helper functions for your testing of set-like lists. 
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
(* let cmp_set_like_lists lst1 lst2 =
   let uniq1 = List.sort_uniq compare lst1 in
   let uniq2 = List.sort_uniq compare lst2 in
   List.length lst1 = List.length uniq1
   &&
   List.length lst2 = List.length uniq2
   &&
   uniq1 = uniq2

   (** [pp_string s] pretty-prints string [s]. *)
   let pp_string s = "\"" ^ s ^ "\""

   (** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
   let pp_list pp_elt lst =
   let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
   in "[" ^ pp_elts lst ^ "]"

   (* These tests demonstrate how to use [cmp_set_like_lists] and 
   [pp_list] to get helpful output from OUnit. *)
   let cmp_demo = 
   [
    "order is irrelevant" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "bar"] ["bar"; "foo"]);
    (* Uncomment this test to see what happens when a test case fails.
       "duplicates not allowed" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "foo"] ["foo"]);
    *)
   ] *)

(********************************************************************
   End helper functions.
 ********************************************************************)

(* You are welcome to add strings containing JSON here, and use them as the
   basis for unit tests.  Or you can add .json files in this directory and
   use them, too.  Any .json files in this directory will be included
   by [make zip] as part of your CMS submission. *)


let a3 = Yojson.Basic.from_file "our_cornell.json"
let a3_t = from_json a3
let new_a3_t = add_key a3_t (init_state a3_t) "north"
let key_a3_t = add_key a3_t (add_key a3_t (init_state a3_t) "north") "food"



(** THE FOLLOWING ARE TEST CASES FOR A3. *)

(** A3 test cases for adventure*)
let adv_a3_tests =
  [ 
    "treasure_room a3_t" >:: (fun _ -> 
        assert_equal "duffield" (treasure_room a3_t));
    "help a3_t" >:: (fun _ -> 
        assert_equal "If you are in a new room/ don't have key, please type 'trivia'. After passing trivia, you have won a key type and this is automatically added to your inventory. Type 'take KEY_TYPE(eg. north)', so you can have access to all the rooms under the location KEY_TYPE(eg. north). 'drop KEY_TYPE' makes you lose access to rooms under location KEY_TYPE. 'take KEY_TYPE' enables you to pick up the dropped key but only in the location you dropped the key! Type 'unlock KEY_TYPE(eg. north)' to proceed to the rooms under that location. Do not try to unlock/go to nonexisting room names or take/drop nonexisting key types, else error! 'lock KEY_TYPE' locks the rooms under location KEY_TYPE. Please go to the room name exactly as they are north campus, not North Campus." (help a3_t));
    "incorrects a3_t" >:: (fun _ -> 
        assert_equal 4 (incorrects a3_t));
    "room_score a3_t" >:: (fun _ -> 
        assert_equal 0 (room_score a3_t "day hall"));
    "room_key a3_t" >:: (fun _ -> 
        assert_equal "day hall" (room_key a3_t "day hall"));
    "prize_key a3_t" >:: (fun _ -> 
        assert_equal "north" (prize_key a3_t "day hall"));
    "trivia_list and print_question a3_t" >:: (fun _ -> 
        assert_equal "What is the first letter in the Alphabet?" (print_question (List.hd (trivia_list a3_t "day hall"))));
    "trivia_list and options a3_t" >:: (fun _ -> 
        assert_equal [
          "A : A";
          "B : B";
          "C : C";
          "D : D"
        ] (options (List.hd (trivia_list a3_t "day hall"))));
    "trivia_list and print_options a3_t" >:: (fun _ -> 
        assert_equal "A : A; B : B; C : C; D : D; " (print_options (options (List.hd (trivia_list a3_t "day hall"))))); 
    "trivia_list and correct_answer a3_t" >:: (fun _ -> 
        assert_equal "A" (correct_answer (List.hd (trivia_list a3_t "day hall"))));




  ]

(** A3 test cases for state*)
let state_a3_tests =
  [
    "unlock startroom a3_t" >:: (fun _ -> 
        let r = unlock a3_t "day hall" (init_state a3_t) in
        match r with
        | Legal (t) -> assert_equal "day hall" (current_room_id t)
        | _ -> failwith "invalid");
    "unlock you got key a3_t" >:: (fun _ -> 
        let r = unlock a3_t "north" new_a3_t in
        match r with
        | Legal (t) -> assert_equal "north" (current_room_id (get_t "north campus" a3_t t))
        | _ -> failwith "invalid");
    "unlock you don't have the key a3_t" >:: (fun _ -> 
        let r = unlock a3_t "north" (init_state a3_t) in
        match r with
        | Legal (t) -> failwith "invalid"
        | Illegal -> assert_equal 1 1);
    "unlock invalid room a3_t" >:: (fun _ -> 
        let r = unlock a3_t "Colombia" (init_state a3_t) in
        match r with
        | Legal (t) -> failwith "invalid"
        | Illegal -> assert_equal 1 1);
    "lock room already locked a3_t" >:: (fun _ -> 
        let r = lock a3_t "north" (init_state a3_t) in
        match r with
        | Legal (t) -> assert_equal 1 1
        | Illegal -> failwith "invalid");
    "lock lock the room a3_t" >:: (fun _ -> 
        let r = unlock a3_t "north" new_a3_t in
        match r with
        | Legal (t) -> let new_r = lock a3_t "north" t in 
          (match new_r with 
           | Legal(t) -> assert_equal 1 1 | _ -> failwith "invalid")
        | _ -> failwith "invalid");
    "lock and drop you dont have the right key a3_t" >:: (fun _ -> 
        let r = unlock a3_t "north" new_a3_t in
        match r with
        | Legal (t) -> let new_r = drop t "north" in 
          (match new_r with 
           | Legal(t) -> (let newest_r = lock a3_t "north" t in 
                          (match newest_r with 
                           | Legal (t) -> failwith "invalid" 
                           | Illegal -> assert_equal 1 1)) 
           | Illegal -> failwith "invalid")
        | Illegal -> failwith "invalid");
    "lock room is not next door a3_t" >:: (fun _ -> 
        let r = lock a3_t "duffield" (init_state a3_t) in
        match r with
        | Legal (t) -> failwith "Invalid"
        | Illegal -> assert_equal 1 1);
    "add_score and get_score a3_t" >:: (fun _ -> 
        assert_equal 5 (get_score (add_score (init_state a3_t) 5) ));
    "get_inventory startroom a3_t" >:: (fun _ -> 
        assert_equal ["day hall"] (get_inventory (init_state a3_t)));
    "get_inventory north key added a3_t" >:: (fun _ -> 
        assert_equal ["north"; "day hall"] (get_inventory new_a3_t));
    "get_inventory 2 keys added a3_t" >:: (fun _ -> 
        assert_equal ["food"; "north"; "day hall"] (get_inventory key_a3_t));
    "print_inventory 2 keys added a3_t" >:: (fun _ -> 
        assert_equal "north, food" (print_inventory ["north";"food"]));
    "take a3_t" >:: (fun _ -> 
        let r = unlock a3_t "north" new_a3_t in
        match r with
        | Legal (t) -> let new_r = drop t "north" in 
          (match new_r with 
           | Legal(t) -> (let newest_r = take t "north" in 
                          (match newest_r with 
                           | Legal (t) -> assert_equal 1 1
                           | Illegal -> failwith "invalid")) 
           | Illegal -> failwith "invalid")
        | Illegal -> failwith "invalid");

  ]

(** A3 test cases for command*)
let command_a3_tests = 

  [
    "empty input" >:: (fun _ -> 
        assert_raises (Empty) (fun() -> parse ""));
    "invalid input" >:: (fun _ ->
        assert_raises (Malformed) (fun() -> parse "abc"));
    "nothing after go" >:: (fun _ ->
        assert_raises (Malformed) (fun() -> parse "go"));
    "stuff after quit" >:: (fun _ -> 
        assert_raises (Malformed) (fun() -> parse "quit clock tower"));
    "case-sensitive quit" >:: (fun _ ->
        assert_raises (Malformed) (fun() -> parse "Quit"));
    "case-sensitive go" >:: (fun _ -> 
        assert_raises (Malformed) (fun() -> parse "GO clock tower")); 
    "case-sensitive rooms" >:: (fun _ ->
        assert_equal (Go ["Ho";"plaza"]) (parse "go Ho plaza"));
    "quit" >:: (fun _ ->
        assert_equal (Quit) (parse "quit"));
    "go clock tower" >:: (fun _ ->
        assert_equal (Go ["clock";"tower"]) (parse "go clock tower"));
    "spaces in input" >:: (fun _ ->
        assert_equal (Go ["clock";"tower"]) (parse "go   clock   tower  "));   
    "go ho plaza" >:: (fun _ -> 
        assert_equal (Go ["ho";"plaza"]) (parse "go ho plaza"));
    "score" >:: (fun _ ->
        assert_equal (Score) (parse "score"));
    "inventory" >:: (fun _ ->
        assert_equal (Inventory) (parse "inventory"));
    "trivia" >:: (fun _ ->
        assert_equal (Trivia) (parse "trivia"));
    "help" >:: (fun _ ->
        assert_equal (Help) (parse "help"));
    "stuff after score" >:: (fun _ -> 
        assert_raises (Malformed) (fun() -> parse "score clock tower"));
    "stuff after inventory" >:: (fun _ -> 
        assert_raises (Malformed) (fun() -> parse "inventory clock tower"));
    "stuff after trivia" >:: (fun _ -> 
        assert_raises (Malformed) (fun() -> parse "trivia clock tower"));
    "stuff after help" >:: (fun _ -> 
        assert_raises (Malformed) (fun() -> parse "help clock tower"));
    "case-sensitive score" >:: (fun _ ->
        assert_raises (Malformed) (fun() -> parse "Score"));
    "case-sensitive inventory" >:: (fun _ ->
        assert_raises (Malformed) (fun() -> parse "Inventory"));
    "case-sensitive trivia" >:: (fun _ ->
        assert_raises (Malformed) (fun() -> parse "Trivia"));
    "case-sensitive help" >:: (fun _ ->
        assert_raises (Malformed) (fun() -> parse "Help"));
    "nothing after take" >:: (fun _ ->
        assert_raises (Malformed) (fun() -> parse "take"));
    "nothing after drop" >:: (fun _ ->
        assert_raises (Malformed) (fun() -> parse "drop"));
    "nothing after answer" >:: (fun _ ->
        assert_raises (Malformed) (fun() -> parse "answer"));
    "nothing after unlock" >:: (fun _ ->
        assert_raises (Malformed) (fun() -> parse "unlock"));
    "nothing after lock" >:: (fun _ ->
        assert_raises (Malformed) (fun() -> parse "lock"));
    "case-sensitive take" >:: (fun _ -> 
        assert_raises (Malformed) (fun() -> parse "TAKE clock tower")); 
    "case-sensitive rooms" >:: (fun _ ->
        assert_equal (Take ["Ho";"plaza"]) (parse "take Ho plaza"));
    "take clock tower" >:: (fun _ ->
        assert_equal (Take ["clock";"tower"]) (parse "take clock tower"));
    "spaces in input" >:: (fun _ ->
        assert_equal (Take ["clock";"tower"]) (parse "take   clock   tower  "));   
    "take ho plaza" >:: (fun _ -> 
        assert_equal (Take ["ho";"plaza"]) (parse "take ho plaza"));
    "case-sensitive drop" >:: (fun _ -> 
        assert_raises (Malformed) (fun() -> parse "DROP clock tower")); 
    "case-sensitive rooms" >:: (fun _ ->
        assert_equal (Drop ["Ho";"plaza"]) (parse "drop Ho plaza"));
    "drop clock tower" >:: (fun _ ->
        assert_equal (Drop ["clock";"tower"]) (parse "drop clock tower"));
    "spaces in input" >:: (fun _ ->
        assert_equal (Drop ["clock";"tower"]) (parse "drop   clock   tower  "));   
    "drop ho plaza" >:: (fun _ -> 
        assert_equal (Drop ["ho";"plaza"]) (parse "drop ho plaza"));
    "case-sensitive answer" >:: (fun _ -> 
        assert_raises (Malformed) (fun() -> parse "ANSWER clock tower")); 
    "case-sensitive rooms" >:: (fun _ ->
        assert_equal (Answer ["Ho";"plaza"]) (parse "answer Ho plaza"));
    "answer clock tower" >:: (fun _ ->
        assert_equal (Answer ["clock";"tower"]) (parse "answer clock tower"));
    "spaces in input" >:: (fun _ ->
        assert_equal (Answer ["clock";"tower"]) (parse "answer   clock   tower  "));   
    "answer ho plaza" >:: (fun _ -> 
        assert_equal (Answer ["ho";"plaza"]) (parse "answer ho plaza"));
    "case-sensitive unlock" >:: (fun _ -> 
        assert_raises (Malformed) (fun() -> parse "UNLOCK clock tower")); 
    "case-sensitive rooms" >:: (fun _ ->
        assert_equal (Unlock ["Ho";"plaza"]) (parse "unlock Ho plaza"));
    "unlock clock tower" >:: (fun _ ->
        assert_equal (Unlock ["clock";"tower"]) (parse "unlock clock tower"));
    "spaces in input" >:: (fun _ ->
        assert_equal (Unlock ["clock";"tower"]) (parse "unlock   clock   tower  "));   
    "unlock ho plaza" >:: (fun _ -> 
        assert_equal (Unlock ["ho";"plaza"]) (parse "unlock ho plaza"));
    "case-sensitive lock" >:: (fun _ -> 
        assert_raises (Malformed) (fun() -> parse "LOCK clock tower")); 
    "case-sensitive rooms" >:: (fun _ ->
        assert_equal (Lock ["Ho";"plaza"]) (parse "lock Ho plaza"));
    "lock clock tower" >:: (fun _ ->
        assert_equal (Lock ["clock";"tower"]) (parse "lock clock tower"));
    "spaces in input" >:: (fun _ ->
        assert_equal (Lock ["clock";"tower"]) (parse "lock   clock   tower  "));   
    "lock ho plaza" >:: (fun _ -> 
        assert_equal (Lock ["ho";"plaza"]) (parse "lock ho plaza"));


  ]
(** Suite for the test cases*)
let suite =
  "test suite for A2"  >::: List.flatten [
    adv_a3_tests;
    state_a3_tests;
    command_a3_tests;

  ]

(* Runs the tests*)
let _ = run_test_tt_main suite
