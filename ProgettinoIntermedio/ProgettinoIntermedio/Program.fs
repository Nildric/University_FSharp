module EsercitazioneIntermedia

// Auxiliar functions
let list_of_string (str : string) : char list = str.ToCharArray() |> List.ofArray
let string_of_list (cl : char list) : string = cl |> List.fold (sprintf "%s%c") ""

// Body

(*
 * Authors:
 * - Federico Longhin       857179
 * - Francesco Sponchiado   856166
 *
 *)

(*
 *  list_size
 *
 *  Calculates the size of a given list.
 *
 *  list                  'a list                           The list to calculate the length of
 *  result                int                               Temporary integer used for tail-call recursion, must be initialized to 0
 *
 *  Return                int                               The input list's length
 *
 *  examples:
 *
 *  listSize [1; -34; 8] 0 --> 3
 *  listSize [] 0          --> 0
 *
 *)
let rec list_size list (result : int) : int =
    match list with
        | [] -> result
        | x :: xs -> list_size xs (result + 1)
    ;;

(*
 *  item_at
 *
 *  Returns the item at the given position in the given list.
 *
 *  list                  'a list                           The list to find the element in
 *  index                 int                               The index of the element to extract (starting from 0)
 *
 *  Return                'a                                The element if found, fails with "Index out of bounds" otherwise
 *
 *  examples:
 *
 *  itemAt [1; -34; 8] 1 --> -34
 *  itemAt [1; -34; 8] 3 --> Index out of bounds
 *
 *)
let rec item_at list (index : int) =
   match list with
       | head :: _ when index = 0 -> head
       | _ :: tail -> item_at tail (index - 1)
       | [] -> failwith "Index out of bounds"
    ;;

(*
 *  remove
 *
 *  Removes the item at the given index in the given list.
 *
 *  list                  'a list                           The list to remove the element from
 *  index                 int                               The index of the element to remove from the list (starting from 0)
 *
 *  Return                'a list                           The list with the element removed, or fails with "Index out of bounds"
 *
 *  examples:
 *
 *  remove [1; -34; 8] 1 --> [1; 8]
 *  remove [1; -34; 8] 3 --> Index out of bounds
 *
 *)
let rec remove list (index : int) =
    match list, index with
        | x :: xs, 0 -> xs
        | x :: xs, _  -> x :: (remove xs (index - 1))
        | [], _ -> failwith "Index out of bounds"
    ;;

(*
 *  hamming_distance
 *
 *  Calculates the Hamming distance between two words (in char list format).
 *
 *  w1                    char list                         The first word
 *  w2                    char list                         The second word
 *  result                int                               Temporary buffer used internally for tail-call recursion, must be initialized to 0
 *
 *  Return                int                               The Hamming distance between the given words
 *
 *  examples:
 *
 *  hamming_distance ['f';'o';'o'] ['b';'o';'o'] --> 1
 *  hamming_distance ['f';'o';'o'] ['f';'o';'o'] --> 0
 *  hamming_distance [] ['f';'o';'o']            --> Error : "The two words must have the same length"
 *  hamming_distance ['f';'o';'o'] []            --> Error : "The two words must have the same length"
 *
 *)
let rec hamming_distance (w1 : char list) (w2 : char list) (result : int) : int =
    match w1, w2 with
        | [], [] -> result
        | x::xs, y::ys -> if x <> y then
                              hamming_distance xs ys (result + 1)
                          else
                              hamming_distance xs ys result
        | _ -> failwith "The two words must have the same length"
    ;;

(*
 *  split_string_in_chars_lists 
 *
 *  Tokenizes a string in the "list of characters" form, skipping spaces.
 *
 *  text                  char list                         Input string that will be tokenized (in char list form because of listSize)
 *  result                char list list                    List of char lists (words) that represents the tokenized string
 *  temp_list             char list                         List of characters representing a temporary word (buffer to save the word before a space or the end of the string is found)
 *
 *  Return                char list list                    The tokenized string 
 *
 *  example:
 *
 *  split_string_in_chars_lists ['h';'e';'y';' ';'t';'h';'e';'r';'e'] [] [] --> [['h';'e';'y']; ['t';'h';'e';'r';'e']]
 *
 *)
let rec split_string_in_chars_lists (text : char list) (result : char list list) (temp_list : char list) : char list list = 
    match text with
        | [] when temp_list <> [] -> result @ [temp_list]
        | [] when temp_list = [] -> result
        | x :: xs when x = ' ' && temp_list <> [] -> split_string_in_chars_lists xs (result @ [temp_list]) []
        | x :: xs when x = ' ' && temp_list = [] -> split_string_in_chars_lists xs (result) []
        | x :: xs -> split_string_in_chars_lists xs (result) (temp_list @ [x])
        | _ -> result
    ;;

(*
 *  join_char_lists 
 *
 *  Joins a list of char lists into a string list, converting each char list into a string.
 *
 *  words                 char list list                    The list of words (in char list format) to be converted
 *
 *  Return                string list                       The joined strings
 *
 *  example:
 *
 *  join_char_lists [['h';'e';'y']; ['t';'h';'e';'r';'e']] --> ["hey"; "there"]
 *
 *)
let rec join_char_lists (words : char list list) : string list = 
        match words with
            | [] -> []
            | x :: xs -> (string_of_list x) :: (join_char_lists xs)
    ;;

(*
 *  split_string 
 *
 *  Splits a string into a list of words, using the space (" ") character as the delimiter.
 *
 *  text                  string                            The text to split
 *
 *  Return                string list                       The split strings
 *
 *  example:
 *
 *  split_string "hey there, how are you?" --> ["hey"; "there,"; "how"; "are"; "you?"]
 *
 *)
let split_string (text : string) : string list =    
   join_char_lists (split_string_in_chars_lists (list_of_string text) [] [])
   ;;

(*
 *  insert_min
 *  
 *  Inserts a pair into a list of pairs in the format (distance, word), checking the distance every time 
 *  and replacing the whole list with the input pair if the input pair's distance is less than the distances in the list,
 *  or appending the pair to the pairs list if the distance is equal.
 *  If the input pair's distance is greater than the distances in the pairs list, it is discarded.
 *
 *  pairs_list            (int * string) list               List of pairs the pair will be checked against 
 *                                                          and (possibly) inserted into
 *  pair                  (int * string)                    The pair to test for possible insertion
 *  
 *  Return                (int * string) list               The resultant list of pairs, or a list containing the pair itself 
 *                                                          if its distance is less than the distances in the input list
 *
 *  examples:
 *  
 *  insert_min  [(5, "hello"); (5, "test")] (2, "insert")  --> [(2, "insert")]
 *  insert_min  [(5, "hello"); (5, "test")] (5, "insert")  --> [(5, "hello"); (5, "test"); (5, "insert")]
 *  insert_min  [(5, "hello"); (5, "test")] (20, "insert") --> [(5, "hello"); (5, "test")]
 *  insert_min  [] (20, "insert")                          --> [(20, "insert")]
 *
 *)
let rec insert_min (pairs_list : (int * string) list) (pair : (int * string)) : (int * string) list = 
    match pairs_list with
        | [] -> [pair]
        | (distance, word) :: xs when (fst pair) < distance -> [pair]
        | (distance, word) :: xs when (fst pair) > distance -> pairs_list
        | (distance, word) :: xs when (fst pair) = distance -> (distance, word) :: (insert_min xs pair)
        | _ -> [pair]
  ;;

(*
 *  convert_to_string_list_nested
 *  
 *  Inner function used by convert_to_string_list to convert a list of (distance, word) pairs into a string list.
 *  Walks the innermost list of pairs and appends each word to form a string list.
 *
 *  words_list            (int * string) list               List of pairs to convert to a string list
 *  result                string list                       Temporary buffer used internally for tail-call recursion, must be initialized to []
 *  
 *  Return                string list                       The list of words extracted from the pairs
 *
 *  example:
 *  
 *  convert_to_string_list_nested  [(5, "hello"); (5, "test")] --> ["hello"; "test"]
 *
 *)
let rec convert_to_string_list_nested (words_list : (int * string) list) (result : string list) : string list = 
    match words_list with 
        | [] -> result
        | (distance, word) :: xs -> convert_to_string_list_nested xs (result @ [word])
    ;;

(*
 *  convert_to_string_list
 *  
 *  Converts a list of lists of (distance, word) pairs into a list of lists of strings, extracting the
 *  "word" element from each pair and concatenating it to the next pair's "word" element.
 *
 *  words_list            (int * string) list list          List of lists of pairs to convert
 *  result                string list list                  Temporary buffer used internally for tail-call recursion, must be initialized to []
 *  
 *  Return                string list list                  The list of lists of words extracted from the pairs
 *
 *  example:
 *  
 *  convert_to_string_list_nested  [[(5, "hello"); (5, "test")]; [(0, "foo")]; [(2, "foo"); (2, "bar")]] --> [["hello"; "test"]; ["foo"]; ["foo"; "bar"]]
 *
 *)
let rec convert_to_string_list (words_list : (int * string) list list) (result : string list list) : string list list =
    match words_list with 
        | [] -> result
        | x :: xs -> convert_to_string_list xs (result @ [(convert_to_string_list_nested x [])])
    ;;

(*
 *  walk_dictionary
 *  
 *  Given a dictionary and a word, returns the ordered list of (distance, word) pairs for the input word, 
 *  with distance being the Hamming distance between the word in the sentence and the word in the dictionary.
 *
 *  dictionary            string list                       The dictionary containing the words to search
 *  word                  string                            The word to calculate the Hamming distance from
 *  result                (int * string) list               Temporary buffer used internally by the function
 *
 *  Return                (int * string) list               The resultant (distance, word) pair for the given word
 *
 *  example:
 *  
 *  walk_dictionary "foo" ["foo"; "boo"; "moo"; "batman"] [] --> [[(0, "foo"); (1, "boo"); (1, "moo")];
 *
 *)
let rec walk_dictionary (word : string) (dictionary : string list) (result : (int * string) list) : (int * string) list = 
    match dictionary with
        | [] -> result
        | x :: xs -> let x_char = (list_of_string x)
                     let word_char = (list_of_string word)  

                     if (list_size x_char 0) = (list_size word_char 0) then
                         walk_dictionary word xs (insert_min result ((hamming_distance x_char word_char 0), x))
                     else 
                         walk_dictionary word xs result
    ;;

(*
 *  get_sentence_distances_list
 *  
 *  Given a dictionary and a sentence, returns the list of orderd lists of (distance, word) pairs for every word of the input
 *  sentence, with distance being the Hamming distance between the word in the sentence and the word in the dictionary.
 *  The resultant list is indexed based on the words of the sentence, so the position of the word in the sentence corresponds to
 *  the index in the resultant list (e.g. : "hey how are you" --> [[<pairs for "hey">]; [<pairs for "how">]; [<pairs for "are">]; [<pairs for "you">]]).
 *
 *  dictionary            string list                       The dictionary containing the words to search
 *  words                 string list                       The words of the sentence
 *
 *  Return                (int * string) list list          The resultant (distance, word) pairs for every word in the sentence
 *
 *  example:
 *  
 *  get_sentence_distances_list ["foo"; "boo"; "moo"; "batman"; "fatman"; "madman"] ["foo"; "catman"] --> [[(0, "foo"); (1, "boo"); (1, "moo")]; [(1, "batman"); (1, "fatman"); (2, "madman")]]
 *
 *)
let rec get_sentence_distances_list (dictionary : string list) (words : string list) : (int * string) list list = 
    match words with
        | [] -> []
        | x :: xs -> (walk_dictionary x dictionary []) :: (get_sentence_distances_list dictionary xs)
    ;;

(*
 *  find_min_length_word
 *  
 *  Returns the length of the word whose length is the minimum among all the given words.
 *
 *  words                 string list                       The words to find the minimum length word in
 *
 *  Return                int                               The minimum length
 *
 *  example:
 *  
 *  find_min_length_word ["foo"; "a"; "test"] --> 1
 *
 *)
let rec find_min_length_word (words : string list) : int =
    match words with
        | [] -> failwith "List can't be empty"
        | [x] -> (list_size (list_of_string x) 0)
        | x :: y :: zs -> if (list_size (list_of_string x) 0) < (list_size (list_of_string y) 0) then
                              find_min_length_word (x :: zs)
                          else
                              find_min_length_word (y :: zs)
    ;;

(*
 *  find_max_length_word
 *  
 *  Returns the length of the word whose length is the maximum among all the given words.
 *
 *  words                 string list                       The words to find the maximum length word in
 *
 *  Return                int                               The maximum length
 *
 *  example:
 *  
 *  find_max_length_word ["foo"; "a"; "test"] --> 4
 *
 *)
let rec find_max_length_word (words : string list) : int =
    match words with
        | [] -> failwith "List can't be empty"
        | [x] -> (list_size (list_of_string x) 0)
        | x :: y :: zs -> if (list_size (list_of_string x) 0) >= (list_size (list_of_string y) 0) then
                              find_max_length_word (x :: zs)
                          else
                              find_max_length_word (y :: zs)
    ;;

(*
 *  filter_dictionary_by_lengths
 *  
 *  Filters a given dictionary by excluding all words whose lengths aren't within the specified minimum and maximum bounds.
 *
 *  dictionary            string list                       The dictionary to filter
 *  min                   int                               The lower length bound
 *  max                   int                               The upper length bound
 *  
 *  Return                string list                       The filtered dictionary
 *  
 *  example:
 *  
 *  filter_dictionary_by_lengths ["foo"; "a"; "test"] 1 3 --> ["foo"; "a"]
 *
 *)
let rec filter_dictionary_by_lengths (dictionary : string list) (min : int) (max : int) : string list = 
    match dictionary with
        | [] -> []
        | x :: xs -> let length = (list_size (list_of_string x) 0)
                     if length >= min && length <= max then
                        x :: filter_dictionary_by_lengths xs min max
                     else
                        filter_dictionary_by_lengths xs min max
    ;;

(*
 *  merge_lists
 *  
 *  Merges the second list to the first list.
 *
 *  list_to               string list list                  The list to merge to
 *  list_from             string list                       The list to merge from
 *
 *  Return                string list list                  The result of the merge between the two lists
 *
 *  example:
 *  
 *  merge_lists [["hey"; "hello";]; ["a"]; ["foo"; "bar"]] ["test";"b";"baz"] --> [["hey"; "hello"; "test"]; ["a"; "b"]; ["foo"; "bar"; "baz"]]
 *
 *)
let rec merge_lists (list_to : string list list) (list_from : string list) : string list list =
    match list_to, list_from with
        | [], [] -> []
        | x :: xs, y :: ys when x <> [y] -> (x @ [y]) :: (merge_lists xs ys)
        | x :: xs, y :: ys -> x :: (merge_lists xs ys)
        | _, _ -> []
    ;;

(*
 *  expand
 *  
 *  "Expands" a list by repeating it the number of times specified.
 *
 *  to_expand             string list                       The list to expand
 *  times                 int                               The number of times to expand the list
 *
 *  Return                string list                       The expanded (repeated) list
 *
 *  example:
 *  
 *  expand ["foo"; "bar"; "baz"] 0 --> ["foo"; "bar"; "baz"]
 *  expand ["foo"; "bar"; "baz"] 1 --> ["foo"; "bar"; "baz"; "foo"; "bar"; "baz"]
 *  expand ["foo"; "bar"; "baz"] 2 --> ["foo"; "bar"; "baz"; "foo"; "bar"; "baz"; "foo"; "bar"; "baz"]
 *
 *)
let rec expand (to_expand : string list) (times : int) : string list = 
    if times = 0 then
        to_expand
    else
        to_expand @ (expand to_expand (times - 1))
    ;;

(*
 *  concatenate_to_list
 *  
 *  Concatenates a string (and a space) to each element of the given list, up to the specified position.
 *
 *  list                  string list                       The list to concatenate to
 *  string                string                            The string to concatenate to each element of the list
 *  index                 int                               Internal counter to keep the position, starts from 0
 *  upto                  int                               The position up to which the string will be concatenated
 *
 *  Return                string list                       The list with the concatenated strings
 *
 *  example:
 *  
 *  concatenate_to_list ["foo"; "bar"; "baz"] "1" 0 2   --> ["foo 1"; "bar 1"; "baz"]
 *  concatenate_to_list ["foo"; "bar"; "baz"] "bar" 0 1 --> ["foo bar"; "bar"; "baz"]
 *
 *)
let rec concatenate_to_list (list : string list) (string : string) (index : int) (upto : int) : string list = 
    match list, index with
        | xs, index when index = upto -> []
        | x :: xs, index -> (x + " " + string) :: (concatenate_to_list xs string (index + 1) upto)
        | _ -> []
    ;;

(*
 *  combine_inner
 *  
 *  Inner function to walk the combination lists and concatenate them.
 *
 *  expanded              string list                       The expanded list to concatenate to
 *  list                  string list                       The list to concatenate to the expanded list
 *  length                int                               The length of the "list" parameter, used for the concatenate call
 *
 *  Return                string list                       The concatenated list
 *
 *  example:
 *  
 *  combine_inner ["foo"; "bar"; "baz"; "foo"; "bar"; "baz"] ["a"; "b"] 2 --> ["foo a"; "bar a"; "baz a"; "foo b"; "bar b"; "baz b"]
 *
 *)
let rec combine_inner (expanded : string list) (list : string list) (length : int) : string list = 
    match list with
        | [] -> []
        | x :: xs -> (concatenate_to_list expanded x 0 length) @ (combine_inner expanded xs length)
    ;;

(*
 *  combine
 *  
 *  Cartesian product of a list of words in the (string list list) format.
 *  Prints all combinations of the words in the list, following the order they appear in.
 *
 *  words                 string list list                  The words list, in format [[<words>]; [<words>]; ... ]
 *  temp_list             string list                       Internal buffer used by the function, must be initialized to []
 *
 *  Return                string list                       The cartesian product (list of sentences)
 *
 *  example:
 *  
 *  combine [["foo"; "bar"]; ["baz"];] []        --> ["foo baz"; "bar baz"]
 *  combine [["foo"; "bar"]; ["baz"; "boo"];] [] --> ["foo baz"; "foo boo"; "bar baz"; "bar boo"]
 *
 *)
let rec combine (words : string list list) (temp_list : string list) : string list =
    match words, temp_list with
        | [], _ -> temp_list
        | x :: xs, [] -> (combine xs x)
        | x :: xs, _ -> combine xs (combine_inner (expand temp_list (list_size x 0)) x (list_size temp_list 0))
    ;;

let correct (dictionary : string list) (text : string) : string list = 
    let words = (split_string text)

    let min_length_word = find_min_length_word words

    let max_length_word = find_max_length_word words

    let filtered_dictionary = filter_dictionary_by_lengths dictionary min_length_word max_length_word

    let distances_list = get_sentence_distances_list filtered_dictionary words

    let min_distances = convert_to_string_list distances_list []

    let merged = (merge_lists min_distances words)

    combine merged []
    ;;

(*
 *  substring
 *  
 *  Returns the portion of string specified by the start and length parameters. 
 *
 *  string                string                            The string to find the substring of
 *  start                 int                               The point to start extracting the substring from
 *  length                int                               The number of characters to extract
 *
 *  Return                char list                         The extracted portion of the string (in char list format)
 *
 *  example:
 *  
 *  substring "foobar" 0 3   --> ['f''o';'o']
 *  substring "foobar" 0 500 --> ['f';'o';'o';'b';'a';'r']
 *  substring "foobar" 0 0   --> []
 *  substring "foobar" 0 1   --> ['f']
 *  substring "foobar" 2 3   --> ['o';'b';'a']
 *
 *)
let rec substring (string : string) (start : int) (length : int) : char list =
    let rec substring_nested (string : char list) (counter : int) (index : int) (start : int) (length : int) : char list =
        match string with
            | [] -> []
            | x :: xs when counter >= start && index < length -> x :: substring_nested xs (counter + 1) (index + 1) start length
            | x :: xs -> substring_nested xs (counter + 1) (index) start length

    substring_nested (list_of_string string) 0 0 start length
    ;;

(*
 *  test_prefix
 *  
 *  Tests if a given (lite) regular expression matches the given string.
 *  This lite regular expression can only contain characters and the dot (".") character, which symbolizes "any character" in the F# default coding.
 *
 *  string_input          char list                         The regular expression (in char list format)
 *  string_dict           char list                         The string to test the expression against (in char list format)
 *
 *  Return                bool                              true if the expression matches, false otherwise
 *
 *  example:
 *  
 *  test_prefix "f.." "foo" --> true
 *  test_prefix "..o" "foo" --> true
 *  test_prefix "..o" "f.o" --> true
 *  test_prefix "..o" "f.." --> false
 *  test_prefix "fo." "bar" --> false
 *  test_prefix "foo" "bar" --> false
 *
 *)
let rec test_prefix (string_input : char list) (string_dict : char list) : bool =
    match string_input, string_dict with
        | [], [] -> true
        | x :: xs, y :: ys when x = '.' || (x <> '.' && y = x) -> test_prefix xs ys
        | _ -> false
    ;;

(*
 *  match_regex
 *  
 *  Given a dictionary, finds the words of the dictionary that match the given (lite) regular expression.
 *  Possible wildcard chatacters are the dot (".") and the asterisk ("*") characters. 
 *  The . character symbolizes a single character and can be repeated and appear in any position, 
 *  whereas the * character symbolizes zero or more characters and can only appear once in the 
 *  last position in the string.
 *
 *  regex                 string                            The regular expression
 *  prefix                char list                         The prefix of the regular expression (e.g. if the expression is "f.o*", the prefix is "f.o")
 *  operation             'a                                The mathematical comparison operation to perform (>= or =)
 *  index                 int                               Internal counter used to keep track of the word's position within the dictionary
 *  dictionary            string list                       The dictionary to match the regex against
 *  result                (string * string * int) list      Temporary buffer used for tail-call recursion, must be initialized to []
 *
 *  Return                (string * string * int) list      A list of tuples. Each tuple contains the regular expression, 
 *                                                          the matched word and the position of the matched word in the dictionary
 *
 *  example:
 *  
 *  match_regex "f.o*" "f.o" >= 0 ["foo"; "foobar"; "test"; "frobar"] [] --> [("f.o*", "foo", 0); ("f.o*", "foobar", 1)]
 *
 *)
let rec match_regex (regex : string) (prefix : char list) (operation) (index : int) (dictionary : string list) (result : (string * string * int) list) : (string * string * int) list =
    match dictionary with
        | [] -> result
        | x :: xs when operation (list_size (list_of_string x) 0) (list_size prefix 0) -> 
                        if test_prefix prefix (substring x 0 (list_size prefix 0)) then
                           (match_regex regex prefix operation (index + 1) xs (result @ [(regex, x, index)]))
                        else
                           (match_regex regex prefix operation (index + 1) xs result)
        | x :: xs -> (match_regex regex prefix operation (index + 1) xs result)
    ;;

(*
 *  print_whole_dictionary
 *  
 *  Formats the whole given dictionary, used to maximise the efficency of "*" input in "find" function.
 *
 *  dictionary            string                            The dictionary to print
 *  index                 int                               Internal counter used to keep track of the word's position within the dictionary
 *  result                (string * string * int) list      A list of tuples. Each tuple contains the regular expression,
 *                                                          the matched word and the position of the matched word in the dictionary
 *
 *  Return                (string * string * int) list      A list of tuples. Each tuple contains the regular expression, 
 *                                                          the matched word and the position of the matched word in the dictionary
 *
 *  example:
 *  
 *  print_whole_dictionary ["foo"; "foobar"; "test"; "frobar"] 0 [] --> [("*", "foo", 0); ("*", "foobar", 1); ("*", "test", 2); ("*", "frobar", 3)]
 *
 *)
let rec print_whole_dictionary (dictionary : string list) (index : int) (result : (string * string * int) list) : (string * string * int) list =
    match dictionary with
        | [] -> result
        | x :: xs -> (print_whole_dictionary xs (index + 1) (result @ [("*", x, index)]))
    ;;

(*
 *  check_valid_wildcards
 *  
 *  Checks if the given regular expression is valid by the following rules:
 *  The . character be repeated and appear in any position, whereas the * character can only appear once in the last position in the string.
 *
 *  regex                 char list                         The regex to check (in char list format)
 *
 *  Return                bool                              true if the regex is valid, false otherwise
 *
 *  example:
 *  
 *  check_valid_wildcards ['f';'.';'o';'*'] --> true
 *  check_valid_wildcards ['*';'.';'o';'*'] --> false
 *  check_valid_wildcards ['.';'.';'.']     --> true
 *  check_valid_wildcards ['*']             --> true
 *
 *)
let rec check_valid_wildcards (regex : char list) : bool = 
    match regex with
        | [] -> false
        | [x] -> true
        | x :: xs when x = '*' -> false
        | x :: xs -> check_valid_wildcards xs
    ;;

let find (w : string) (ws : string list) : (string * string * int) list =
    if w = "*" then
        print_whole_dictionary ws 0 []
    else
        let regex = list_of_string w
        if check_valid_wildcards regex then
            if item_at regex ((list_size regex 0) - 1) = '*' then
                match_regex w (substring w 0 ((list_size regex 0) - 1)) (>=) 0 ws []
            else
                match_regex w regex (=) 0 ws []
        else
            []
    ;;

// MAIN - YOU SHALL NOT TOUCH
[<EntryPoint>]
let main args = 
  let uncurry2 f = fun (x, y) -> f x y
  let uncurry3 f = fun (x, y, z) -> f x y z
  let dictionary = 
    (fun () ->
      do printfn "Select the dictionary:\n\t1\t1000 words dictionary.\n\t2\t60000 words dictionary.\n\tother\tenter the words separated by space here."
      let choice = System.Console.ReadLine()
      match choice with 
      | "1" -> System.IO.File.ReadAllLines "dict1000.txt" |> List.ofArray
      | "2" -> System.IO.File.ReadAllLines "dict60000.txt" |> List.ofArray
      | words ->  words.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray)()
  do printfn "Select the function:\n\t1\tcorrect\n\t2\tsearch\n\tother\texit"
  let choice = System.Console.ReadLine()
  let body_find dictionary = 
    do printfn "Enter the string to search. (Empty to exit)"
    let search = System.Console.ReadLine()
    if search <> "" then
      do printfn "Found..."
      let found = find search dictionary
      if found = [] then
          printfn "Found no occurrences of %s" search
      else do List.iter (uncurry3 (printfn "Searched: %s; found: %s; at: %d")) found
      true
    else false
  let body_correct dictionary = 
    do printfn "Enter the string message. (Empty to exit)"
    let msg = System.Console.ReadLine()
    if msg <> "" then
      let choices = correct dictionary msg
      let choices = choices |> Set.ofList |> List.ofSeq |> List.sort |> List.map (fun x -> (msg, x))
      do List.iter (uncurry2 (printfn "%s: %s")) choices
      true
    else false      
  let rec body f dictionary = 
    if f dictionary then      
      body f dictionary
    else
      printfn "Quit..."
  do body (match choice with "1" -> body_correct | "2" -> body_find | _ -> fun _ -> false) dictionary
  do printf "\nPress any key to continue . . . "
  do System.Console.ReadKey() |> ignore
  0