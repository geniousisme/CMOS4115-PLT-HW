{ type token = EOF | Word of string }

rule token = parse
    | eof { EOF }
    | ['a'-'z' 'A'-'Z']+ as word { Word(word) }
    | _ { token lexbuf }


{
    module StringMap = Map.Make(String);;

    let lexbuf = Lexing.from_channel stdin in
    let wordlist =
        let rec next l = match token lexbuf with
                            EOF -> l
                            | Word(s) -> next (s :: l)
        in next []
    in
    (* Build the word map *)
    let word_count_map =
        let rec word_count_helper word_map = function
            | []       -> word_map
            | hd :: tl -> word_count_helper
                          (if not (StringMap.mem hd word_map) then StringMap.add hd 1 word_map
                           else StringMap.add hd ((StringMap.find hd word_map) + 1) word_map) tl
            in  word_count_helper StringMap.empty wordlist
    in
    (* Fold the word_count_map to tuple list with count first, word in the second position *)
    let count_word_list = StringMap.fold
                                    (fun word count res_lst -> (count, word) :: res_lst) word_count_map []
    in
    (* Sort the count_word_list *)
    let wordcounts =
        List.sort (fun (c1, _) (c2, _) -> Pervasives.compare c2 c1) count_word_list
    in
    (* Print out all results with assigned format *)
    List.iter (fun count_word_tuple -> print_endline (string_of_int(fst count_word_tuple) ^ " " ^ (snd count_word_tuple))) wordcounts
}