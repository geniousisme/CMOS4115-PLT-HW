(*
    1.
    Write a function that subtracts positive integers represented
    as lists of decimal digits. For example,

    subl [2;5;3] [5;7] = [1;9;6]
    subl [1;0;0;0;0;0;0;0;0;0;0;0] [4;2;0;0;0;0;0;0;0;0;0] =
    [0;5;8;0;0;0;0;0;0;0;0;0]
    subl [1] [0];;
    subl [1; 0; 0] [1];;

    Your algorithm may assume the first number is larger
    than the second. Arbitrary-precision arithmetic packages
    use a similar technique but with a much larger radix.
*)

let subl1 lst1 lst2 =
    (* this method will make the result exloded *)
    (* list_to_int: transfer int list into a int number first *)
    let list_to_int lst =
         List.fold_left (fun a b -> 10 * a + b) 0 lst
    in
    (* int_to_list: transfer the int number into int list *)
    let rec int_to_list target_length target_int res_list =
        if target_int = 0 && target_length = 0 then 0 :: res_list
        else if target_int < 10 && target_length = 0 then target_int :: res_list
        else if target_int < 10 && target_length > 0 then int_to_list (target_length - 1) 0 (target_int :: res_list)
        else int_to_list (target_length - 1) (target_int / 10) ((target_int mod 10) :: res_list)
    in
    (* subtract two int value first, then transfer the int value into list *)
    int_to_list (List.length lst1 - 1) ((list_to_int lst1) - (list_to_int lst2)) []
;;

let subl lst1 lst2 = 
    let rec add_zero_before_list zero_count lst =
        if zero_count = 0 then lst
        else add_zero_before_list (zero_count - 1) (0 :: lst)
    in
    let lst_subl lst1 lst2 =
        List.map2 (fun a b -> a - b) lst1 lst2
    in
    let build_lst lst =
        let rec lst_scan res_lst carry = function
            | [] -> res_lst 
            | hd :: tl -> 
                        if hd - carry < 0 then lst_scan ((hd - carry + 10) :: res_lst) 1 tl
                        else lst_scan ((hd - carry) :: res_lst) 0 tl
        in lst_scan [] 0 (List.rev lst)
    in build_lst (lst_subl lst1 (add_zero_before_list ((List.length lst1) - (List.length lst2)) lst2))
;;

let () =
    assert(subl [1] [0] = [1]);
    assert(subl [1; 0; 0; 0] [1] = [0; 9; 9; 9]);
    assert(subl [2; 5; 3] [5; 7] = [1; 9; 6]);
    assert(subl [1;0;0;0;0;0;0;0;0;0;0;0] [4;2;0;0;0;0;0;0;0;0;0] = [0; 5; 8; 0; 0; 0; 0; 0; 0; 0; 0; 0])
;;
(*
    To test my code, first I run following on my terminal:

    $ocaml

    # let subl lst1 lst2 =
        (* list_to_int: transfer int list into a int number first *)
        let list_to_int lst =
             List.fold_left (fun a b -> 10 * a + b) 0 lst in
        (* int_to_list: transfer the int number into int list *)
        let rec int_to_list target_length target_int res_list =
            if target_int = 0 && target_length = 0 then 0 :: res_list
            else if target_int < 10 && target_length = 0 then target_int :: res_list
            else if target_int < 10 && target_length > 0 then int_to_list (target_length - 1) 0 (target_int :: res_list)
            else int_to_list (target_length - 1) (target_int / 10) ((target_int mod 10) :: res_list) in
        (* subtract two int value first, then transfer the int value into list *)
        int_to_list (List.length lst1 - 1) ((list_to_int lst1) - (list_to_int lst2)) []
    ;;
                            val subl : int list -> int list -> int list = <fun>

    # subl [1] [0];;
    - : int list = [1]
    # subl [1] [0];;
    - : int list = [1]
    # subl [1; 0; 0; 0] [1];;
    - : int list = [0; 9; 9; 9]
    # subl [2; 5; 3] [5; 7];;
    - : int list = [1; 9; 6]
    # subl [1;0;0;0;0;0;0;0;0;0;0;0] [4;2;0;0;0;0;0;0;0;0;0];;
    - : int list = [0; 5; 8; 0; 0; 0; 0; 0; 0; 0; 0; 0]

    then I use assert to run my code directly:

    $ ocaml subtract/subtract.ml
    $

    No Exception Error, yeah!
*)