type token  = Invalid
            | Open
            | Close
            | Literal of string
;;

let tokenize chan =
    let rec process s =
        let has_str = String.length s > 0 in
        try
            match input_char chan with
            | '(' when not has_str -> Open :: process ""
            | '(' when has_str -> (Literal s) :: Open :: process ""
            | ')' when not has_str -> Close :: process ""
            | ')' when has_str -> (Literal s) :: Close :: process ""
            | ' ' | '\t' | '\r' | '\n'  ->
                    if not has_str then process ""
                    else (Literal s) :: process ""
            | ch -> process (s ^ String.make 1 ch)
        with End_of_file ->
            if has_str then [Literal s] else []
    in process ""
;;

let peek_token token_list = List.hd token_list ;;

let match_open = function
    | Open :: tl -> tl
    | _ -> failwith "expected '('"
;;

let match_close = function
    | Close :: tl -> tl
    | _ -> failwith "expected ')'"
;;

let match_string = function
    | Literal s :: tl -> (s, tl)
    | _ -> failwith "expected literal"
;;

let match_literal name = function
    | Literal name :: tl -> tl
    | _ -> failwith @@ "expected " ^ name
;;

let match_int token_list =
    let s, tl = match_string token_list in (int_of_string s, tl)
;;

let match_bool token_list =
    let s, tl = match_string token_list in
    match s with
    | "true" -> (true, tl)
    | "false" -> (false, tl)
    | _ -> failwith "expected 'true' or 'false'"
;;

let rec tokens_to_string = function
    | Open :: tl -> "(" ^ (tokens_to_string tl)
    | Close :: tl -> ")" ^ (tokens_to_string tl)
    | Literal s :: tl -> s ^ " " ^ (tokens_to_string tl)
    | _ -> "\n"
;;
