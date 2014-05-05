type file_position = string * int;;

type token  = Invalid of file_position
            | Open of file_position
            | Close of file_position
            | Literal of string * file_position
;;

exception ParseError of string;;

let tokenize file_name =
    let chan =
        match file_name with
        | "" -> stdin
        | _ -> open_in file_name
    in
    let rec process line s =
        let has_str = String.length s > 0 in
        let pos = (file_name, line) in
        let handle_eol () =
            let line = line + 1 in
            if not has_str then process line ""
            else Literal (s, pos) :: process line ""
        in
        let rec skip () =
            match input_char chan with
            | '\n' -> handle_eol ()
            | _ -> skip ()
        in
        try
            match input_char chan with
            | '(' when not has_str ->
                    Open pos :: process line ""
            | '(' when has_str ->
                    Literal (s, pos) :: Open pos :: process line ""
            | ')' when not has_str -> Close pos :: process line ""
            | ')' when has_str ->
                    Literal (s, pos) :: Close pos :: process line ""
            | ';' -> skip ()
            | ' ' | '\t' | '\r' ->
                    if not has_str then process line ""
                    else Literal (s, pos) :: process line ""
            | '\n' -> handle_eol ()
            | ch -> process line (s ^ String.make 1 ch)
        with End_of_file ->
            if has_str then [Literal (s, pos)] else []
    in
    let result = process 1 "" in
    close_in_noerr chan; result
;;

let peek_token token_list = List.hd token_list ;;

let get_position = function
    | Invalid pos -> pos
    | Open pos -> pos
    | Close pos -> pos
    | Literal (_, pos) -> pos
;;

let parse_error token_list msg =
    let (file_name, line) = match token_list with
    | t :: _ -> get_position t
    | _ -> ("<EOF>", 0) in
    let prefix = file_name ^ "[" ^ (string_of_int line) ^ "]: " in
    raise @@ ParseError (prefix ^ msg)

let match_open = function
    | Open _ :: tl -> tl
    | tl -> parse_error tl "expected '('"
;;

let match_close = function
    | Close _ :: tl -> tl
    | tl -> parse_error tl "expected ')'"
;;

let match_string = function
    | Literal (s, _) :: tl -> (s, tl)
    | tl -> parse_error tl "expected literal"
;;

let match_literal name = function
    | Literal (name, _) :: tl -> tl
    | tl -> parse_error tl ("expected " ^ name)
;;

let match_int token_list =
    let s, tl = match_string token_list in (int_of_string s, tl)
;;

let match_bool = function
    | Literal ("true", _) :: tl -> (true, tl)
    | Literal ("false", _) :: tl -> (false, tl)
    | tl -> parse_error tl "expected 'true' or 'false'"
;;

let rec tokens_to_string = function
    | Open _ :: tl -> "(" ^ (tokens_to_string tl)
    | Close _ :: tl -> ")" ^ (tokens_to_string tl)
    | Literal (s, _) :: tl -> s ^ " " ^ (tokens_to_string tl)
    | _ -> "\n"
;;
