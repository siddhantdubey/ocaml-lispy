type token =
    | LParen
    | RParen
    | Symbol of string
    | Number of float
    | String of string

let rec tokenize (s : string) : token list =
    let s = if s.[String.length s - 1] = ' ' then s else s ^ " " in
    let rec next_space_or_paren s i =
        if i >= String.length s then i
        else
            match s.[i] with
            | ' ' | '(' | ')' -> i
            | _ -> next_space_or_paren s (i + 1)
    in 
    let rec tokenize' (s : string) (i : int) : token list =
        if i >= String.length s then []
        else
            match s.[i] with
            | '(' -> LParen :: tokenize' s (i + 1)
            | ')' -> RParen :: tokenize' s (i + 1)
            | ' ' -> tokenize' s (i + 1)
            | '"' ->
                let j = String.index_from s (i + 1) '"' in
                let str = String.sub s (i + 1) (j - i - 1) in
                String str :: tokenize' s (j + 1)
            | _ ->
                let j = next_space_or_paren s i in
                let str = String.sub s i (j - i) in
                let token =
                    try Number (float_of_string str)
                    with Failure _ -> Symbol str
                in
                token :: tokenize' s j
    in
    tokenize' s 0

type atom =
    | Number of float
    | Symbol of string
    | String of string

type expr =
    | Atom of atom
    | List of expr list
    | Fun of (expr list -> expr)

let rec parse_exprs tokens =
    match tokens with
    | [] | RParen :: _ -> ([], tokens)
    | _ ->
        let (expr, rest) = parse_expr tokens in
        let (exprs, final_rest) = parse_exprs rest in
        (expr :: exprs, final_rest)

and parse_expr tokens =
    match tokens with
    | [] -> failwith "Unexpected EOF"
    | Number n :: rest -> (Atom (Number n), rest)
    | Symbol s :: rest -> (Atom (Symbol s), rest)
    | String s :: rest -> (Atom (String s), rest)
    | LParen :: rest ->
            let (exprs, tokens_after_exprs) = parse_exprs rest in
            (match tokens_after_exprs with
            | RParen :: rest -> (List exprs, rest)
            | _ -> failwith "Missing a )")
    | RParen :: _ -> failwith "Unexpected ')'"

type env = (string * expr) list

let rec lookup env symbol =
    match env with
    | [] -> failwith ("Unbound symbol: " ^ symbol)
    | (sym, value) :: _ when sym = symbol -> value
    | _ :: rest -> lookup rest symbol

let extend env symbol value =
    (symbol, value) :: env

let initial_env () =
    let builtin name f = (name, Fun f)
    in
    ("nil", List [])  ::
    [
        builtin "+" (function
            | Atom (Number a) :: tl -> 
                let numbers = List.map (function Atom (Number n) -> n | _ -> failwith "Type error") tl in
                Atom (Number (a +. List.fold_left (+.) 0. numbers))
            | _ -> failwith "Type error"
        );
        builtin "-" (function
            | Atom (Number a) :: tl ->
                let numbers = List.map (function Atom (Number n) -> n | _ -> failwith "Type error") tl in
                Atom (Number (a -. List.fold_left (+.) 0. numbers))
            | _ -> failwith "Type error"
        );
        builtin "*" (function
            | [Atom (Number a); Atom (Number b)] -> Atom (Number (a *. b))
            | _ -> failwith "Type error"
        );
        builtin "/" (function
            | [Atom (Number a); Atom (Number b)] -> Atom (Number (a /. b))
            | _ -> failwith "Type error"
        );
        builtin "eq" (function
            | [Atom a; Atom b] -> Atom (Symbol (string_of_bool (a = b)))
            | _ -> failwith "Type error"
        );
        builtin ">" (function
            | [Atom (Number a); Atom (Number b)] -> Atom (Symbol (string_of_bool (a > b)))
            | _ -> failwith "Type error"
        );
        builtin "<" (function
            | [Atom (Number a); Atom (Number b)] -> Atom (Symbol (string_of_bool (a < b)))
            | _ -> failwith "Type error"
        );
        builtin ">=" (function
            | [Atom (Number a); Atom (Number b)] -> Atom (Symbol (string_of_bool (a >= b)))
            | _ -> failwith "Type error"
        );
        builtin "<=" (function
            | [Atom (Number a); Atom (Number b)] -> Atom (Symbol (string_of_bool (a <= b)))
            | _ -> failwith "Type error"
        );
        builtin "car" (function
            | [List (h :: _)] -> h
            | _ -> failwith "Type error"
        );
        builtin "cdr" (function
            | [List (_ :: tl)] -> List tl
            | _ -> failwith "Type error"
        );
        builtin "cons" (function
            | [h; List tl] -> List (h :: tl)
            | _ -> failwith "Type error"
        );
    ]

let rec eval env = function
    | Atom (Number n) -> Atom (Number n)
    | Atom (Symbol s) -> lookup env s
    | Atom (String s) -> Atom (String s)
    | List ((Atom (Symbol s)) :: args) -> apply (lookup env s) (List.map (eval env) args)
    | _ -> failwith "Invalid expression"

and apply func args =
    match func with
    | Fun f -> f args
    | _ -> failwith "Not a function"

let rec string_of_expr = function
    | Atom (Number n) -> string_of_float n
    | Atom (Symbol s) -> s
    | Atom (String s) -> "\"" ^ s ^ "\""
    | List exprs -> "(" ^ String.concat " " (List.map string_of_expr exprs) ^ ")"
    | _ -> failwith "Invalid expression"

let test_evaluator () =
    let env = initial_env () in
    let test s =
        let tokens = tokenize s in
        let (expr, _) = parse_expr tokens in
        let result = eval env expr in
        Printf.printf "Result: %s\n" (string_of_expr result)
    in
    test "(+ 1 2 3)";
    test "(- 10 3 2)";
    test "(* 5 5)";
    test "(/ 10 2)";
    test "(eq 1 1)";
    test "(car (cons 1 (cons 2 (cons 3 nil))))";
    test "(cdr (cons 1 (cons 2 (cons 3 nil))))";
    test "(cons 1 (cons 2 (cons 3 nil)))"

let () = test_evaluator ()

