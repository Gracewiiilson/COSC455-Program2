
(* Parser Sample *)

(* NOTE: This example uses generic OCaml compatable syntax where the syntax where possible, which is
 * in most places!  There *are* LIBRARY differences between F# and OCaml that necessitate slighly
 * different functions to be used for input and output.
 *
 * If using OCaml instead of F#, check out: https://learnxinyminutes.com/docs/ocaml/
*)

// Grammar for Project 2
// program : stmt_list
// stmt_list : stmt stmt_list | ε
// stmt : assignment | read_stmt | write_stmt | for_loop | if_stmt
// expr : term term_tail
// term_tail : add_op term term_tail | ε
// term : factor factor_tail
// factor_tail : mult_op factor factor_tail | ε
// factor : ( expr ) | id
// add_op : - | +
// mult_op : * | /
// rel_oper : > | < | ==
// cond : expr rel_oper expr
// assignment : if := expr
// read_stmt : read id
// write_stmt : write expr
// if_stmt : if cond then stmt_list else_stmt
// else_stmt : else stmt_list fi | fi
// for_loop : for id = id to id step_stmt do stmt_list done
// step_stmt : step id | ε

// Tokens
type Token =
    | Add_op of string
    | Mult_op of string
    | Rel_oper of string
    | Read of string
    | Write of string
    | If of string
    | Then of string
    | Else of string
    | Fi of string
    | For of string
    | To of string
    | Do of string
    | Done of string
    | Step of string
    | Lpar of string
    | Rpar of string
    | Otherequals of string
    | ID of string

    with static member tokenFromLexeme str = // Function to get a token from a lexeme (String)
            match str with
            | "(" -> Lpar str
            | ")" -> Rpar str
            | "+" | "-" -> Add_op str
            | "*" | "/" -> Mult_op str
            | ">" | "<" | "==" -> Rel_oper str
            | "read" -> Read str
            | "write" -> Write str
            | "if" -> If str
            | "then" -> Then str
            | "else" -> Else str
            | "fi" -> Fi str
            | "for" -> For str
            | "to" -> To str
            | "do" -> Do str
            | "done" -> Done str
            | "step" -> Step str
            | ":=" -> Otherequals str
            | unknown -> ID str




////////////////////////////////////////////////////////////////////////////////////////////////////
//
// The following is but one of many possible structures. In fact F#/Ocaml has many
// features that make parsing complex grammars pretty easy... but... to understand those requires a
// much deeper understanding of the language than we have explored.  Unfortunately, the result is
// that the code will not be nearly as concise or elegant as it could otherwise be. However, if you
// which to explore the additional features of the language, feel free to explore!!!
//
//////////////////////////////////////////////////////////////////////////////////////////////////////

// NOTE: A Better code approach MIGHT BE to use "Active Patterns", but those are a little more
// difficult to understand, especially while still trying to grasp "static patterns".
// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/active-patterns
// https://fsharpforfunandprofit.com/posts/convenience-active-patterns/



// NOTES:
// The |> operator sends (pipes) the output of one function directly to the next one in line.
// "and" just allows multiple, mutually recursive functions to be defined under a single "let"

let rec parse theList = program theList

// program ::= stmt_list
and program lst = 
    lst |> stmt_list

//stmt_list ::= stmt stmt_list | <empty>
and stmt_list lst = 
    match lst with 
    | x :: xs when (isFirstStmt x) ->  lst |> stmt |> stmt_list
    | _ -> lst

and isFirstStmt = function
    | Read _ | Write _ | ID _ | For _ | If _ -> true
    | _  -> false            


// stmt ::= id := expr | read id | write expr | for_loop | if_stmt
and stmt = function
    | ID theID :: Otherequals _ :: xs -> xs |> expr
    | Read theRead :: ID _ :: xs -> xs
    | Write theWrite :: xs -> xs |> expr
    | For theFor :: xs -> [] //|> for_loop
    | If theIF :: xs -> [] //|> if_stmt
    | _ -> failwith ""

// expr ::= term term_tail
and expr lst =
    lst |> term |> term_tail

// term_tail ::= add_op term term_tail | ε
and term_tail = function
    | Add_op theAddop :: xs -> xs |> term |> term_tail
    | xs -> xs

// term ::= factor factor_tail
and term lst = 
    lst |> factor |> factor_tail

// factor_tail ::= mult_op factor factor_tail | ε
and factor_tail = function
    | Mult_op theMultop :: xs -> xs |> factor |> factor_tail
    | xs -> xs

// factor ::= ( expr ) | id
and factor = function
    | Lpar _ :: xs -> xs |> expr |> function Rpar _ :: xs -> xs | _ -> failwith $"Expected right parentheses, but found: %A{xs}"
    | ID theID :: xs -> xs
    | x :: xs -> failwithf $"Expected factor, but found: %A{x}"
    | [] -> failwith "Factor should not be empty"


(* Begin Parsing Process *)
let startParsing (str:string) =
    // Split the String (which creates an Array) -> convert the Array to a List -> MAP the list of strings into a list of Tokens.
    // (Note, though arrays are a lot like lists, lists are a bit easier to use for the pattern matching.)
    let tokenList =
        str.Split ' ' |>
        Array.toList |>
        List.map Token.tokenFromLexeme

    // Display our list of tokens... just for fun.
    printfn $"The initial String: %s{str}"
    printfn $"The initial List: %A{tokenList}\n"

    // Work the magic...
    try
        let parsedList = program tokenList
        in printfn $"The Final List:\n\t%A{parsedList}"
    with
        Failure msg -> printfn $"Error: %s{msg}";  System.Console.ReadLine () |> ignore




(* Get the user input and start parsing *)
// NOTE: To make the let assihnment be a function that accepts no parameters,
// an "empty tuple" must be accepted.
let promptAndGo () =
    (* TEST DATA *)
    // let userInput = "the fast , fast dog chases the fast cat ."
    // let userInput = "the noun chases the adj cat and the adj , adj , fast dog adv chases the cat prep a adj noun ."

    let userInput =
        printf "Enter String: ";
        // A case where it's easier to use the .NET ReadLine as opposed to the more restrictive OCaml native variant.
        System.Console.ReadLine ()

    in startParsing userInput


// RUN INTERACTIVELY AS A SCRIPT
promptAndGo ()

// Uncomment the following to pause at the end if it's running in a terminal which dissapears upon running.
// System.Console.ReadKey(true) |> ignore
