(* Lexing and parsing of micro-SQL SELECT statements using fslex and fsyacc *)

open System
open System.IO
open Absyn
open Microsoft.FSharp.Text

(* Plain parsing from a string, with poor error reporting *)

let addEscapeSlash (str : string) = str.Replace(@"\", @"\\\\")

let fromString (str : string) : stmt = 
    let str' = str |> addEscapeSlash
    let lexbuf = Lexing.LexBuffer<char>.FromString(str')
    try  
        UsqlPar.Main UsqlLex.Token lexbuf
    with exn -> 
        let pos = lexbuf.EndPos
        failwithf "%s near line %d, column %d\n" (exn.Message) (pos.Line + 1) pos.Column

(* Parsing from a file *)

let fromFile (filename : string) = 
    use reader = new StreamReader(filename)
    let lexbuf = Lexing.LexBuffer<char>.FromTextReader reader
    try 
        UsqlPar.Main UsqlLex.Token lexbuf
    with exn -> 
        let pos = lexbuf.EndPos
        failwithf "%s in file %s near line %d, column %d\n" (exn.Message) filename (pos.Line + 1) pos.Column

(* Exercise it *)
let showAllToken lexbuf = 
    let rec loop token tokens = 
        match token with
        | UsqlPar.token.EOF -> List.rev tokens
        | any -> loop (UsqlLex.Token lexbuf) (any :: tokens)
    
    let token = UsqlLex.Token lexbuf
    loop token []

//let e1 = fromString "SELECT name, salary * (1 - taxrate) FROM Employee"
//printfn "%A" e1
//let e2 = fromString @"SET a <- 'c:\temp' "
//printfn "%A" e2
//
//match e2 with
//| Set(vname,vvalue) -> Directory.GetDirectories(vvalue) |> Array.length |> printfn "%d"
let src = "SET A<-'c:\\haha'"
let stmt = fromString (src)

printfn "%A" stmt
//let tokenlist = showAllToken token
//printfn "%A" tokenlist
