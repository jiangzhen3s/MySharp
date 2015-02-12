//namespace test.MySharpLexerTest
module MySharpLexerTest

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit.MsTest
open NHamcrest.Core
open System
open System.IO
open Absyn
open Microsoft.FSharp.Text

(* Plain parsing from a string, with poor error reporting *)

let addEscapeSlash (str : string) = str.Replace(@"\", @"\\\\")

let showAllToken lexbuf = 
    let rec loop token tokens = 
        match token with
        | UsqlPar.token.EOF -> List.rev tokens
        | any -> loop (UsqlLex.Token lexbuf) (any :: tokens)
    
    let token = UsqlLex.Token lexbuf
    loop token []

let fromStringLex (str : string) = 
    let str' = str |> addEscapeSlash
    let lexbuf = Lexing.LexBuffer<char>.FromString(str')
    showAllToken lexbuf

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

[<TestClass>]
type ``lexer test: like token test ``() = 
    
    [<TestMethod>]
    member test.``test lexer: like keyword``() = 
        let src = "SELECT * FROM A WHERE C LIKE '%abc%'"
        let tokens = fromStringLex src
        tokens |> should equal [ UsqlPar.SELECT
                                 UsqlPar.TIMES
                                 UsqlPar.FROM
                                 UsqlPar.NAME("a")
                                 UsqlPar.WHERE
                                 UsqlPar.NAME("c")
                                 UsqlPar.LIKE
                                 UsqlPar.CSTSTRING("%abc%") ]
    
    [<TestMethod>]
    member test.``test parser: like keyword``() = 
        let src = "SELECT * FROM A WHERE C LIKE '%abc%'"
        let stmt = fromString src
        stmt |> should equal (Select([ Star ], [ "a" ], Some([ Like(ColumnExpr(Column("c")), "%abc%") ])))
