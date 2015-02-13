[<AutoOpen>]
module DirEvalProvider

open Absyn
open System.IO
open System.Text
open System.Collections.Generic

//properties env
let propEnv = new Dictionary<string, Dictionary<string, string>>()
// var env
let varEnv : (string * string) list = []
let setVar name value varenv = (name, value) :: varenv
let getVar name varenv = List.tryFind (fun (name', v) -> name = name') varenv
//get directory info
let getDirInfo path = new DirectoryInfo(path)
//get sub dirs
let getSubDirs currentDirPath = Directory.GetDirectories(currentDirPath) |> Array.map getDirInfo

// get dir all public properties
let getAllValue obj = 
    let t = obj.GetType()
    t.GetProperties() |> Array.map (fun p -> (p.Name.ToLower(), p.GetValue(obj).ToString()))

// find properties values
let filterValue names nameValues = 
    nameValues |> Array.filter (fun (name, value) -> 
                      match names |> Array.tryFind (fun _name -> _name = name) with
                      | Some(_) -> true
                      | None -> false)

// get dir info properties
let getValues names obj = getAllValue obj |> filterValue names

//initial env 
let getDirs (tableName : string) varEnv = 
    let dirs = 
        match getVar tableName varEnv with
        | Some(name, fullName : string) -> 
            // if path end with \ then get all sub dirs
            if fullName.EndsWith("\\") then getSubDirs (fullName) |> List.ofArray
            else [ new DirectoryInfo(fullName) ]
        | None -> []
    tableName, dirs

let initalDirEnv dirs varenv = dirs |> List.map (fun dir -> getDirs dir varenv)
let evalSetExpr = ()

let evalWhereExpr expr (tableEnv : (string * DirectoryInfo list) list) = 
    let rec isSatisfy expr' dirinfo = 
        match expr' with
        | Cst(cst) -> cst = CstB(true)
        | Prim(op, exprs) -> 
            match op with
            | "!" -> not (isSatisfy (exprs.Item(0)) dirinfo)
            | "&&" -> (isSatisfy (exprs.Item(0)) dirinfo) && (isSatisfy (exprs.Item(1)) dirinfo)
            | "||" -> (isSatisfy (exprs.Item(0)) dirinfo) || (isSatisfy (exprs.Item(1)) dirinfo)
            | _ -> failwith "unsupport where logic operactor"
        | Like(expr', ptn) -> true
        | _ -> failwith "unsupport where expr."
    tableEnv
    |> List.map (fun (tname, dirs) -> dirs |> List.filter (isSatisfy expr))
    |> List.fold (fun acc dirs -> dirs @ acc) []

//eval absyn 
let eval stmt varenv = 
    match stmt with
    | Select(exprs, dirs, Some(whereExpr)) -> 
        let direnv = initalDirEnv dirs varenv
        let dirlist = evalWhereExpr whereExpr direnv
        ()
    | Select(exprs, dirs, None) -> ()
    | Set(name, str) -> ()
(*
stmt examples:
SET allsubdirs <- 'C:\\Temp\\'
SET c <- 'C:\\Temp'
SELECT * FROM c --query all properties about directory info 
SELECT * FROM c WHERE c.Name LIKE '%test%' --search directory

*)
