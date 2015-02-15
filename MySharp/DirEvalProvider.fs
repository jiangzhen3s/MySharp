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

let evalExpr expr varenv (direnv : (string * DirectoryInfo list) list) = 
    match expr with
    | Star -> 
        direnv
        |> List.map (fun (tn, dirs) -> dirs)
        |> List.map (fun dirinfo -> getAllValue dirinfo |> List.ofArray)
    | Cst(cst) -> 
        let v = 
            match cst with
            | CstI(i) -> i.ToString()
            | CstB(b) -> b.ToString()
            | CstS(s) -> s
        direnv
        |> List.map (fun (tn, dirs) -> dirs)
        |> List.map (fun _ -> [ ("ConstValue", v) ])
    | ColumnExpr(col) -> 
        //let getColumn col =
        match col with
        | Column(cname) -> 
            direnv
            |> List.map (fun (tn, dirs) -> dirs)
            |> List.map (fun dirinfo -> getAllValue dirinfo |> List.ofArray)
            |> List.map (fun dirs -> [ dirs |> List.find (fun (colname, value) -> colname = cname) ])
        | TableColumn(tname, cname) -> 
            direnv
            |> List.filter (fun (tn, _) -> tn = tname)
            |> List.map (fun (_, dirs) -> dirs)
            |> List.map (fun dirinfo -> getAllValue dirinfo |> List.ofArray)
            |> List.map (fun dirs -> [ dirs |> List.find (fun (colname, value) -> colname = cname) ])
    | _ -> failwith "unimplement expr eval,Stay tuned ."

//eval absyn ,return new varenv
let eval stmt varenv = 
    match stmt with
    | Select(exprs, dirs, None) -> 
        let direnv = initalDirEnv dirs varenv
        let vs = exprs |> List.map (fun expr -> evalExpr expr varenv direnv)
        varenv
    //    | Select(exprs, dirs, Some(whereExpr)) -> 
    //        let direnv = initalDirEnv dirs varenv
    //        let dirlist = evalWhereExpr whereExpr direnv
    //        exprs |> List.map (fun expr -> evalExpr expr varenv direnv)
    //        varenv
    | Set(name, str) -> (name, str) :: varenv
    | _ -> failwith "Stay tuned."
(*
stmt examples:
SET allsubdirs <- 'C:\\Temp\\'
SET c <- 'C:\\Temp'
SELECT * FROM c --query all properties about directory info 
SELECT * FROM c WHERE c.Name LIKE '%test%' --search directory

*)
