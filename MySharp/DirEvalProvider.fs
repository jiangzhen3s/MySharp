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

//eval absyn 
let eval stmt = 
    match stmt with
    | Select(exprs, dirs, Some(whereExprs)) -> ()
    | Select(exprs, dirs, None) -> ()
    | Set(name, str) -> ()
(*
stmt examples:

SET c <- 'C:\\Temp'
SELECT * FROM c --query all properties about directory info 
SELECT * FROM c WHERE c.Name LIKE '%test%' --search directory

*)
