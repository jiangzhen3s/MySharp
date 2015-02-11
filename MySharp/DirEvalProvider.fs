module DirEvalProvider

open Absyn
open System.IO
open System.Text
open System.Collections.Generic

let env = new Dictionary<string, Dictionary<string, string>>()
let getDirInfo path = new DirectoryInfo(path)
let getSubDirs currentDirPath = Directory.GetDirectories(currentDirPath) |> Array.map getDirInfo

let getAllValue obj = 
    let t = obj.GetType()
    t.GetProperties() |> Array.map (fun p -> (p.Name.ToLower(), p.GetValue(obj).ToString()))

let filterValue names nameValues = 
    nameValues |> Array.filter (fun (name, value) -> 
                      match names |> Array.tryFind (fun _name -> _name = name) with
                      | Some(_) -> true
                      | None -> false)

let getValues names obj = getAllValue obj |> filterValue names

//let formatAll (env':Dictionary<string, Dictionary<string, string>>) =
//    let ksb =new StringBuilder()
//    let vsb =new StringBuilder()
//    for dir in (env'.Keys) do
//        for propertyName in env'.[dir].Keys do
//            ksb.Append(propertyName+" ")  |> ignore   
//            vsb.Append(env'.[dir].[propertyName]+" ") |> ignore
//
//            
//let evalexpr expr env =
//    match expr with
//    | Star ->
let eval stmt = 
    match stmt with
    | Select(exprs, dirs, Some(whereExprs)) -> ()
    | Select(exprs, dirs, None) -> ()
    | Set(name, str) -> ()
