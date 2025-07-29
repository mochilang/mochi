#r "nuget: FSharp.Compiler.Service, 41.0.0"
open System
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open System.Text.Json

let input = Console.In.ReadToEnd()
let checker = FSharpChecker.Create()
let parseOpts = { FSharpParsingOptions.Default with SourceFiles = [|"stdin.fs"|] }
let res =
    checker.ParseFile("stdin.fs", SourceText.ofString input, parseOpts)
    |> Async.RunSynchronously
match res.ParseTree with
| None ->
    eprintfn "parse failed"
    Environment.ExitCode <- 1
| Some tree ->
    let obj = {| source = input; vars = []; prints = []; stmts = [] |}
    let json = JsonSerializer.Serialize(obj)
    printfn "%s" json
