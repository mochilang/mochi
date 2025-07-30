#r "nuget: FSharp.Compiler.Service, 41.0.1"
open System
open System.Text
open System.Text.Json
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

let input = Console.In.ReadToEnd()
let checker = FSharpChecker.Create()
let opts =
    { FSharpParsingOptions.Default with
        SourceFiles = [| "stdin.fs" |]
        IsInteractive = true }
let res =
    checker.ParseFile("stdin.fs", SourceText.ofString input, opts)
    |> Async.RunSynchronously

let lines = input.Split([|"\n"|], StringSplitOptions.None)
let slice (r: range) =
    let sb = StringBuilder()
    for i in r.StartLine .. r.EndLine do
        let line = lines.[i - 1]
        if i = r.StartLine && i = r.EndLine then
            sb.Append(line.Substring(r.StartColumn, r.EndColumn - r.StartColumn)) |> ignore
        elif i = r.StartLine then
            sb.Append(line.Substring(r.StartColumn)) |> ignore
        elif i = r.EndLine then
            sb.Append(line.Substring(0, r.EndColumn)) |> ignore
        else
            sb.AppendLine(line) |> ignore
    sb.ToString()

let vars = ResizeArray<_>()
let prints = ResizeArray<_>()
let stmts = ResizeArray<_>()

let rec visitExpr expr =
    match expr with
    | SynExpr.App(_,_,SynExpr.Ident id, arg, _) when id.idText = "printfn" ->
        prints.Add(slice arg.Range)
    | SynExpr.ForEach(_,_,_, pat, seqExpr, body, _) ->
        let varName =
            match pat with
            | SynPat.Named(si, _, _, _) ->
                match si with
                | SynIdent(id, _) -> id.idText
                | _ -> "_"
            | _ -> "_"
        let bodyStmtsBefore = stmts.Count
        visitExpr body
        let bodyStmts = [ for i in bodyStmtsBefore .. stmts.Count - 1 -> stmts.[i] ]
        while stmts.Count > bodyStmtsBefore do stmts.RemoveAt(stmts.Count-1)
        stmts.Add(box {| var = varName; expr = slice seqExpr.Range; body = bodyStmts; line = 0; raw = "" |})
    | SynExpr.Sequential(_,_,e1,e2,_) ->
        visitExpr e1
        visitExpr e2
    | _ -> ()

let rec visitDecl decl =
    match decl with
    | SynModuleDecl.Let(_,bindings,_) ->
        for b in bindings do
            match b.HeadPattern with
            | SynPat.Named(si, _, _, _, _) ->
                let name =
                    match si with
                    | SynIdent(id, _) -> id.idText
                    | _ -> "_"
                vars.Add({| name = name; expr = slice b.Expr.Range; ``mutable`` = b.IsMutable; ``type`` = None; line = b.RangeOfBindingSansRhs.StartLine; raw = "" |})
            | _ -> ()
    | SynModuleDecl.DoExpr(_,expr,_) -> visitExpr expr
    | _ -> ()

match res.ParseTree with
| Some (ParsedInput.ImplFile(impl)) ->
    for d in impl.Contents do
        visitDecl d
    let obj = {| vars = vars; prints = prints; stmts = stmts |}
    let json = JsonSerializer.Serialize(obj)
    printfn "%s" json
| _ ->
    eprintfn "parse failed"
    Environment.ExitCode <- 1
