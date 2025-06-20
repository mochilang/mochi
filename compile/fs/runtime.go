package fscode

const (
	helperLoad = `let _load (path: string option) (opts: Map<string,obj> option) : List<Map<string,obj>> =
  let format = opts |> Option.bind (Map.tryFind "format") |> Option.map string |> Option.defaultValue "csv"
  let header = opts |> Option.bind (Map.tryFind "header") |> Option.map unbox<bool> |> Option.defaultValue true
  let mutable delim = opts |> Option.bind (Map.tryFind "delimiter") |> Option.map (fun v -> (string v).[0]) |> Option.defaultValue ','
  if format = "tsv" then delim <- '\t'
  let text =
    match path with
    | None | Some "" | Some "-" -> System.Console.In.ReadToEnd()
    | Some p -> System.IO.File.ReadAllText(p)
  let lines = text.Trim().Split([|'\n';'\r'|], System.StringSplitOptions.RemoveEmptyEntries)
  if lines.Length = 0 then [] else
    let headers =
      if header then lines.[0].Split(delim)
      else Array.init (lines.[0].Split(delim).Length) (fun i -> sprintf "c%d" i)
    let start = if header then 1 else 0
    [ for i in start .. lines.Length - 1 ->
        let parts = lines.[i].Split(delim)
        headers |> Array.mapi (fun j h -> h, if j < parts.Length then box parts.[j] else box "") |> Map.ofArray ]`

	helperSave = `let _save (rows: List<Map<string,obj>>) (path: string option) (opts: Map<string,obj> option) : unit =
  let format = opts |> Option.bind (Map.tryFind "format") |> Option.map string |> Option.defaultValue "csv"
  let header = opts |> Option.bind (Map.tryFind "header") |> Option.map unbox<bool> |> Option.defaultValue false
  let mutable delim = opts |> Option.bind (Map.tryFind "delimiter") |> Option.map (fun v -> (string v).[0]) |> Option.defaultValue ','
  if format = "tsv" then delim <- '\t'
  if format <> "csv" then () else
    let headers = if rows.Length > 0 then rows.[0] |> Map.keys |> Seq.toArray |> Array.sort else [||]
    let sb = System.Text.StringBuilder()
    if header then sb.AppendLine(String.concat (string delim) headers) |> ignore
    for row in rows do
        let line =
            headers
            |> Array.map (fun h -> match Map.tryFind h row with Some v -> string v | None -> "")
            |> String.concat (string delim)
        sb.AppendLine(line) |> ignore
    let out = sb.ToString()
    match path with
    | None | Some "" | Some "-" -> System.Console.Out.Write(out)
    | Some p -> System.IO.File.WriteAllText(p, out)`
	helperRunTest = `let _run_test (name: string) (f: unit -> unit) : bool =
  printf "%s ... " name
  try
    f()
    printfn "PASS"
    true
  with e ->
    printfn "FAIL (%s)" e.Message
    false`
)

var helperMap = map[string]string{
	"_load":     helperLoad,
	"_save":     helperSave,
	"_run_test": helperRunTest,
}
