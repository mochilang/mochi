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

	helperInput = `let _input () : string =
  match System.Console.ReadLine() with
  | null -> ""
  | s -> s.Trim()`

	helperFetch = `let _fetch (url: string) (opts: Map<string,obj> option) : Map<string,obj> =
  use client = new System.Net.Http.HttpClient()
  let resp = client.GetAsync(url).Result
  let status = resp.StatusCode |> int |> box
  let body = resp.Content.ReadAsStringAsync().Result |> box
  Map.ofList [("status", status); ("body", body)]`

	helperToJson = `let rec _to_json (v: obj) : string =
  match v with
  | null -> "null"
  | :? string as s ->
      "\"" + s.Replace("\\", "\\\\").Replace("\"", "\\\"") + "\""
  | :? bool
  | :? int
  | :? int64
  | :? double -> string v
  | :? System.Collections.Generic.IDictionary<string,obj> as m ->
      m
      |> Seq.map (fun (KeyValue(k,v)) ->
          "\"" + k.Replace("\"", "\\\"") + "\":" + _to_json v)
      |> String.concat ","
      |> fun s -> "{" + s + "}"
  | :? System.Collections.IEnumerable as e ->
      e
      |> Seq.cast<obj>
      |> Seq.map _to_json
      |> String.concat ","
      |> fun s -> "[" + s + "]"
  | _ -> "\"" + v.ToString().Replace("\\", "\\\\").Replace("\"", "\\\"") + "\""

        let _json (v: obj) : unit =
  printfn "%s" (_to_json v)`

	helperExtern = `let externObjects = System.Collections.Generic.Dictionary<string,obj>()
let register_extern (name: string) (obj: obj) : unit = externObjects[name] <- obj
let _extern_get (name: string) : obj =
  match externObjects.TryGetValue(name) with
  | true, v -> v
  | _ -> failwith ("extern object not registered: " + name)`
)

var helperMap = map[string]string{
	"_load":         helperLoad,
	"_save":         helperSave,
	"_run_test":     helperRunTest,
	"_input":        helperInput,
	"_fetch":        helperFetch,
	"_json_helpers": helperToJson,
	"_extern":       helperExtern,
}
