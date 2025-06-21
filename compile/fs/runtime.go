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
  let parse_json (s: string) =
    let doc = System.Text.Json.JsonDocument.Parse(s)
    let rec toObj (e: System.Text.Json.JsonElement) : obj =
      match e.ValueKind with
      | System.Text.Json.JsonValueKind.String -> box (e.GetString())
      | System.Text.Json.JsonValueKind.Number ->
        let mutable i = 0L
        if e.TryGetInt64(&i) then box i else box (e.GetDouble())
      | System.Text.Json.JsonValueKind.True
      | System.Text.Json.JsonValueKind.False -> box (e.GetBoolean())
      | System.Text.Json.JsonValueKind.Array -> e.EnumerateArray() |> Seq.map toObj |> Seq.toArray |> box
      | System.Text.Json.JsonValueKind.Object ->
        e.EnumerateObject()
        |> Seq.map (fun p -> p.Name, toObj p.Value)
        |> Map.ofSeq
        |> box
      | _ -> null
    if doc.RootElement.ValueKind = System.Text.Json.JsonValueKind.Array then
      [ for el in doc.RootElement.EnumerateArray() ->
          el.EnumerateObject() |> Seq.map (fun p -> p.Name, toObj p.Value) |> Map.ofSeq ]
    else
      [ doc.RootElement.EnumerateObject() |> Seq.map (fun p -> p.Name, toObj p.Value) |> Map.ofSeq ]
  match format with
  | "json" -> parse_json text
  | "jsonl" ->
      text.Split([|'\n';'\r'|], System.StringSplitOptions.RemoveEmptyEntries)
      |> Array.map parse_json
      |> Array.collect id
      |> Array.toList
  | _ ->
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
  let toDict (m: Map<string,obj>) =
    let d = System.Collections.Generic.Dictionary<string,obj>()
    for KeyValue(k,v) in m do d[k] <- v
    d
  match format with
  | "json" ->
      let data = rows |> List.map toDict
      let text =
        if data.Length = 1 then System.Text.Json.JsonSerializer.Serialize(data.Head)
        else System.Text.Json.JsonSerializer.Serialize(data)
      match path with
      | None | Some "" | Some "-" -> System.Console.Out.Write(text)
      | Some p -> System.IO.File.WriteAllText(p, text)
  | "jsonl" ->
      let lines = rows |> List.map (fun m -> System.Text.Json.JsonSerializer.Serialize(toDict m))
      let out = String.concat "\n" lines + "\n"
      match path with
      | None | Some "" | Some "-" -> System.Console.Out.Write(out)
      | Some p -> System.IO.File.WriteAllText(p, out)
  | _ when format <> "csv" -> ()
  | _ ->
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

	helperGenText = `let _genText (prompt: string) (model: string) (params: Map<string,obj> option) : string =
  // TODO: integrate with an LLM
  prompt`

	helperGenEmbed = `let _genEmbed (text: string) (model: string) (params: Map<string,obj> option) : double[] =
  text |> Seq.map double |> Seq.toArray`

	helperGenStruct = `let _genStruct<'T> (prompt: string) (model: string) (params: Map<string,obj> option) : 'T =
  // TODO: integrate with an LLM and parse JSON
  System.Text.Json.JsonSerializer.Deserialize<'T>(prompt)`

	helperEval = `let _eval (code: string) : obj =
  let dt = new System.Data.DataTable()
  dt.Compute(code, "")`

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
	"_eval":         helperEval,
	"_genText":      helperGenText,
	"_genEmbed":     helperGenEmbed,
	"_genStruct":    helperGenStruct,
	"_json_helpers": helperToJson,
	"_extern":       helperExtern,
}
