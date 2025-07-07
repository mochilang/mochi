//go:build archived

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
  if url.StartsWith("file://") then
    let path = url.Substring(7)
    let text = System.IO.File.ReadAllText(path)
    Map.ofList [("status", box 200); ("body", box text)]
  else
    let mutable urlStr = url
    let meth = opts |> Option.bind (Map.tryFind "method") |> Option.map string |> Option.defaultValue "GET"
    if opts.IsSome then
      match Map.tryFind "query" opts.Value with
      | Some q ->
          let qs =
            (q :?> Map<string,obj>)
            |> Seq.map (fun (KeyValue(k,v)) -> System.Uri.EscapeDataString(k) + "=" + System.Uri.EscapeDataString(string v))
            |> String.concat "&"
          let sep = if urlStr.Contains("?") then "&" else "?"
          urlStr <- urlStr + sep + qs
      | None -> ()
    use msg = new System.Net.Http.HttpRequestMessage(new System.Net.Http.HttpMethod(meth), urlStr)
    if opts.IsSome then
      match Map.tryFind "body" opts.Value with
      | Some b ->
          let json = System.Text.Json.JsonSerializer.Serialize(b)
          msg.Content <- new System.Net.Http.StringContent(json, System.Text.Encoding.UTF8, "application/json")
      | None -> ()
    if opts.IsSome then
      match Map.tryFind "headers" opts.Value with
      | Some hs ->
          for KeyValue(k,v) in (hs :?> Map<string,obj>) do
            msg.Headers.TryAddWithoutValidation(k, string v) |> ignore
      | None -> ()
    use client = new System.Net.Http.HttpClient()
    if opts.IsSome then
      match Map.tryFind "timeout" opts.Value with
      | Some t ->
          match t with
          | :? int as i -> client.Timeout <- System.TimeSpan.FromSeconds(float i)
          | :? int64 as i -> client.Timeout <- System.TimeSpan.FromSeconds(float i)
          | :? float as f -> client.Timeout <- System.TimeSpan.FromSeconds(f)
          | :? float32 as f -> client.Timeout <- System.TimeSpan.FromSeconds(float f)
          | _ -> ()
      | None -> ()
    let resp = client.Send(msg).Result
    let status = resp.StatusCode |> int |> box
    let body = resp.Content.ReadAsStringAsync().Result |> box
    Map.ofList [("status", status); ("body", body)]`

	helperFetchTyped = `let _fetch_json<'T> (url: string) (opts: Map<string,obj> option) : 'T =
  let res = _fetch url opts
  let body = Map.find "body" res :?> string
  System.Text.Json.JsonSerializer.Deserialize<'T>(body)`

	helperCast = `let _cast<'T> (v: obj) : 'T =
  match v with
  | :? 'T as t -> t
  | _ ->
      let json = System.Text.Json.JsonSerializer.Serialize(v)
      System.Text.Json.JsonSerializer.Deserialize<'T>(json)`

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

	helperSliceString = `let _slice_string (s: string) (i: int) (j: int) : string =
  let mutable start = i
  let mutable stop = j
  let n = s.Length
  if start < 0 then start <- start + n
  if stop < 0 then stop <- stop + n
  if start < 0 then start <- 0
  if stop > n then stop <- n
  if stop < start then stop <- start
  s.Substring(start, stop - start)`

	helperReverseArray = `let inline _reverse_array (xs: 'T[]) : 'T[] =
  Array.rev xs`

	helperReverseString = `let inline _reverse_string (s: string) : string =
  s.ToCharArray() |> Array.rev |> System.String`

	helperConcat = `let inline _concat (a: 'T[]) (b: 'T[]) : 'T[] =
  Array.append a b`

	helperUnionAll = `let inline _union_all (a: 'T[]) (b: 'T[]) : 'T[] =
  Array.append a b`

	helperUnion = `let inline _union (a: 'T[]) (b: 'T[]) : 'T[] =
  Array.append a b |> Array.distinct`

	helperExcept = `let inline _except (a: 'T[]) (b: 'T[]) : 'T[] =
  let setB = Set.ofArray b
  Array.filter (fun x -> not (Set.contains x setB)) a`

	helperIntersect = `let inline _intersect (a: 'T[]) (b: 'T[]) : 'T[] =
  let setB = Set.ofArray b
  Array.filter (fun x -> Set.contains x setB) a |> Array.distinct`

	helperSeq = `let inline sum (xs: seq< ^T >) : ^T =
  Seq.sum xs
let inline avg (xs: seq< ^T >) : ^T =
  Seq.average xs
let inline _min (xs: seq< ^T >) : ^T when ^T : comparison =
  Seq.min xs
let inline _max (xs: seq< ^T >) : ^T when ^T : comparison =
  Seq.max xs
let count (xs: seq<'T>) : int =
  Seq.length xs`

	helperGroup = `type _Group<'T>(key: obj) =
  member val key = key with get, set
  member val Items = System.Collections.Generic.List<'T>() with get
  member this.size = this.Items.Count`

	helperGroupBy = `let _group_by (src: 'T list) (keyfn: 'T -> obj) : _Group<'T> list =
  src
  |> Seq.groupBy keyfn
  |> Seq.map (fun (k, items) ->
      let g = _Group<'T>(k)
      for it in items do
        g.Items.Add(it)
      g)
  |> Seq.toList`

	helperLeftJoin = `let _left_join (a: 'L[]) (b: 'R[]) (pred: 'L -> 'R -> bool) : ('L option * 'R option)[] =
  [|
  for x in a do
    let mutable matched = false
    for y in b do
      if pred x y then
        matched <- true
        yield (Some x, Some y)
    if not matched then
      yield (Some x, None)
  |]`

	helperRightJoin = `let _right_join (a: 'L[]) (b: 'R[]) (pred: 'L -> 'R -> bool) : ('L option * 'R option)[] =
  [|
  for y in b do
    let mutable matched = false
    for x in a do
      if pred x y then
        matched <- true
        yield (Some x, Some y)
    if not matched then
      yield (None, Some y)
  |]`

	helperOuterJoin = `let _outer_join (a: 'L[]) (b: 'R[]) (pred: 'L -> 'R -> bool) : ('L option * 'R option)[] =
  Array.append (_left_join a b pred)
    [| for y in b do if not (Array.exists (fun (_,r) -> match r with Some v -> pred v y | None -> false) (_left_join a b pred)) then yield (None, Some y) |]`

	helperToJson = `let rec _to_json (v: obj) : string =
  match v with
  | null -> "null"
  | :? string as s ->
      "\"" + s.Replace("\\", "\\\\").Replace("\"", "\\\"") + "\""
  | :? bool
  | :? int | :? int64
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
	"_load":           helperLoad,
	"_save":           helperSave,
	"_run_test":       helperRunTest,
	"_input":          helperInput,
	"_fetch":          helperFetch,
	"_fetch_json":     helperFetchTyped,
	"_cast":           helperCast,
	"_eval":           helperEval,
	"_slice_string":   helperSliceString,
	"_reverse_array":  helperReverseArray,
	"_reverse_string": helperReverseString,
	"_concat":         helperConcat,
	"_genText":        helperGenText,
	"_genEmbed":       helperGenEmbed,
	"_genStruct":      helperGenStruct,
	"_json_helpers":   helperToJson,
	"_extern":         helperExtern,
	"_union_all":      helperUnionAll,
	"_union":          helperUnion,
	"_except":         helperExcept,
	"_intersect":      helperIntersect,
	"_seq_helpers":    helperSeq,
	"_Group":          helperGroup,
	"_group_by":       helperGroupBy,
	"_left_join":      helperLeftJoin,
	"_right_join":     helperRightJoin,
	"_outer_join":     helperOuterJoin,
}
