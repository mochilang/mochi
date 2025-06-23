package excode

const (
	helperInput = "defp _input() do\n  String.trim(IO.gets(\"\"))\nend\n"

	helperCount = "defp _count(v) do\n  cond do\n    is_list(v) -> length(v)\n    is_map(v) and Map.has_key?(v, :Items) -> length(v[:Items])\n    true -> raise \"count() expects list or group\"\n  end\nend\n"

	helperAvg = "defp _avg(v) do\n  list = cond do\n    is_map(v) and Map.has_key?(v, :Items) -> v[:Items]\n    is_list(v) -> v\n    true -> raise \"avg() expects list or group\"\n  end\n  if Enum.count(list) == 0 do\n    0\n  else\n    Enum.sum(list) / Enum.count(list)\n  end\nend\n"

	helperGroup = "defmodule _Group do\n  defstruct key: nil, Items: []\nend\n"

	helperGroupBy = "defp _group_by(src, keyfn) do\n  {groups, order} = Enum.reduce(src, {%{}, []}, fn it, {groups, order} ->\n    key = keyfn.(it)\n    ks = to_string(key)\n    {groups, order} = if Map.has_key?(groups, ks) do\n      {groups, order}\n    else\n      {Map.put(groups, ks, %_Group{key: key}), order ++ [ks]}\n    end\n    groups = Map.update!(groups, ks, fn g -> %{g | Items: g.Items ++ [it]} end)\n    {groups, order}\n  end)\n  Enum.map(order, fn k -> groups[k] end)\nend\n"

	helperParseCSV = "defp _parse_csv(text, header, delim) do\n  lines = text |> String.trim() |> String.split(~r/\\r?\\n/, trim: true)\n  if lines == [] do\n    []\n  else\n    {headers, start} = if header do\n      {String.split(hd(lines), delim), 1}\n    else\n      cols = String.split(hd(lines), delim)\n      {Enum.map(0..length(cols)-1, fn i -> \"c\" <> Integer.to_string(i) end), 0}\n    end\n    Enum.drop(lines, start)\n    |> Enum.map(fn line ->\n      parts = String.split(line, delim)\n      Enum.with_index(headers)\n      |> Enum.reduce(%{}, fn {h,i}, acc ->\n        val = if i < length(parts), do: Enum.at(parts, i), else: \"\"\n        value = case Integer.parse(val) do\n          {int, \"\"} -> int\n          _ -> case Float.parse(val) do\n            {f, \"\"} -> f\n            _ -> val\n          end\n        end\n        Map.put(acc, h, value)\n      end)\n    end)\n  end\nend\n"

	helperToCSV = "defp _to_csv(rows, header, delim) do\n  headers = if rows == [], do: [], else: rows |> hd() |> Map.keys() |> Enum.sort()\n  lines = if header, do: [Enum.join(headers, delim)], else: []\n  lines = lines ++ Enum.map(rows, fn row ->\n    Enum.map(headers, fn h ->\n      val = Map.get(row, h)\n      cond do\n        is_map(val) or is_list(val) -> Jason.encode!(val)\n        val == nil -> \"\"\n        true -> to_string(val)\n      end\n    end) |> Enum.join(delim)\n  end)\n  Enum.join(lines, \"\\n\")\nend\n"

	helperToMapList = "defp _to_map_list(v) do\n  cond do\n    is_list(v) and Enum.all?(v, &is_map/1) -> v\n    true -> raise \"save source must be list of maps\"\n  end\nend\n"

	helperLoad = "defp _load(path, opts \\ nil) do\n  format = if opts, do: Map.get(opts, \"format\", \"csv\"), else: \"csv\"\n  header = if opts && Map.has_key?(opts, \"header\"), do: opts[\"header\"], else: true\n  delim = if opts && Map.has_key?(opts, \"delimiter\"), do: String.first(to_string(opts[\"delimiter\"] || \",\")), else: \",\"\n  text = case path do\n    nil -> IO.read(:stdio, :eof)\n    \"\" -> IO.read(:stdio, :eof)\n    \"-\" -> IO.read(:stdio, :eof)\n    _ -> File.read!(path)\n  end\n  case format do\n    \"jsonl\" -> String.trim(text) |> String.split(~r/\\r?\\n/, trim: true) |> Enum.map(&Jason.decode!/1)\n    \"json\" -> case Jason.decode!(text) do\n                list when is_list(list) -> list\n                obj -> [obj]\n              end\n    \"tsv\" -> _parse_csv(text, header, \"\t\")\n    _ -> _parse_csv(text, header, delim)\n  end\nend\n"

	helperSave = "defp _save(data, path, opts \\ nil) do\n  rows = _to_map_list(data)\n  format = if opts, do: Map.get(opts, \"format\", \"csv\"), else: \"csv\"\n  header = if opts && Map.has_key?(opts, \"header\"), do: opts[\"header\"], else: false\n  delim = if opts && Map.has_key?(opts, \"delimiter\"), do: String.first(to_string(opts[\"delimiter\"] || \",\")), else: \",\"\n  out = case format do\n    \"json\" -> Jason.encode!(rows)\n    \"jsonl\" -> Enum.map(rows, &Jason.encode!/1) |> Enum.join(\"\\n\") <> \"\\n\"\n    \"tsv\" -> _to_csv(rows, header, \"\t\")\n    _ -> _to_csv(rows, header, delim)\n  end\n  case path do\n    nil -> IO.write(:stdio, out)\n    \"\" -> IO.write(:stdio, out)\n    \"-\" -> IO.write(:stdio, out)\n    _ -> File.write!(path, out)\n  end\nend\n"

	helperFetch = "defp _fetch(url, opts \\ nil) do\n  :inets.start()\n  :ssl.start()\n  method = if opts, do: Map.get(opts, \"method\", \"GET\"), else: \"GET\"\n  headers = if opts && Map.has_key?(opts, \"headers\"), do: Enum.map(opts[\"headers\"], fn {k,v} -> {String.to_charlist(k), String.to_charlist(to_string(v))} end), else: []\n  body = if opts && Map.has_key?(opts, \"body\"), do: Jason.encode!(opts[\"body\"]), else: \"\"\n  query = if opts && Map.has_key?(opts, \"query\"), do: URI.encode_query(opts[\"query\"]), else: nil\n  full = if query, do: url <> \"?\" <> query, else: url\n  {{_,_,_}, _, resp} = :httpc.request(String.to_atom(String.upcase(method)), {String.to_charlist(full), headers, 'application/json', String.to_charlist(body)}, [], []) |> elem(1)\n  Jason.decode!(resp)\nend\n"

	helperGenText = "defp _gen_text(prompt, _model, _params) do\n  prompt\nend\n"

	helperGenEmbed = "defp _gen_embed(text, _model, _params) do\n  String.to_charlist(text) |> Enum.map(&(&1 + 0.0))\nend\n"

	helperGenStruct = "defp _gen_struct(mod, prompt, _model, _params) do\n  data = Jason.decode!(prompt)\n  struct(mod, for {k,v} <- data, into: %{}, do: {String.to_atom(k), v})\nend\n"
)

var helperMap = map[string]string{
	"_input":       helperInput,
	"_count":       helperCount,
	"_avg":         helperAvg,
	"_group":       helperGroup,
	"_group_by":    helperGroupBy,
	"_parse_csv":   helperParseCSV,
	"_to_csv":      helperToCSV,
	"_to_map_list": helperToMapList,
	"_load":        helperLoad,
	"_save":        helperSave,
	"_fetch":       helperFetch,
	"_gen_text":    helperGenText,
	"_gen_embed":   helperGenEmbed,
	"_gen_struct":  helperGenStruct,
}
