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

	helperUnionAll = "defp _union_all(a, b) do\n  a ++ b\nend\n"

	helperUnion = "defp _union(a, b) do\n  Enum.uniq(a ++ b)\nend\n"

	helperExcept = "defp _except(a, b) do\n  Enum.reject(a, fn x -> Enum.member?(b, x) end)\nend\n"

	helperIntersect = "defp _intersect(a, b) do\n  Enum.filter(a, fn x -> Enum.member?(b, x) end) |> Enum.uniq()\nend\n"

	helperIndexString = "defp _index_string(s, i) do\n  chars = String.graphemes(s)\n  idx = if i < 0, do: i + length(chars), else: i\n  if idx < 0 or idx >= length(chars), do: raise \"index out of range\"\n  Enum.at(chars, idx)\nend\n"

	helperSliceString = "defp _slice_string(s, i, j) do\n  chars = String.graphemes(s)\n  n = length(chars)\n  start = if i < 0, do: i + n, else: i\n  finish = if j < 0, do: j + n, else: j\n  start = if start < 0, do: 0, else: start\n  finish = if finish > n, do: n, else: finish\n  finish = if finish < start, do: start, else: finish\n  Enum.slice(chars, start, finish - start) |> Enum.join()\nend\n"

	helperIter = "defp _iter(v) do\n  if is_map(v) do\n    Map.keys(v)\n  else\n    v\n  end\nend\n"

	helperQuery = "defp _query(src, joins, opts \\ %{}) do\n" +
		"  items = Enum.map(src, fn v -> [v] end)\n" +
		"  items = Enum.reduce(joins, items, fn j, items ->\n" +
		"    joined = cond do\n" +
		"      Map.get(j, :right) && Map.get(j, :left) ->\n" +
		"        matched = for _ <- j[:items], do: false\n" +
		"        {res, matched} = Enum.reduce(items, {[], matched}, fn left, {acc, matched} ->\n" +
		"          {acc, matched, m} = Enum.reduce(Enum.with_index(j[:items]), {acc, matched, false}, fn {right, ri}, {acc, matched, m} ->\n" +
		"            keep = if Map.has_key?(j, :on) and j[:on], do: apply(j[:on], left ++ [right]), else: true\n" +
		"            if keep do\n" +
		"              matched = List.replace_at(matched, ri, true)\n" +
		"              {acc ++ [left ++ [right]], matched, true}\n" +
		"            else\n" +
		"              {acc, matched, m}\n" +
		"            end\n" +
		"          end)\n" +
		"          acc = if !m, do: acc ++ [left ++ [nil]], else: acc\n" +
		"          {acc, matched}\n" +
		"        end)\n" +
		"        Enum.reduce(Enum.with_index(j[:items]), res, fn {right, ri}, acc ->\n" +
		"          if Enum.at(matched, ri) do\n" +
		"            acc\n" +
		"          else\n" +
		"            undef = List.duplicate(nil, if items == [], do: 0, else: length(hd(items)))\n" +
		"            acc ++ [undef ++ [right]]\n" +
		"          end\n" +
		"        end)\n" +
		"      Map.get(j, :right) ->\n" +
		"        Enum.reduce(j[:items], [], fn right, acc ->\n" +
		"          {acc2, m} = Enum.reduce(items, {acc, false}, fn left, {acc, m} ->\n" +
		"            keep = if Map.has_key?(j, :on) and j[:on], do: apply(j[:on], left ++ [right]), else: true\n" +
		"            if keep, do: {acc ++ [left ++ [right]], true}, else: {acc, m}\n" +
		"          end)\n" +
		"          if !m do\n" +
		"            undef = List.duplicate(nil, if items == [], do: 0, else: length(hd(items)))\n" +
		"            acc2 ++ [undef ++ [right]]\n" +
		"          else\n" +
		"            acc2\n" +
		"          end\n" +
		"        end)\n" +
		"      true ->\n" +
		"        Enum.reduce(items, [], fn left, acc ->\n" +
		"          {acc2, m} = Enum.reduce(j[:items], {acc, false}, fn right, {acc, m} ->\n" +
		"            keep = if Map.has_key?(j, :on) and j[:on], do: apply(j[:on], left ++ [right]), else: true\n" +
		"            if keep, do: {acc ++ [left ++ [right]], true}, else: {acc, m}\n" +
		"          end)\n" +
		"          if Map.get(j, :left) && !m do\n" +
		"            acc2 ++ [left ++ [nil]]\n" +
		"          else\n" +
		"            acc2\n" +
		"          end\n" +
		"        end)\n" +
		"    end\n" +
		"    joined\n" +
		"  end)\n" +
		"  items = if Map.has_key?(opts, :where), do: Enum.filter(items, fn r -> apply(opts[:where], r) end), else: items\n" +
		"  items = if Map.has_key?(opts, :sortKey), do: Enum.sort_by(items, fn r -> apply(opts[:sortKey], r) end), else: items\n" +
		"  items = if Map.has_key?(opts, :skip), do: (n = opts[:skip]; if n < length(items), do: Enum.drop(items, n), else: []), else: items\n" +
		"  items = if Map.has_key?(opts, :take), do: (n = opts[:take]; if n < length(items), do: Enum.take(items, n), else: items), else: items\n" +
		"  Enum.map(items, fn r -> apply(opts[:select], r) end)\n" +
		"end\n"
)

var helperMap = map[string]string{
	"_input":        helperInput,
	"_count":        helperCount,
	"_avg":          helperAvg,
	"_group":        helperGroup,
	"_group_by":     helperGroupBy,
	"_parse_csv":    helperParseCSV,
	"_to_csv":       helperToCSV,
	"_to_map_list":  helperToMapList,
	"_load":         helperLoad,
	"_save":         helperSave,
	"_fetch":        helperFetch,
	"_gen_text":     helperGenText,
	"_gen_embed":    helperGenEmbed,
	"_gen_struct":   helperGenStruct,
	"_union_all":    helperUnionAll,
	"_union":        helperUnion,
	"_except":       helperExcept,
	"_intersect":    helperIntersect,
	"_index_string": helperIndexString,
	"_slice_string": helperSliceString,
	"_iter":         helperIter,
	"_query":        helperQuery,
}
