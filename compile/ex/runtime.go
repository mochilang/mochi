package excode

const (
	helperInput = "defp _input() do\n  String.trim(IO.gets(\"\"))\nend\n"

	helperCount = "defp _count(v) do\n  cond do\n    is_list(v) -> length(v)\n    is_map(v) and Map.has_key?(v, :Items) -> length(v[:Items])\n    true -> raise \"count() expects list or group\"\n  end\nend\n"

	helperAvg = "defp _avg(v) do\n  list = cond do\n    is_map(v) and Map.has_key?(v, :Items) -> v[:Items]\n    is_list(v) -> v\n    true -> raise \"avg() expects list or group\"\n  end\n  if Enum.count(list) == 0 do\n    0\n  else\n    Enum.sum(list) / Enum.count(list)\n  end\nend\n"

	helperGroup = "defmodule _Group do\n  defstruct key: nil, Items: []\nend\n"

	helperGroupBy = "defp _group_by(src, keyfn) do\n  {groups, order} = Enum.reduce(src, {%{}, []}, fn it, {groups, order} ->\n    key = keyfn.(it)\n    ks = to_string(key)\n    {groups, order} = if Map.has_key?(groups, ks) do\n      {groups, order}\n    else\n      {Map.put(groups, ks, %_Group{key: key}), order ++ [ks]}\n    end\n    groups = Map.update!(groups, ks, fn g -> %{g | Items: g.Items ++ [it]} end)\n    {groups, order}\n  end)\n  Enum.map(order, fn k -> groups[k] end)\nend\n"

	helperLoad = "defp _load(path, _opts) do\n  bin = case path do\n    nil -> IO.binread(:stdio, :eof)\n    \"\" -> IO.binread(:stdio, :eof)\n    \"-\" -> IO.binread(:stdio, :eof)\n    _ -> File.read!(path)\n  end\n  :erlang.binary_to_term(bin)\nend\n"

	helperSave = "defp _save(data, path, _opts) do\n  bin = :erlang.term_to_binary(data)\n  case path do\n    nil -> IO.binwrite(:stdio, bin)\n    \"\" -> IO.binwrite(:stdio, bin)\n    \"-\" -> IO.binwrite(:stdio, bin)\n    _ -> File.write!(path, bin)\n  end\nend\n"
)

var helperMap = map[string]string{
	"_input":    helperInput,
	"_count":    helperCount,
	"_avg":      helperAvg,
	"_group":    helperGroup,
	"_group_by": helperGroupBy,
	"_load":     helperLoad,
	"_save":     helperSave,
}
