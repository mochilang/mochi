package excode

const (
	helperInput = "defp _input() do\n  String.trim(IO.gets(\"\"))\nend\n"

	helperCount = "defp _count(v) do\n  cond do\n    is_list(v) -> length(v)\n    is_map(v) and Map.has_key?(v, :Items) -> length(v[:Items])\n    true -> raise \"count() expects list or group\"\n  end\nend\n"

	helperAvg = "defp _avg(v) do\n  list = cond do\n    is_map(v) and Map.has_key?(v, :Items) -> v[:Items]\n    is_list(v) -> v\n    true -> raise \"avg() expects list or group\"\n  end\n  if Enum.count(list) == 0 do\n    0\n  else\n    Enum.sum(list) / Enum.count(list)\n  end\nend\n"
)

var helperMap = map[string]string{
	"_input": helperInput,
	"_count": helperCount,
	"_avg":   helperAvg,
}
