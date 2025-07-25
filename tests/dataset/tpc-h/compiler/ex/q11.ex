# Generated by Mochi compiler v0.10.27 on 2025-07-17T18:23:56Z
defmodule Main do
  @target_nation "GERMANY"
  def main do
    # nation :: list(map())
    nation = [%{n_nationkey: 1, n_name: "GERMANY"}, %{n_nationkey: 2, n_name: "FRANCE"}]
    # supplier :: list(map())
    supplier = [
      %{s_suppkey: 100, s_nationkey: 1},
      %{s_suppkey: 200, s_nationkey: 1},
      %{s_suppkey: 300, s_nationkey: 2}
    ]

    # partsupp :: list(map())
    partsupp = [
      %{ps_partkey: 1000, ps_suppkey: 100, ps_supplycost: 10, ps_availqty: 100},
      %{ps_partkey: 1000, ps_suppkey: 200, ps_supplycost: 20, ps_availqty: 50},
      %{ps_partkey: 2000, ps_suppkey: 100, ps_supplycost: 5, ps_availqty: 10},
      %{ps_partkey: 3000, ps_suppkey: 300, ps_supplycost: 8, ps_availqty: 500}
    ]

    # filtered :: list(map())
    filtered =
      for ps <- partsupp,
          s <- supplier,
          n <- nation,
          s.s_suppkey == ps.ps_suppkey && n.n_nationkey == s.s_nationkey &&
            n.n_name == @target_nation,
          do: %{ps_partkey: ps.ps_partkey, value: ps.ps_supplycost * ps.ps_availqty}

    # grouped :: list(map())
    grouped =
      Enum.map(_group_by(filtered, fn x -> x.ps_partkey end), fn g ->
        %{ps_partkey: g.key, value: Enum.sum(for r <- g.items, do: r.value)}
      end)

    # total :: float()
    total = Enum.sum(for x <- filtered, do: x.value)
    # threshold :: float()
    threshold = total * 0.0001
    # result :: list(map())
    result = for x <- Enum.sort_by(grouped, fn x -> -x.value end), x.value > threshold, do: x
    _json(result)
  end

  defmodule Group do
    defstruct key: nil, items: []

    def fetch(g, k) do
      case k do
        :key -> {:ok, g.key}
        :items -> {:ok, g.items}
        _ -> :error
      end
    end

    def get_and_update(g, k, f) do
      case k do
        :key ->
          {v, nv} = f.(g.key)
          {v, %{g | key: nv}}

        :items ->
          {v, nv} = f.(g.items)
          {v, %{g | items: nv}}

        _ ->
          {nil, g}
      end
    end
  end

  defp _group_by(src, keyfn) do
    {groups, order} =
      Enum.reduce(src, {%{}, []}, fn it, {groups, order} ->
        key =
          if is_list(it) do
            arity = :erlang.fun_info(keyfn, :arity) |> elem(1)
            if arity == 1, do: keyfn.(it), else: apply(keyfn, it)
          else
            keyfn.(it)
          end

        ks = :erlang.phash2(key)

        {groups, order} =
          if Map.has_key?(groups, ks) do
            {groups, order}
          else
            {Map.put(groups, ks, %Group{key: key}), order ++ [ks]}
          end

        val = if is_list(it) and length(it) == 1, do: hd(it), else: it
        groups = Map.update!(groups, ks, fn g -> %{g | items: g.items ++ [val]} end)
        {groups, order}
      end)

    Enum.map(order, fn k -> groups[k] end)
  end

  defp _escape_json(<<>>), do: ""
  defp _escape_json(<<"\\", rest::binary>>), do: "\\\\" <> _escape_json(rest)
  defp _escape_json(<<"\"", rest::binary>>), do: "\\\"" <> _escape_json(rest)
  defp _escape_json(<<c::binary-size(1), rest::binary>>), do: c <> _escape_json(rest)
  defp _to_json(v) when is_binary(v), do: "\"" <> _escape_json(v) <> "\""
  defp _to_json(v) when is_number(v), do: to_string(v)
  defp _to_json(v) when is_boolean(v), do: if(v, do: "true", else: "false")
  defp _to_json(v) when is_list(v), do: "[" <> Enum.map_join(v, ",", &_to_json/1) <> "]"

  defp _to_json(v) when is_map(v) do
    keys = Map.keys(v) |> Enum.map(&to_string/1) |> Enum.sort()

    inner =
      Enum.map_join(keys, ",", fn k ->
        _to_json(k) <> ":" <> _to_json(Map.get(v, String.to_atom(k), Map.get(v, k)))
      end)

    "{" <> inner <> "}"
  end

  defp _to_json(_), do: "null"
  defp _json(v), do: IO.puts(_to_json(v))
end

Main.main()
