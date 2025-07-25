# Generated by Mochi compiler v0.10.27 on 2025-07-17T18:23:34Z
defmodule Main do
  @start_date "1993-07-01"
  @end_date "1993-08-01"
  def main do
    # orders :: list(map())
    orders = [
      %{o_orderkey: 1, o_orderdate: "1993-07-01", o_orderpriority: "1-URGENT"},
      %{o_orderkey: 2, o_orderdate: "1993-07-15", o_orderpriority: "2-HIGH"},
      %{o_orderkey: 3, o_orderdate: "1993-08-01", o_orderpriority: "3-NORMAL"}
    ]

    # lineitem :: list(map())
    lineitem = [
      %{l_orderkey: 1, l_commitdate: "1993-07-10", l_receiptdate: "1993-07-12"},
      %{l_orderkey: 1, l_commitdate: "1993-07-12", l_receiptdate: "1993-07-10"},
      %{l_orderkey: 2, l_commitdate: "1993-07-20", l_receiptdate: "1993-07-25"},
      %{l_orderkey: 3, l_commitdate: "1993-08-02", l_receiptdate: "1993-08-01"},
      %{l_orderkey: 3, l_commitdate: "1993-08-05", l_receiptdate: "1993-08-10"}
    ]

    # date_filtered_orders :: list(map())
    date_filtered_orders =
      for o <- orders, o.o_orderdate >= @start_date && o.o_orderdate < @end_date, do: o

    # late_orders :: list(map())
    late_orders =
      for o <- date_filtered_orders,
          length(
            for l <- lineitem,
                l.l_orderkey == o.o_orderkey && l.l_commitdate < l.l_receiptdate,
                do: l
          ) > 0,
          do: o

    # result :: list(map())
    result =
      (fn ->
         src = late_orders
         rows = _query(src, [], %{select: fn o -> [o] end})
         groups = _group_by(rows, fn [o] -> o.o_orderpriority end)
         items = groups
         items = Enum.sort_by(items, fn g -> g.key end)
         Enum.map(items, fn g -> %{o_orderpriority: g.key, order_count: length(g.items)} end)
       end).()

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

  defp _query(src, joins, opts \\ %{}) do
    where = Map.get(opts, :where)
    items = Enum.map(src, fn v -> [v] end)

    items =
      Enum.reduce(joins, items, fn j, items ->
        joined =
          cond do
            Map.get(j, :right) && Map.get(j, :left) ->
              matched = for _ <- j[:items], do: false

              {res, matched} =
                Enum.reduce(items, {[], matched}, fn left, {acc, matched} ->
                  {acc, matched, m} =
                    Enum.reduce(Enum.with_index(j[:items]), {acc, matched, false}, fn {right, ri},
                                                                                      {acc,
                                                                                       matched,
                                                                                       m} ->
                      keep =
                        if Map.has_key?(j, :on) and j[:on],
                          do: apply(j[:on], left ++ [right]),
                          else: true

                      if keep do
                        matched = List.replace_at(matched, ri, true)
                        {acc ++ [left ++ [right]], matched, true}
                      else
                        {acc, matched, m}
                      end
                    end)

                  acc = if !m, do: acc ++ [left ++ [nil]], else: acc
                  {acc, matched}
                end)

              Enum.reduce(Enum.with_index(j[:items]), res, fn {right, ri}, acc ->
                if Enum.at(matched, ri) do
                  acc
                else
                  undef = List.duplicate(nil, if(items == [], do: 0, else: length(hd(items))))
                  acc ++ [undef ++ [right]]
                end
              end)

            Map.get(j, :right) ->
              Enum.reduce(j[:items], [], fn right, acc ->
                {acc2, m} =
                  Enum.reduce(items, {acc, false}, fn left, {acc, m} ->
                    keep =
                      if Map.has_key?(j, :on) and j[:on],
                        do: apply(j[:on], left ++ [right]),
                        else: true

                    if keep, do: {acc ++ [left ++ [right]], true}, else: {acc, m}
                  end)

                if !m do
                  undef = List.duplicate(nil, if(items == [], do: 0, else: length(hd(items))))
                  acc2 ++ [undef ++ [right]]
                else
                  acc2
                end
              end)

            true ->
              Enum.reduce(items, [], fn left, acc ->
                {acc2, m} =
                  Enum.reduce(j[:items], {acc, false}, fn right, {acc, m} ->
                    keep =
                      if Map.has_key?(j, :on) and j[:on],
                        do: apply(j[:on], left ++ [right]),
                        else: true

                    if keep, do: {acc ++ [left ++ [right]], true}, else: {acc, m}
                  end)

                if Map.get(j, :left) && !m do
                  acc2 ++ [left ++ [nil]]
                else
                  acc2
                end
              end)
          end

        joined
      end)

    items = if where, do: Enum.filter(items, fn r -> where.(r) end), else: items

    items =
      if Map.has_key?(opts, :sortKey),
        do: Enum.sort_by(items, fn r -> apply(opts[:sortKey], r) end),
        else: items

    items =
      if Map.has_key?(opts, :skip),
        do:
          (
            n = opts[:skip]
            if n < length(items), do: Enum.drop(items, n), else: []
          ),
        else: items

    items =
      if Map.has_key?(opts, :take),
        do:
          (
            n = opts[:take]
            if n < length(items), do: Enum.take(items, n), else: items
          ),
        else: items

    Enum.map(items, fn r -> apply(opts[:select], r) end)
  end
end

Main.main()
