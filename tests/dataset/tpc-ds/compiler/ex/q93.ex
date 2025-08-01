# Generated by Mochi compiler v0.10.26 on 2025-07-16T17:37:13Z
defmodule Main do
  def main do
    # store_sales :: list(map())
    store_sales = [
      %{
        ss_item_sk: 1,
        ss_ticket_number: 1,
        ss_customer_sk: 1,
        ss_quantity: 5,
        ss_sales_price: 10
      },
      %{ss_item_sk: 1, ss_ticket_number: 2, ss_customer_sk: 2, ss_quantity: 3, ss_sales_price: 20}
    ]

    # store_returns :: list(map())
    store_returns = [
      %{sr_item_sk: 1, sr_ticket_number: 1, sr_reason_sk: 1, sr_return_quantity: 1}
    ]

    # reason :: list(map())
    reason = [%{r_reason_sk: 1, r_reason_desc: "ReasonA"}]
    # t :: list(map())
    t =
      (fn ->
         src = store_sales

         _query(
           src,
           [
             %{
               items: store_returns,
               on: fn ss, sr ->
                 ss.ss_item_sk == sr.sr_item_sk && ss.ss_ticket_number == sr.sr_ticket_number
               end,
               left: true
             },
             %{
               items: reason,
               on: fn ss, sr, r -> sr != nil && sr.sr_reason_sk == r.r_reason_sk end,
               left: true
             }
           ],
           %{
             select: fn ss, sr, r ->
               %{
                 ss_customer_sk: ss.ss_customer_sk,
                 act_sales:
                   (fn ->
                      if sr != nil do
                        (ss.ss_quantity - sr.sr_return_quantity) * ss.ss_sales_price
                      else
                        ss.ss_quantity * ss.ss_sales_price
                      end
                    end).()
               }
             end,
             where: fn [ss, sr, r] -> r == nil || r.r_reason_desc == "ReasonA" end
           }
         )
       end).()

    # result :: list(map())
    result =
      (fn ->
         src = t
         rows = _query(src, [], %{select: fn x -> [x] end})
         groups = _group_by(rows, fn [x] -> x.ss_customer_sk end)
         items = groups
         items = Enum.sort_by(items, fn g -> [_sum(for y <- g.items, do: y.act_sales), g.key] end)

         Enum.map(items, fn g ->
           %{ss_customer_sk: g.key, sumsales: _sum(for y <- g.items, do: y.act_sales)}
         end)
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

  defp _sum(v) do
    list =
      cond do
        is_map(v) and Map.has_key?(v, :items) -> Map.get(v, :items)
        is_list(v) -> v
        true -> raise "sum() expects list or group"
      end

    Enum.sum(list)
  end
end

Main.main()
