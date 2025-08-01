# Generated by Mochi compiler v0.10.26 on 2025-07-16T17:34:24Z
defmodule Main do
  def main do
    # catalog_sales :: list(map())
    catalog_sales = [%{cs_bill_customer_sk: 1, cs_sales_price: 600, cs_sold_date_sk: 1}]
    # customer :: list(map())
    customer = [%{c_customer_sk: 1, c_current_addr_sk: 1}]
    # customer_address :: list(map())
    customer_address = [%{ca_address_sk: 1, ca_zip: "85669", ca_state: "CA"}]
    # date_dim :: list(map())
    date_dim = [%{d_date_sk: 1, d_qoy: 1, d_year: 2000}]
    # filtered :: list(map())
    filtered =
      (fn ->
         src = catalog_sales

         rows =
           _query(
             src,
             [
               %{items: customer, on: fn cs, c -> cs.cs_bill_customer_sk == c.c_customer_sk end},
               %{
                 items: customer_address,
                 on: fn cs, c, ca -> c.c_current_addr_sk == ca.ca_address_sk end
               },
               %{items: date_dim, on: fn cs, c, ca, d -> cs.cs_sold_date_sk == d.d_date_sk end}
             ],
             %{
               select: fn cs, c, ca, d ->
                 Map.merge(Map.merge(Map.merge(Map.merge(cs, c), ca), d), %{
                   cs: cs,
                   c: c,
                   ca: ca,
                   d: d
                 })
               end,
               where: fn [cs, c, ca, d] ->
                 (if(
                    is_map([
                      "85669",
                      "86197",
                      "88274",
                      "83405",
                      "86475",
                      "85392",
                      "85460",
                      "80348",
                      "81792"
                    ]),
                    do:
                      Map.has_key?(
                        [
                          "85669",
                          "86197",
                          "88274",
                          "83405",
                          "86475",
                          "85392",
                          "85460",
                          "80348",
                          "81792"
                        ],
                        _slice_string(ca.ca_zip, 0, 5)
                      ),
                    else:
                      Enum.member?(
                        [
                          "85669",
                          "86197",
                          "88274",
                          "83405",
                          "86475",
                          "85392",
                          "85460",
                          "80348",
                          "81792"
                        ],
                        _slice_string(ca.ca_zip, 0, 5)
                      )
                  ) ||
                    if(is_map(["CA", "WA", "GA"]),
                      do: Map.has_key?(["CA", "WA", "GA"], ca.ca_state),
                      else: Enum.member?(["CA", "WA", "GA"], ca.ca_state)
                    ) || cs.cs_sales_price > 500) && d.d_qoy == 1 && d.d_year == 2000
               end
             }
           )

         groups = _group_by(rows, fn %{cs: cs, c: c, ca: ca, d: d} -> %{zip: ca.ca_zip} end)
         items = groups
         items = Enum.sort_by(items, fn g -> g.key.zip end)

         Enum.map(items, fn g ->
           %{ca_zip: g.key.zip, sum_sales: _sum(for x <- g.items, do: x.cs_sales_price)}
         end)
       end).()

    _json(filtered)
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

  defp _slice_string(s, i, j) do
    chars = String.graphemes(s)
    n = length(chars)
    start = if i < 0, do: i + n, else: i
    finish = if j < 0, do: j + n, else: j
    start = if start < 0, do: 0, else: start
    finish = if finish > n, do: n, else: finish
    finish = if finish < start, do: start, else: finish
    Enum.slice(chars, start, finish - start) |> Enum.join()
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
