# Generated by Mochi compiler v0.10.26 on 2025-07-16T17:37:05Z
defmodule Main do
  def main do
    # web_sales :: list(map())
    web_sales = [
      %{ws_sold_time_sk: 1, ws_ship_hdemo_sk: 1, ws_web_page_sk: 10},
      %{ws_sold_time_sk: 1, ws_ship_hdemo_sk: 1, ws_web_page_sk: 10},
      %{ws_sold_time_sk: 2, ws_ship_hdemo_sk: 1, ws_web_page_sk: 10}
    ]

    # household_demographics :: list(map())
    household_demographics = [%{hd_demo_sk: 1, hd_dep_count: 2}]
    # time_dim :: list(map())
    time_dim = [%{t_time_sk: 1, t_hour: 7}, %{t_time_sk: 2, t_hour: 14}]
    # web_page :: list(map())
    web_page = [%{wp_web_page_sk: 10, wp_char_count: 5100}]
    # amc :: integer()
    amc =
      _count(
        for ws <- web_sales,
            hd <- household_demographics,
            t <- time_dim,
            wp <- web_page,
            ws.ws_ship_hdemo_sk == hd.hd_demo_sk && ws.ws_sold_time_sk == t.t_time_sk &&
              ws.ws_web_page_sk == wp.wp_web_page_sk &&
              (t.t_hour >= 7 && t.t_hour <= 8 && hd.hd_dep_count == 2 && wp.wp_char_count >= 5000 &&
                 wp.wp_char_count <= 5200),
            do: ws
      )

    # pmc :: integer()
    pmc =
      _count(
        for ws <- web_sales,
            hd <- household_demographics,
            t <- time_dim,
            wp <- web_page,
            ws.ws_ship_hdemo_sk == hd.hd_demo_sk && ws.ws_sold_time_sk == t.t_time_sk &&
              ws.ws_web_page_sk == wp.wp_web_page_sk &&
              (t.t_hour >= 14 && t.t_hour <= 15 && hd.hd_dep_count == 2 &&
                 wp.wp_char_count >= 5000 && wp.wp_char_count <= 5200),
            do: ws
      )

    # result :: float()
    result = String.to_float(amc) / String.to_float(pmc)
    _json(result)
  end

  defp _count(v) do
    cond do
      is_list(v) -> length(v)
      is_map(v) and Map.has_key?(v, :items) -> length(Map.get(v, :items))
      true -> raise "count() expects list or group"
    end
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
