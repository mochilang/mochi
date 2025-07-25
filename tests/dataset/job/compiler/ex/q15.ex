# Generated by Mochi compiler v0.10.25 on 2025-07-13T12:56:58Z
defmodule Main do
  def main do
    # aka_title :: list(map())
    aka_title = [%{movie_id: 1}, %{movie_id: 2}]
    # company_name :: list(map())
    company_name = [%{id: 1, country_code: "[us]"}, %{id: 2, country_code: "[gb]"}]
    # company_type :: list(map())
    company_type = [%{id: 10}, %{id: 20}]
    # info_type :: list(map())
    info_type = [%{id: 5, info: "release dates"}, %{id: 6, info: "other"}]
    # keyword :: list(map())
    keyword = [%{id: 100}, %{id: 200}]
    # movie_companies :: list(map())
    movie_companies = [
      %{movie_id: 1, company_id: 1, company_type_id: 10, note: "release (2005) (worldwide)"},
      %{movie_id: 2, company_id: 2, company_type_id: 20, note: "release (1999) (worldwide)"}
    ]

    # movie_info :: list(map())
    movie_info = [
      %{movie_id: 1, info_type_id: 5, note: "internet", info: "USA: March 2005"},
      %{movie_id: 2, info_type_id: 5, note: "theater", info: "USA: May 1999"}
    ]

    # movie_keyword :: list(map())
    movie_keyword = [%{movie_id: 1, keyword_id: 100}, %{movie_id: 2, keyword_id: 200}]
    # title :: list(map())
    title = [
      %{id: 1, title: "Example Movie", production_year: 2005},
      %{id: 2, title: "Old Movie", production_year: 1999}
    ]

    # rows :: list(map())
    rows =
      for t <- title,
          at <- aka_title,
          mi <- movie_info,
          mk <- movie_keyword,
          mc <- movie_companies,
          k <- keyword,
          it1 <- info_type,
          cn <- company_name,
          ct <- company_type,
          at.movie_id == t.id && mi.movie_id == t.id && mk.movie_id == t.id && mc.movie_id == t.id &&
            k.id == mk.keyword_id && it1.id == mi.info_type_id && cn.id == mc.company_id &&
            ct.id == mc.company_type_id &&
            (cn.country_code == "[us]" && it1.info == "release dates" &&
               String.contains?(mc.note, "200") && String.contains?(mc.note, "worldwide") &&
               String.contains?(mi.note, "internet") && String.contains?(mi.info, "USA:") &&
               String.contains?(mi.info, "200") && t.production_year > 2000),
          do: %{release_date: mi.info, internet_movie: t.title}

    # result :: list(map())
    result = [
      %{
        release_date: _min(for r <- rows, do: r.release_date),
        internet_movie: _min(for r <- rows, do: r.internet_movie)
      }
    ]

    _json(result)
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

  defp _min(v) do
    list =
      cond do
        is_map(v) and Map.has_key?(v, :items) -> Map.get(v, :items)
        is_list(v) -> v
        true -> raise "min() expects list or group"
      end

    if Enum.count(list) == 0 do
      0
    else
      hd = hd(list)

      Enum.reduce(tl(list), hd, fn it, acc ->
        cond do
          is_binary(acc) and is_binary(it) -> if it < acc, do: it, else: acc
          true -> if Kernel.<(it, acc), do: it, else: acc
        end
      end)
    end
  end
end

Main.main()
