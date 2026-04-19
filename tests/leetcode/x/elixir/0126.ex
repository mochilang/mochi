defmodule Main do
  def ladders(begin_word, end_word, words) do
    word_set = MapSet.new(words)
    if !MapSet.member?(word_set, end_word), do: [], else: do_ladders(begin_word, end_word, word_set)
  end

  defp do_ladders(begin_word, end_word, word_set) do
    {parents, found} = bfs(MapSet.new([begin_word]), MapSet.new([begin_word]), %{}, end_word, word_set)
    if !found, do: [], else: backtrack(end_word, begin_word, parents) |> Enum.sort()
  end

  defp bfs(level, _visited, parents, _end_word, _word_set) when map_size(level) == 0, do: {parents, false}
  defp bfs(level, visited, parents, end_word, word_set) do
    {next, parents, found} =
      level
      |> MapSet.to_list()
      |> Enum.sort()
      |> Enum.reduce({MapSet.new(), parents, false}, fn word, {next, parents, found} ->
        chars = String.to_charlist(word)
        Enum.reduce(0..(length(chars)-1), {next, parents, found}, fn i, acc ->
          {next2, parents2, found2} = acc
          orig = Enum.at(chars, i)
          Enum.reduce(?a..?z, {next2, parents2, found2}, fn ch, {nxt, par, fnd} ->
            if ch == orig do
              {nxt, par, fnd}
            else
              nw = chars |> List.replace_at(i, ch) |> to_string()
              if MapSet.member?(word_set, nw) and !MapSet.member?(visited, nw) do
                par = Map.update(par, nw, [word], fn v -> [word | v] end)
                {MapSet.put(nxt, nw), par, fnd or nw == end_word}
              else
                {nxt, par, fnd}
              end
            end
          end)
        end)
      end)
    if found, do: {parents, true}, else: bfs(next, MapSet.union(visited, next), parents, end_word, word_set)
  end

  defp backtrack(word, begin_word, _parents) when word == begin_word, do: [[begin_word]]
  defp backtrack(word, begin_word, parents) do
    (Map.get(parents, word, []) |> Enum.sort())
    |> Enum.flat_map(fn p -> Enum.map(backtrack(p, begin_word, parents), fn path -> path ++ [word] end) end)
  end

  defp fmt(paths) do
    ([Integer.to_string(length(paths))] ++ Enum.map(paths, &Enum.join(&1, "->"))) |> Enum.join("\n")
  end

  def main do
    lines = IO.read(:stdio, :eof) |> String.split(~r/\r?\n/, trim: true)
    if lines != [] do
      tc = hd(lines) |> String.to_integer()
      {_, out} = Enum.reduce(1..tc, {1, []}, fn _, {idx, out} ->
        begin_word = Enum.at(lines, idx)
        end_word = Enum.at(lines, idx + 1)
        n = Enum.at(lines, idx + 2) |> String.to_integer()
        words = Enum.slice(lines, idx + 3, n)
        {idx + 3 + n, out ++ [fmt(ladders(begin_word, end_word, words))]}
      end)
      IO.write(Enum.join(out, "\n\n"))
    end
  end
end

Main.main()
