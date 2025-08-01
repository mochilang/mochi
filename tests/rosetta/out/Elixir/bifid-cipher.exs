# Generated by Mochi compiler v0.10.26 on 2025-07-16T12:46:22Z
defmodule Main do
  @spec square_to_maps(list(list(String.t()))) :: map()
  def square_to_maps(square) do
    try do
      emap = %{}
      _ = emap
      dmap = %{}
      _ = dmap
      x = 0
      _ = x

      t1 = fn t1, dmap, emap, x ->
        try do
          if x < length(square) do
            row = Enum.at(square, x)
            y = 0
            _ = y

            t2 = fn t2, dmap, emap, y ->
              try do
                if y < length(row) do
                  ch = Enum.at(row, y)
                  emap = Map.put(emap, ch, [x, y])
                  dmap = Map.put(dmap, (to_string(x) <> ",") <> to_string(y), ch)
                  y = y + 1
                  t2.(t2, dmap, emap, y)
                else
                  {:ok, dmap, emap, y}
                end
              catch
                :break ->
                  {:ok, dmap, emap, y}
              end
            end

            {_, dmap, emap, y} = t2.(t2, dmap, emap, y)
            _ = dmap
            _ = emap
            _ = y
            x = x + 1
            t1.(t1, dmap, emap, x)
          else
            {:ok, dmap, emap, x}
          end
        catch
          :break ->
            {:ok, dmap, emap, x}
        end
      end

      {_, dmap, emap, x} = t1.(t1, dmap, emap, x)
      _ = dmap
      _ = emap
      _ = x
      throw({:return, %{"e" => emap, "d" => dmap}})
    catch
      {:return, v} -> v
    end
  end

  @spec remove_space(String.t(), map()) :: String.t()
  def remove_space(text, emap) do
    try do
      s = upper.(text)
      out = ""
      _ = out
      i = 0
      _ = i

      t3 = fn t3, i, out ->
        try do
          if i < length(s) do
            ch = Enum.slice(s, i, i + 1 - i)

            if ch != " " &&
                 if(is_map(emap), do: Map.has_key?(emap, ch), else: Enum.member?(emap, ch)) do
              out = out + ch
            end

            i = i + 1
            t3.(t3, i, out)
          else
            {:ok, i, out}
          end
        catch
          :break ->
            {:ok, i, out}
        end
      end

      {_, i, out} = t3.(t3, i, out)
      _ = i
      _ = out
      throw({:return, out})
    catch
      {:return, v} -> v
    end
  end

  @spec encrypt(String.t(), map(), map()) :: String.t()
  def encrypt(text, emap, dmap) do
    try do
      text = remove_space(text, emap)
      row0 = []
      _ = row0
      row1 = []
      _ = row1
      i = 0
      _ = i

      t4 = fn t4, i, row0, row1 ->
        try do
          if i < length(text) do
            ch = Enum.slice(text, i, i + 1 - i)
            xy = Enum.at(emap, ch)
            row0 = row0 ++ [Enum.at(xy, 0)]
            row1 = row1 ++ [Enum.at(xy, 1)]
            i = i + 1
            t4.(t4, i, row0, row1)
          else
            {:ok, i, row0, row1}
          end
        catch
          :break ->
            {:ok, i, row0, row1}
        end
      end

      {_, i, row0, row1} = t4.(t4, i, row0, row1)
      _ = i
      _ = row0
      _ = row1

      {row0} =
        Enum.reduce(_iter(row1), {row0}, fn v, {row0} ->
          row0 = row0 ++ [v]
          {row0}
        end)

      _ = row0
      res = ""
      _ = res
      j = 0
      _ = j

      t5 = fn t5, j, res ->
        try do
          if j < length(row0) do
            key = (to_string(Enum.at(row0, j)) <> ",") <> to_string(Enum.at(row0, j + 1))
            res = res + Enum.at(dmap, key)
            j = j + 2
            t5.(t5, j, res)
          else
            {:ok, j, res}
          end
        catch
          :break ->
            {:ok, j, res}
        end
      end

      {_, j, res} = t5.(t5, j, res)
      _ = j
      _ = res
      throw({:return, res})
    catch
      {:return, v} -> v
    end
  end

  @spec decrypt(String.t(), map(), map()) :: String.t()
  def decrypt(text, emap, dmap) do
    try do
      text = remove_space(text, emap)
      coords = []
      _ = coords
      i = 0
      _ = i

      t6 = fn t6, coords, i ->
        try do
          if i < length(text) do
            ch = Enum.slice(text, i, i + 1 - i)
            xy = Enum.at(emap, ch)
            coords = coords ++ [Enum.at(xy, 0)]
            coords = coords ++ [Enum.at(xy, 1)]
            i = i + 1
            t6.(t6, coords, i)
          else
            {:ok, coords, i}
          end
        catch
          :break ->
            {:ok, coords, i}
        end
      end

      {_, coords, i} = t6.(t6, coords, i)
      _ = coords
      _ = i
      half = length(coords) / 2
      _ = half
      k1 = []
      _ = k1
      k2 = []
      _ = k2
      idx = 0
      _ = idx

      t7 = fn t7, idx, k1 ->
        try do
          if idx < half do
            k1 = k1 ++ [Enum.at(coords, idx)]
            idx = idx + 1
            t7.(t7, idx, k1)
          else
            {:ok, idx, k1}
          end
        catch
          :break ->
            {:ok, idx, k1}
        end
      end

      {_, idx, k1} = t7.(t7, idx, k1)
      _ = idx
      _ = k1

      t8 = fn t8, idx, k2 ->
        try do
          if idx < length(coords) do
            k2 = k2 ++ [Enum.at(coords, idx)]
            idx = idx + 1
            t8.(t8, idx, k2)
          else
            {:ok, idx, k2}
          end
        catch
          :break ->
            {:ok, idx, k2}
        end
      end

      {_, idx, k2} = t8.(t8, idx, k2)
      _ = idx
      _ = k2
      res = ""
      _ = res
      j = 0
      _ = j

      t9 = fn t9, j, res ->
        try do
          if j < half do
            key = (to_string(Enum.at(k1, j)) <> ",") <> to_string(Enum.at(k2, j))
            res = res + Enum.at(dmap, key)
            j = j + 1
            t9.(t9, j, res)
          else
            {:ok, j, res}
          end
        catch
          :break ->
            {:ok, j, res}
        end
      end

      {_, j, res} = t9.(t9, j, res)
      _ = j
      _ = res
      throw({:return, res})
    catch
      {:return, v} -> v
    end
  end

  @spec main() :: nil
  def main() do
    try do
      squareRosetta = [
        ["A", "B", "C", "D", "E"],
        ["F", "G", "H", "I", "K"],
        ["L", "M", "N", "O", "P"],
        ["Q", "R", "S", "T", "U"],
        ["V", "W", "X", "Y", "Z"],
        ["J", "1", "2", "3", "4"]
      ]

      squareWikipedia = [
        ["B", "G", "W", "K", "Z"],
        ["Q", "P", "N", "D", "S"],
        ["I", "O", "A", "X", "E"],
        ["F", "C", "L", "U", "M"],
        ["T", "H", "Y", "V", "R"],
        ["J", "1", "2", "3", "4"]
      ]

      textRosetta = "0ATTACKATDAWN"
      textWikipedia = "FLEEATONCE"
      textTest = "The invasion will start on the first of January"
      maps = square_to_maps(squareRosetta)
      _ = maps
      emap = Enum.at(maps, "e")
      _ = emap
      dmap = Enum.at(maps, "d")
      _ = dmap
      IO.puts("from Rosettacode")
      IO.inspect("original:\t " <> textRosetta)
      s = encrypt(textRosetta, emap, dmap)
      _ = s
      IO.inspect("codiert:\t " <> s)
      s = decrypt(s, emap, dmap)
      IO.inspect("and back:\t " <> s)
      maps = square_to_maps(squareWikipedia)
      emap = Enum.at(maps, "e")
      dmap = Enum.at(maps, "d")
      IO.puts("from Wikipedia")
      IO.inspect("original:\t " <> textWikipedia)
      s = encrypt(textWikipedia, emap, dmap)
      IO.inspect("codiert:\t " <> s)
      s = decrypt(s, emap, dmap)
      IO.inspect("and back:\t " <> s)
      maps = square_to_maps(squareWikipedia)
      emap = Enum.at(maps, "e")
      dmap = Enum.at(maps, "d")
      IO.puts("from Rosettacode long part")
      IO.inspect("original:\t " <> textTest)
      s = encrypt(textTest, emap, dmap)
      IO.inspect("codiert:\t " <> s)
      s = decrypt(s, emap, dmap)
      IO.inspect("and back:\t " <> s)
    catch
      {:return, v} -> v
    end
  end

  def main do
    main()
  end

  defp _iter(v) do
    if is_map(v) do
      Map.keys(v)
    else
      v
    end
  end
end

Main.main()
