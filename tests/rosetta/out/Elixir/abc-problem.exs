# Generated by Mochi compiler v0.10.30 on 2025-07-19T01:02:36Z
defmodule Main do
  @spec fields(String.t()) :: list(String.t())
  def fields(s) do
    try do
      # res :: list(String.t())
      res = []
      _ = res
      # cur :: String.t()
      cur = ""
      _ = cur
      # i :: integer()
      i = 0
      _ = i

      t1 = fn t1, cur, i, res ->
        try do
          if i < String.length(s) do
            c = String.slice(s, i, i + 1 - i)

            if c == " " do
              if String.length(cur) > 0 do
                res = res ++ [cur]
                cur = ""
              end
            else
              cur = cur <> c
            end

            i = i + 1
            t1.(t1, cur, i, res)
          else
            {:ok, cur, i, res}
          end
        catch
          :break ->
            {:ok, cur, i, res}
        end
      end

      {_, cur, i, res} = t1.(t1, cur, i, res)
      _ = cur
      _ = i
      _ = res

      if String.length(cur) > 0 do
        res = res ++ [cur]
      end

      throw({:return, res})
    catch
      {:return, v} -> v
    end
  end

  @spec canSpell(String.t(), list(String.t())) :: boolean()
  def canSpell(word, blks) do
    try do
      if String.length(word) == 0 do
        throw({:return, true})
      end

      c = String.downcase(to_string(String.slice(word, 0, 1 - 0)))
      # i :: integer()
      i = 0
      _ = i

      t2 = fn t2, i ->
        try do
          if i < length(blks) do
            b = Enum.at(blks, i)

            if c == String.downcase(to_string(_slice_string(b, 0, 1))) ||
                 c == String.downcase(to_string(_slice_string(b, 1, 2))) do
              # rest :: list(String.t())
              rest = []
              _ = rest
              # j :: integer()
              j = 0
              _ = j

              t3 = fn t3, j, rest ->
                try do
                  if j < length(blks) do
                    if j != i do
                      rest = rest ++ [Enum.at(blks, j)]
                    end

                    j = j + 1
                    t3.(t3, j, rest)
                  else
                    {:ok, j, rest}
                  end
                catch
                  :break ->
                    {:ok, j, rest}
                end
              end

              {_, j, rest} = t3.(t3, j, rest)
              _ = j
              _ = rest

              if canSpell(String.slice(word, 1, length(String.graphemes(word)) - 1), rest) do
                throw({:return, true})
              end
            end

            i = i + 1
            t2.(t2, i)
          else
            {:ok, i}
          end
        catch
          :break ->
            {:ok, i}
        end
      end

      {_, i} = t2.(t2, i)
      _ = i
      throw({:return, false})
    catch
      {:return, v} -> v
    end
  end

  @spec newSpeller(String.t()) :: (String.t() -> boolean())
  def newSpeller(blocks) do
    try do
      bl = fields(blocks)
      throw({:return, fn w -> canSpell(w, bl) end})
    catch
      {:return, v} -> v
    end
  end

  @spec main() :: nil
  def main() do
    try do
      sp = newSpeller("BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM")

      for word <- ["A", "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "CONFUSE"] do
        IO.inspect((word <> " ") <> to_string(sp(word)))
      end
    catch
      {:return, v} -> v
    end
  end

  def main do
    main()
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
end

Main.main()
