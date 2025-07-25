# Generated by Mochi compiler v0.10.26 on 2025-07-16T12:46:35Z
defmodule Main do
  @spec indexOf(String.t(), String.t()) :: integer()
  def indexOf(s, ch) do
    try do
      i = 0
      _ = i

      t1 = fn t1, i ->
        try do
          if i < length(s) do
            if Enum.slice(s, i, i + 1 - i) == ch do
              throw({:return, i})
            end

            i = i + 1
            t1.(t1, i)
          else
            {:ok, i}
          end
        catch
          :break ->
            {:ok, i}
        end
      end

      {_, i} = t1.(t1, i)
      _ = i
      throw({:return, -1})
    catch
      {:return, v} -> v
    end
  end

  @spec set58(String.t()) :: list(integer())
  def set58(addr) do
    try do
      tmpl = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
      a = []
      _ = a
      i = 0
      _ = i

      t2 = fn t2, a, i ->
        try do
          if i < 25 do
            a = a ++ [0]
            i = i + 1
            t2.(t2, a, i)
          else
            {:ok, a, i}
          end
        catch
          :break ->
            {:ok, a, i}
        end
      end

      {_, a, i} = t2.(t2, a, i)
      _ = a
      _ = i
      idx = 0
      _ = idx

      t3 = fn t3, a, idx ->
        try do
          if idx < length(addr) do
            ch = Enum.slice(addr, idx, idx + 1 - idx)
            c = indexOf(tmpl, ch)
            _ = c

            if c < 0 do
              throw({:return, []})
            end

            j = 24
            _ = j

            t4 = fn t4, a, c, j ->
              try do
                if j >= 0 do
                  c = c + 58 * Enum.at(a, j)
                  a = Map.put(a, j, rem(c, 256))
                  c = String.to_integer(c / 256)
                  j = j - 1
                  t4.(t4, a, c, j)
                else
                  {:ok, a, c, j}
                end
              catch
                :break ->
                  {:ok, a, c, j}
              end
            end

            {_, a, c, j} = t4.(t4, a, c, j)
            _ = a
            _ = c
            _ = j

            if c > 0 do
              throw({:return, []})
            end

            idx = idx + 1
            t3.(t3, a, idx)
          else
            {:ok, a, idx}
          end
        catch
          :break ->
            {:ok, a, idx}
        end
      end

      {_, a, idx} = t3.(t3, a, idx)
      _ = a
      _ = idx
      throw({:return, a})
    catch
      {:return, v} -> v
    end
  end

  @spec doubleSHA256(list(integer())) :: list(integer())
  def doubleSHA256(bs) do
    try do
      # first :: (list(any()) -> any())
      first = sha256.(bs)
      throw({:return, sha256.(first)})
    catch
      {:return, v} -> v
    end
  end

  @spec computeChecksum(list(integer())) :: list(integer())
  def computeChecksum(a) do
    try do
      hash = doubleSHA256(Enum.slice(a, 0, 21 - 0))
      throw({:return, Enum.slice(hash, 0, 4 - 0)})
    catch
      {:return, v} -> v
    end
  end

  @spec validA58(String.t()) :: boolean()
  def validA58(addr) do
    try do
      a = set58(addr)

      if length(a) != 25 do
        throw({:return, false})
      end

      if Enum.at(a, 0) != 0 do
        throw({:return, false})
      end

      # sum :: (any() -> float())
      sum = computeChecksum(a)
      i = 0
      _ = i

      t5 = fn t5, i ->
        try do
          if i < 4 do
            if Enum.at(a, 21 + i) != Enum.at(sum, i) do
              throw({:return, false})
            end

            i = i + 1
            t5.(t5, i)
          else
            {:ok, i}
          end
        catch
          :break ->
            {:ok, i}
        end
      end

      {_, i} = t5.(t5, i)
      _ = i
      throw({:return, true})
    catch
      {:return, v} -> v
    end
  end

  def main do
    IO.puts(to_string(validA58("1AGNa15ZQXAZUgFiqJ3i7Z2DPU2J6hW62i")))
    IO.puts(to_string(validA58("17NdbrSGoUotzeGCcMMCqnFkEvLymoou9j")))
  end
end

Main.main()
