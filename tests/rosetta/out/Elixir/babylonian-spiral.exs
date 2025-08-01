# Generated by Mochi compiler v0.10.26 on 2025-07-16T12:46:10Z
defmodule Main do
  @spec push(list(map()), map()) :: list(map())
  def push(h, it) do
    try do
      h = h ++ [it]
      i = length(h) - 1
      _ = i

      t1 = fn t1, h, i ->
        try do
          if i > 0 && Enum.at(Enum.at(h, i - 1), "s") > Enum.at(Enum.at(h, i), "s") do
            tmp = Enum.at(h, i - 1)
            h = Map.put(h, i - 1, Enum.at(h, i))
            h = Map.put(h, i, tmp)
            i = i - 1
            t1.(t1, h, i)
          else
            {:ok, h, i}
          end
        catch
          :break ->
            {:ok, h, i}
        end
      end

      {_, h, i} = t1.(t1, h, i)
      _ = h
      _ = i
      throw({:return, h})
    catch
      {:return, v} -> v
    end
  end

  @spec step(list(map()), integer(), list(integer())) :: map()
  def step(h, nv, dir) do
    try do
      t2 = fn t2, h, nv ->
        try do
          if length(h) == 0 || nv * nv <= Enum.at(Enum.at(h, 0), "s") do
            h = push(h, %{"s" => nv * nv, "a" => nv, "b" => 0})
            nv = nv + 1
            t2.(t2, h, nv)
          else
            {:ok, h, nv}
          end
        catch
          :break ->
            {:ok, h, nv}
        end
      end

      {_, h, nv} = t2.(t2, h, nv)
      _ = h
      _ = nv
      s = Enum.at(Enum.at(h, 0), "s")
      v = []
      _ = v

      t3 = fn t3, h, v ->
        try do
          if length(h) > 0 && Enum.at(Enum.at(h, 0), "s") == s do
            it = Enum.at(h, 0)
            h = Enum.slice(h, 1, length(String.graphemes(h)) - 1)
            v = v ++ [[Enum.at(it, "a"), Enum.at(it, "b")]]

            if Enum.at(it, "a") > Enum.at(it, "b") do
              h =
                push(h, %{
                  "s" =>
                    Enum.at(it, "a") * Enum.at(it, "a") +
                      (Enum.at(it, "b") + 1) * (Enum.at(it, "b") + 1),
                  "a" => Enum.at(it, "a"),
                  "b" => Enum.at(it, "b") + 1
                })
            end

            t3.(t3, h, v)
          else
            {:ok, h, v}
          end
        catch
          :break ->
            {:ok, h, v}
        end
      end

      {_, h, v} = t3.(t3, h, v)
      _ = h
      _ = v
      list = []
      _ = list

      {list} =
        Enum.reduce(_iter(v), {list}, fn p, {list} ->
          list = list ++ [p]
          {list}
        end)

      _ = list
      temp = list
      _ = temp

      {list} =
        Enum.reduce(_iter(temp), {list}, fn p, {list} ->
          if Enum.at(p, 0) != Enum.at(p, 1) do
            list = list ++ [[Enum.at(p, 1), Enum.at(p, 0)]]
          end

          {list}
        end)

      _ = list
      temp = list

      {list} =
        Enum.reduce(_iter(temp), {list}, fn p, {list} ->
          if Enum.at(p, 1) != 0 do
            list = list ++ [[Enum.at(p, 0), -Enum.at(p, 1)]]
          end

          {list}
        end)

      _ = list
      temp = list

      {list} =
        Enum.reduce(_iter(temp), {list}, fn p, {list} ->
          if Enum.at(p, 0) != 0 do
            list = list ++ [[-Enum.at(p, 0), Enum.at(p, 1)]]
          end

          {list}
        end)

      _ = list
      bestDot = -999_999_999
      _ = bestDot
      best = dir
      _ = best

      {best, bestDot} =
        Enum.reduce(_iter(list), {best, bestDot}, fn p, {best, bestDot} ->
          cross = Enum.at(p, 0) * Enum.at(dir, 1) - Enum.at(p, 1) * Enum.at(dir, 0)

          if cross >= 0 do
            dot = Enum.at(p, 0) * Enum.at(dir, 0) + Enum.at(p, 1) * Enum.at(dir, 1)

            if dot > bestDot do
              bestDot = dot
              best = p
            end
          end

          {best, bestDot}
        end)

      _ = best
      _ = bestDot
      throw({:return, %{"d" => best, "heap" => h, "n" => nv}})
    catch
      {:return, v} -> v
    end
  end

  @spec positions(integer()) :: list(list(integer()))
  def positions(n) do
    try do
      pos = []
      _ = pos
      x = 0
      _ = x
      y = 0
      _ = y
      dir = [0, 1]
      _ = dir
      heap = []
      _ = heap
      nv = 1
      _ = nv
      i = 0
      _ = i

      t4 = fn t4, dir, heap, i, nv, pos, x, y ->
        try do
          if i < n do
            pos = pos ++ [[x, y]]
            st = step(heap, nv, dir)
            dir = Enum.at(st, "d")
            heap = Enum.at(st, "heap")
            nv = String.to_integer(Enum.at(st, "n"))
            x = x + Enum.at(dir, 0)
            y = y + Enum.at(dir, 1)
            i = i + 1
            t4.(t4, dir, heap, i, nv, pos, x, y)
          else
            {:ok, dir, heap, i, nv, pos, x, y}
          end
        catch
          :break ->
            {:ok, dir, heap, i, nv, pos, x, y}
        end
      end

      {_, dir, heap, i, nv, pos, x, y} = t4.(t4, dir, heap, i, nv, pos, x, y)
      _ = dir
      _ = heap
      _ = i
      _ = nv
      _ = pos
      _ = x
      _ = y
      throw({:return, pos})
    catch
      {:return, v} -> v
    end
  end

  @spec pad(String.t(), integer()) :: String.t()
  def pad(s, w) do
    try do
      r = s
      _ = r

      t5 = fn t5, r ->
        try do
          if length(r) < w do
            r = r <> " "
            t5.(t5, r)
          else
            {:ok, r}
          end
        catch
          :break ->
            {:ok, r}
        end
      end

      {_, r} = t5.(t5, r)
      _ = r
      throw({:return, r})
    catch
      {:return, v} -> v
    end
  end

  @spec main() :: nil
  def main() do
    try do
      pts = positions(40)
      IO.puts("The first 40 Babylonian spiral points are:")
      line = ""
      _ = line
      i = 0
      _ = i

      t6 = fn t6, i, line ->
        try do
          if i < length(pts) do
            p = Enum.at(pts, i)

            s =
              pad(
                ((("(" <> to_string(Enum.at(p, 0))) <> ", ") <> to_string(Enum.at(p, 1))) <> ")",
                10
              )

            line = line + s

            if rem(i + 1, 10) == 0 do
              IO.inspect(line)
              line = ""
            end

            i = i + 1
            t6.(t6, i, line)
          else
            {:ok, i, line}
          end
        catch
          :break ->
            {:ok, i, line}
        end
      end

      {_, i, line} = t6.(t6, i, line)
      _ = i
      _ = line
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
