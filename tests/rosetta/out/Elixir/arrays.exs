# Generated by Mochi compiler v0.10.26 on 2025-07-16T12:45:39Z
defmodule Main do
  @spec listStr(list(integer())) :: String.t()
  def listStr(xs) do
    try do
      # s :: list(integer())
      s = "["
      _ = s
      i = 0
      _ = i

      t1 = fn t1, i, s ->
        try do
          if i < length(xs) do
            s = s ++ to_string(Enum.at(xs, i))

            if i + 1 < length(xs) do
              s = s ++ " "
            end

            i = i + 1
            t1.(t1, i, s)
          else
            {:ok, i, s}
          end
        catch
          :break ->
            {:ok, i, s}
        end
      end

      {_, i, s} = t1.(t1, i, s)
      _ = i
      _ = s
      s = s ++ "]"
      throw({:return, s})
    catch
      {:return, v} -> v
    end
  end

  def main do
    # a :: list(integer())
    a = [0, 0, 0, 0, 0]
    _ = a
    IO.puts("len(a) = " <> to_string(length(a)))
    IO.puts("a = " <> listStr(a))
    a = List.replace_at(a, 0, 3)
    IO.puts("a = " <> listStr(a))
    IO.puts("a[0] = " <> to_string(Enum.at(a, 0)))
    # s :: list(integer())
    s = Enum.slice(a, 0, 4 - 0)
    _ = s
    # cap_s :: integer()
    cap_s = 5
    _ = cap_s
    IO.puts("s = " <> listStr(s))
    IO.puts((("len(s) = " <> to_string(length(s))) <> "  cap(s) = ") <> to_string(cap_s))
    s = Enum.slice(a, 0, 5 - 0)
    IO.puts("s = " <> listStr(s))
    a = List.replace_at(a, 0, 22)
    s = List.replace_at(s, 0, 22)
    IO.puts("a = " <> listStr(a))
    IO.puts("s = " <> listStr(s))
    s = s ++ [4]
    s = s ++ [5]
    s = s ++ [6]
    cap_s = 10
    IO.puts("s = " <> listStr(s))
    IO.puts((("len(s) = " <> to_string(length(s))) <> "  cap(s) = ") <> to_string(cap_s))
    a = List.replace_at(a, 4, -1)
    IO.puts("a = " <> listStr(a))
    IO.puts("s = " <> listStr(s))
    s = []

    {s} =
      Enum.reduce(0..(8 - 1), {s}, fn i, {s} ->
        s = s ++ [0]
        {s}
      end)

    _ = s
    cap_s = 8
    IO.puts("s = " <> listStr(s))
    IO.puts((("len(s) = " <> to_string(length(s))) <> "  cap(s) = ") <> to_string(cap_s))
  end
end

Main.main()
