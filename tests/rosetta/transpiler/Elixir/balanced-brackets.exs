# Code generated by Mochi transpiler 2025-07-27 01:41 +0700
defmodule Main do
  defp _now() do
    seeded = Process.get(:_now_seeded, false)
    seed = Process.get(:_now_seed, 0)
    if !seeded do
      case System.get_env("MOCHI_NOW_SEED") do
        nil -> :ok
        s ->
          case Integer.parse(s) do
            {v, ""} ->
              Process.put(:_now_seed, v)
              Process.put(:_now_seeded, true)
              seed = v
              seeded = true
            _ -> :ok
          end
      end
    end
    if seeded do
      seed = rem(seed * 1664525 + 1013904223, 2147483647)
      Process.put(:_now_seed, seed)
      abs(seed)
    else
      abs(System.os_time(:nanosecond))
    end
  end
  defp _mem() do
    :erlang.memory(:total)
  end
  defp _lookup_host(host) do
    case :inet.gethostbyname(String.to_charlist(host)) do
      {:ok, {:hostent, _, _, _, _, addrs}} ->
        ips = Enum.map(addrs, &:inet.ntoa/1)
        [ips, nil]
      {:error, reason} ->
        [nil, reason]
    end
  end
  defp _slice(base, start, len) do
    cond do
      is_binary(base) -> String.slice(base, start, len)
      len == 1 -> Enum.slice(base, start, len)
      true -> Enum.slice(base, start, len)
    end
  end
  def prng(max) do
    try do
      Process.put(:seed, rem((Process.get(:seed) * 1103515245 + 12345), 2147483648))
      throw {:return, rem(Process.get(:seed), max)}
    catch
      {:return, val} -> val
    end
  end
  def gen(n) do
    try do
      arr = []
      i = 0
      while_fun = fn while_fun, arr, i ->
        if i < n do
          arr = (arr ++ ["["])
          arr = (arr ++ ["]"])
          i = i + 1
          while_fun.(while_fun, arr, i)
        else
          {arr, i}
        end
      end
      {arr, i} = try do
          while_fun.(while_fun, arr, i)
        catch
          :break -> {arr, i}
        end

      j = length(arr) - 1
      while_fun_2 = fn while_fun_2, arr, j ->
        if j > 0 do
          k = Main.prng(j + 1)
          tmp = Enum.at(arr, j)
          arr = List.replace_at(arr, j, Enum.at(arr, k))
          arr = List.replace_at(arr, k, tmp)
          j = j - 1
          while_fun_2.(while_fun_2, arr, j)
        else
          {arr, j}
        end
      end
      {arr, j} = try do
          while_fun_2.(while_fun_2, arr, j)
        catch
          :break -> {arr, j}
        end

      out = ""
      {out} = Enum.reduce(arr, {out}, fn ch, {out} ->
        out = (out <> ch)
        {out}
      end)
      throw {:return, out}
    catch
      {:return, val} -> val
    end
  end
  def testBalanced(s) do
    try do
      open = 0
      i = 0
      while_fun_3 = fn while_fun_3, i, open ->
        if i < String.length(s) do
          c = String.slice(s, i, i + 1 - i)
          {open} = if c == "[" do
            open = open + 1
            {open}
          else
            {open} = if c == "]" do
              if open == 0 do
                IO.puts((s <> ": not ok"))
                throw {:return, nil}
              end
              open = open - 1
              {open}
            else
              IO.puts((s <> ": not ok"))
              throw {:return, nil}
              {open}
            end
            {open}
          end
          i = i + 1
          while_fun_3.(while_fun_3, i, open)
        else
          {i, open}
        end
      end
      {i, open} = try do
          while_fun_3.(while_fun_3, i, open)
        catch
          :break -> {i, open}
        end

      if open == 0 do
        IO.puts((s <> ": ok"))
      else
        IO.puts((s <> ": not ok"))
      end
    catch
      {:return, val} -> val
    end
  end
  def main() do
    try do
      i = 0
      while_fun_4 = fn while_fun_4, i ->
        if i < 10 do
          Main.testBalanced(Main.gen(i))
          i = i + 1
          while_fun_4.(while_fun_4, i)
        else
          i
        end
      end
      i = try do
          while_fun_4.(while_fun_4, i)
        catch
          :break -> i
        end

      Main.testBalanced("()")
    catch
      {:return, val} -> val
    end
  end
  Process.put(:seed, 1)
  def bench_main() do
    Process.put(:seed, 1)
    mem_start = _mem()
    t_start = _now()
    main()
    duration_us = div(_now() - t_start, 1000)
    mem_diff = abs(_mem() - mem_start)
    IO.puts("{\n  \"duration_us\": #{duration_us},\n  \"memory_bytes\": #{mem_diff},\n  \"name\": \"main\"\n}")
  end
end
Main.bench_main()
