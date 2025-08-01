# Code generated by Mochi transpiler 2025-07-26 19:01 +0700
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
      seed
    else
      System.os_time(:nanosecond)
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
      len == 1 -> Enum.at(base, start)
      true -> Enum.slice(base, start, len)
    end
  end
  def abs_(x) do
    try do
      throw {:return, (if x < 0.0, do: -x, else: x)}
    catch
      {:return, val} -> val
    end
  end
  def maxf(a, b) do
    try do
      throw {:return, (if a > b, do: a, else: b)}
    catch
      {:return, val} -> val
    end
  end
  def isClose(a, b) do
    try do
      relTol = 0.000000001
      t = abs_(a - b)
      u = relTol * maxf(abs_(a), abs_(b))
      throw {:return, t <= u}
    catch
      {:return, val} -> val
    end
  end
  def sqrtApprox(x) do
    try do
      guess = x
      i = 0
      while_fun = fn while_fun, guess, i ->
        if i < 10 do
          guess = (guess + x / guess) / 2.0
          i = i + 1
          while_fun.(while_fun, guess, i)
        else
          {guess, i}
        end
      end
      {guess, i} = try do
          while_fun.(while_fun, guess, i)
        catch
          :break -> {guess, i}
        end

      throw {:return, guess}
    catch
      {:return, val} -> val
    end
  end
  def main() do
    try do
      root2 = sqrtApprox(2.0)
      pairs = [[100000000000000.02, 100000000000000.02], [100.01, 100.011], [10000000000000.002 / 10000.0, 1000000000.0000001], [0.001, 0.0010000001], [0.000000000000000000000101, 0.0], [root2 * root2, 2.0], [(-root2) * root2, -2.0], [100000000000000000.0, 100000000000000000.0], [3.141592653589793, 3.141592653589793]]
      Enum.each(pairs, fn pair ->
        a = Enum.at(pair, 0)
        b = Enum.at(pair, 1)
        s = if isClose(a, b), do: "≈", else: "≉"
        IO.puts(((((to_string(a) <> " ") <> s) <> " ") <> to_string(b)))
      end)
    catch
      {:return, val} -> val
    end
  end
  def bench_main() do
    mem_start = _mem()
    t_start = _now()
    main()
    duration_us = div(_now() - t_start, 1000)
    mem_diff = abs(_mem() - mem_start)
    IO.puts("{\n  \"duration_us\": #{duration_us},\n  \"memory_bytes\": #{mem_diff},\n  \"name\": \"main\"\n}")
  end
end
Main.bench_main()
