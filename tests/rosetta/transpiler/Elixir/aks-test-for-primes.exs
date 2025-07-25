# Code generated by Mochi transpiler 2025-07-25 18:41 +0700
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
  def poly(p) do
    try do
      s = ""
      coef = 1
      i = p
      {s} = if coef != 1 do
        s = (s <> to_string(coef))
        {s}
      else
        {s}
      end
      while_fun = fn while_fun, coef, i, s ->
        if i > 0 do
          s = (s <> "x")
          if i != 1 do
            s = ((s <> "^") <> to_string(i))
          end
          coef = trunc((coef * i / (p - i + 1)))
          d = coef
          {d} = if rem((p - (i - 1)), 2) == 1 do
            d = -d
            {d}
          else
            {d}
          end
          s = if d < 0, do: ((s <> " - ") <> to_string(-d)), else: ((s <> " + ") <> to_string(d))
          i = i - 1
          while_fun.(while_fun, coef, i, s)
        else
          {coef, i, s}
        end
      end
      {coef, i, s} = try do
          while_fun.(while_fun, coef, i, s)
        catch
          :break -> {coef, i, s}
        end

      {s} = if s == "" do
        s = "1"
        {s}
      else
        {s}
      end
      throw {:return, s}
    catch
      {:return, val} -> val
    end
  end
  def aks(n) do
    try do
      if n < 2 do
        throw {:return, false}
      end
      c = n
      i = 1
      while_fun_2 = fn while_fun_2, c, i ->
        if i < n do
          if rem(c, n) != 0 do
            throw {:return, false}
          end
          c = trunc((c * (n - i) / (i + 1)))
          i = i + 1
          while_fun_2.(while_fun_2, c, i)
        else
          {c, i}
        end
      end
      {c, i} = try do
          while_fun_2.(while_fun_2, c, i)
        catch
          :break -> {c, i}
        end

      throw {:return, true}
    catch
      {:return, val} -> val
    end
  end
  def main() do
    try do
      p = 0
      while_fun_3 = fn while_fun_3, p ->
        if p <= 7 do
          IO.puts(((to_string(p) <> ":  ") <> poly(p)))
          p = p + 1
          while_fun_3.(while_fun_3, p)
        else
          p
        end
      end
      p = try do
          while_fun_3.(while_fun_3, p)
        catch
          :break -> p
        end

      first = true
      p = 2
      line = ""
      while_fun_4 = fn while_fun_4, first, line, p ->
        if p < 50 do
          if aks(p) do
            if first do
              line = (line <> to_string(p))
              first = false
            else
              line = ((line <> " ") <> to_string(p))
            end
          end
          p = p + 1
          while_fun_4.(while_fun_4, first, line, p)
        else
          {first, line, p}
        end
      end
      {first, line, p} = try do
          while_fun_4.(while_fun_4, first, line, p)
        catch
          :break -> {first, line, p}
        end

      IO.puts(line)
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
