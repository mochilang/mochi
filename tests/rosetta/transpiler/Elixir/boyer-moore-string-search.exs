# Code generated by Mochi transpiler 2025-07-27 22:38 +0700
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
  defp _clamp_slice(n, start, stop) do
    start = if start < 0, do: start + n, else: start
    stop = if stop < 0, do: stop + n, else: stop
    start = max(min(start, n), 0)
    stop = min(max(stop, start), n)
    {start, stop}
  end
  defp _slice(base, start, len) do
    cond do
      is_binary(base) ->
        chars = String.graphemes(base)
        n = length(chars)
        {s, e} = _clamp_slice(n, start, start + len)
        Enum.slice(chars, s, e - s) |> Enum.join("")
      true ->
        n = length(base)
        {s, e} = _clamp_slice(n, start, start + len)
        Enum.slice(base, s, e - s)
    end
  end
  defp _len(x) do
    cond do
      x == nil -> 0
      is_binary(x) -> String.length(x)
      true -> length(x)
    end
  end
  defp _bigrat(v) do
    _bigrat(v, 1)
  end
  defp _bigrat(n, d) do
    g = Integer.gcd(n, d)
    n = div(n, g)
    d = div(d, g)
    if d < 0 do
      {-n, -d}
    else
      {n, d}
    end
  end
  defp _bigrat_add(a, b) do
    {an, ad} = a
    {bn, bd} = b
    _bigrat(an * bd + ad * bn, ad * bd)
  end
  defp _bigrat_sub(a, b) do
    {an, ad} = a
    {bn, bd} = b
    _bigrat(an * bd - ad * bn, ad * bd)
  end
  defp _bigrat_mul(a, b) do
    {an, ad} = a
    {bn, bd} = b
    _bigrat(an * bn, ad * bd)
  end
  defp _bigrat_div(a, b) do
    {an, ad} = a
    {bn, bd} = b
    _bigrat(an * bd, ad * bn)
  end
  defp _bigrat_neg(a) do
    {n, d} = a
    {-n, d}
  end
  defp _sha256(bs) do
    bin = :erlang.list_to_binary(bs)
    :crypto.hash(:sha256, bin) |> :erlang.binary_to_list()
  end
  def indexOfStr(h, n) do
    try do
      hlen = _len(h)
      nlen = _len(n)
      if nlen == 0 do
        throw {:return, 0}
      end
      i = 0
      while_fun = fn while_fun, i ->
        if i <= hlen - nlen do
          if _slice(h, i, i + nlen - (i)) == n do
            throw {:return, i}
          end
          i = i + 1
          while_fun.(while_fun, i)
        else
          i
        end
      end
      i = try do
          while_fun.(while_fun, i)
        catch
          :break -> i
        end

      throw {:return, -1}
    catch
      {:return, val} -> val
    end
  end
  def stringSearchSingle(h, n) do
    try do
      throw {:return, Main.indexOfStr(h, n)}
    catch
      {:return, val} -> val
    end
  end
  def stringSearch(h, n) do
    try do
      result = []
      start = 0
      hlen = _len(h)
      nlen = _len(n)
      while_fun_2 = fn while_fun_2, result, start ->
        if start < hlen do
          idx = Main.indexOfStr(_slice(h, start, hlen - (start)), n)
          {result, start} = if idx >= 0 do
            result = (result ++ [start + idx])
            start = start + idx + nlen
            {result, start}
          else
            throw :break
            {result, start}
          end
          while_fun_2.(while_fun_2, result, start)
        else
          {result, start}
        end
      end
      {result, start} = try do
          while_fun_2.(while_fun_2, result, start)
        catch
          :break -> {result, start}
        end

      throw {:return, result}
    catch
      {:return, val} -> val
    end
  end
  def display(nums) do
    try do
      s = "["
      i = 0
      while_fun_3 = fn while_fun_3, i, s ->
        if i < _len(nums) do
          {s} = if i > 0 do
            s = (s <> ", ")
            {s}
          else
            {s}
          end
          s = (s <> Kernel.to_string(Enum.at(nums, i)))
          i = i + 1
          while_fun_3.(while_fun_3, i, s)
        else
          {i, s}
        end
      end
      {i, s} = try do
          while_fun_3.(while_fun_3, i, s)
        catch
          :break -> {i, s}
        end

      s = (s <> "]")
      throw {:return, s}
    catch
      {:return, val} -> val
    end
  end
  def main() do
    try do
      texts = ["GCTAGCTCTACGAGTCTA", "GGCTATAATGCGTA", "there would have been a time for such a word", "needle need noodle needle", "DKnuthusesandprogramsanimaginarycomputertheMIXanditsassociatedmachinecodeandassemblylanguages", "Nearby farms grew an acre of alfalfa on the dairy's behalf, with bales of that alfalfa exchanged for milk."]
      patterns = ["TCTA", "TAATAAA", "word", "needle", "and", "alfalfa"]
      i = 0
      while_fun_4 = fn while_fun_4, i ->
        if i < _len(texts) do
          IO.puts(((("text" <> Kernel.to_string(i + 1)) <> " = ") <> Enum.at(texts, i)))
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

      IO.puts("")
      j = 0
      while_fun_5 = fn while_fun_5, j ->
        if j < _len(texts) do
          idxs = Main.stringSearch(Enum.at(texts, j), Enum.at(patterns, j))
          IO.puts(((((("Found \"" <> Enum.at(patterns, j)) <> "\" in 'text") <> Kernel.to_string(j + 1)) <> "' at indexes ") <> Main.display(idxs)))
          j = j + 1
          while_fun_5.(while_fun_5, j)
        else
          j
        end
      end
      j = try do
          while_fun_5.(while_fun_5, j)
        catch
          :break -> j
        end

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
