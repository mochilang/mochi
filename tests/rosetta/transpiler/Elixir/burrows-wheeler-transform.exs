# Code generated by Mochi transpiler 2025-07-28 00:36 +0700
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
    n = trunc(n)
    d = trunc(d)
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
  def contains(s, ch) do
    try do
      i = 0
      while_fun = fn while_fun, i ->
        if i < _len(s) do
          if _slice(s, i, i + 1 - (i)) == ch do
            throw {:return, true}
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
          {:break, i} -> i
        end

      throw {:return, false}
    catch
      {:return, val} -> val
    end
  end
  def sortStrings(xs) do
    try do
      arr = xs
      n = _len(arr)
      i = 0
      while_fun_2 = fn while_fun_2, arr, i ->
        if i < n do
          j = 0
          while_fun_3 = fn while_fun_3, arr, j ->
            if j < n - 1 do
              {arr} = if Enum.at(arr, j) > Enum.at(arr, j + 1) do
                tmp = Enum.at(arr, j)
                arr = List.replace_at(arr, j, Enum.at(arr, j + 1))
                arr = List.replace_at(arr, j + 1, tmp)
                {arr}
              else
                {arr}
              end
              j = j + 1
              while_fun_3.(while_fun_3, arr, j)
            else
              {arr, j}
            end
          end
          {arr, j} = try do
              while_fun_3.(while_fun_3, arr, j)
            catch
              {:break, {arr, j}} -> {arr, j}
            end

          i = i + 1
          while_fun_2.(while_fun_2, arr, i)
        else
          {arr, i}
        end
      end
      {arr, i} = try do
          while_fun_2.(while_fun_2, arr, i)
        catch
          {:break, {arr, i}} -> {arr, i}
        end

      throw {:return, arr}
    catch
      {:return, val} -> val
    end
  end
  def bwt(s) do
    try do
      if Main.contains(s, Process.get(:stx)) || Main.contains(s, Process.get(:etx)) do
        throw {:return, %{"err" => true, "res" => ""}}
      end
      s = ((Process.get(:stx) <> s) <> Process.get(:etx))
      le = _len(s)
      table = []
      i = 0
      while_fun_4 = fn while_fun_4, i, table ->
        if i < le do
          rot = (_slice(s, i, le - (i)) <> _slice(s, 0, i - (0)))
          table = (table ++ [rot])
          i = i + 1
          while_fun_4.(while_fun_4, i, table)
        else
          {i, table}
        end
      end
      {i, table} = try do
          while_fun_4.(while_fun_4, i, table)
        catch
          {:break, {i, table}} -> {i, table}
        end

      table = Main.sortStrings(table)
      last = ""
      i = 0
      while_fun_5 = fn while_fun_5, i, last ->
        if i < le do
          last = (last <> _slice(Enum.at(table, i), le - 1, le - (le - 1)))
          i = i + 1
          while_fun_5.(while_fun_5, i, last)
        else
          {i, last}
        end
      end
      {i, last} = try do
          while_fun_5.(while_fun_5, i, last)
        catch
          {:break, {i, last}} -> {i, last}
        end

      throw {:return, %{"err" => false, "res" => last}}
    catch
      {:return, val} -> val
    end
  end
  def ibwt(r) do
    try do
      le = _len(r)
      table = []
      i = 0
      while_fun_6 = fn while_fun_6, i, table ->
        if i < le do
          table = (table ++ [""])
          i = i + 1
          while_fun_6.(while_fun_6, i, table)
        else
          {i, table}
        end
      end
      {i, table} = try do
          while_fun_6.(while_fun_6, i, table)
        catch
          {:break, {i, table}} -> {i, table}
        end

      n = 0
      while_fun_7 = fn while_fun_7, i, n, table ->
        if n < le do
          i = 0
          while_fun_8 = fn while_fun_8, i, table ->
            if i < le do
              table = List.replace_at(table, i, (_slice(r, i, i + 1 - (i)) <> Enum.at(table, i)))
              i = i + 1
              while_fun_8.(while_fun_8, i, table)
            else
              {i, table}
            end
          end
          {i, table} = try do
              while_fun_8.(while_fun_8, i, table)
            catch
              {:break, {i, table}} -> {i, table}
            end

          table = Main.sortStrings(table)
          n = n + 1
          while_fun_7.(while_fun_7, i, n, table)
        else
          {i, n, table}
        end
      end
      {i, n, table} = try do
          while_fun_7.(while_fun_7, i, n, table)
        catch
          {:break, {i, n, table}} -> {i, n, table}
        end

      i = 0
      while_fun_9 = fn while_fun_9, i ->
        if i < le do
          if _slice(Enum.at(table, i), le - 1, le - (le - 1)) == Process.get(:etx) do
            throw {:return, _slice(Enum.at(table, i), 1, le - 1 - (1))}
          end
          i = i + 1
          while_fun_9.(while_fun_9, i)
        else
          i
        end
      end
      i = try do
          while_fun_9.(while_fun_9, i)
        catch
          {:break, i} -> i
        end

      throw {:return, ""}
    catch
      {:return, val} -> val
    end
  end
  def makePrintable(s) do
    try do
      out = ""
      i = 0
      while_fun_10 = fn while_fun_10, i, out ->
        if i < _len(s) do
          ch = _slice(s, i, i + 1 - (i))
          {out} = if ch == Process.get(:stx) do
            out = (out <> "^")
            {out}
          else
            {out} = if ch == Process.get(:etx) do
              out = (out <> "|")
              {out}
            else
              out = (out <> ch)
              {out}
            end
            {out}
          end
          i = i + 1
          while_fun_10.(while_fun_10, i, out)
        else
          {i, out}
        end
      end
      {i, out} = try do
          while_fun_10.(while_fun_10, i, out)
        catch
          {:break, {i, out}} -> {i, out}
        end

      throw {:return, out}
    catch
      {:return, val} -> val
    end
  end
  def main() do
    try do
      examples = ["banana", "appellee", "dogwood", "TO BE OR NOT TO BE OR WANT TO BE OR NOT?", "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES", "\x02ABC\x03"]
      Enum.each(examples, fn t ->
        IO.puts(Kernel.inspect(Main.makePrintable(t)))
        res = Main.bwt(t)
        if res["err"] do
          IO.puts(" --> ERROR: String can't contain STX or ETX")
          IO.puts(" -->")
        else
          enc = res["res"]
          IO.puts((" --> " <> Main.makePrintable(enc)))
          r = Main.ibwt(enc)
          IO.puts((" --> " <> r))
        end
        IO.puts("")
      end)
    catch
      {:return, val} -> val
    end
  end
  Process.put(:stx, "\x02")
  Process.put(:etx, "\x03")
  def bench_main() do
    Process.put(:stx, "\x02")
    Process.put(:etx, "\x03")
    mem_start = _mem()
    t_start = _now()
    main()
    duration_us = div(_now() - t_start, 1000)
    mem_diff = abs(_mem() - mem_start)
    IO.puts("{\n  \"duration_us\": #{duration_us},\n  \"memory_bytes\": #{mem_diff},\n  \"name\": \"main\"\n}")
  end
end
Main.bench_main()
