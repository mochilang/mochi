# Code generated by Mochi transpiler 2025-07-28 11:41 +0700
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
  def countChange(amount) do
    try do
      ways = []
      i = 0
      while_fun = fn while_fun, i, ways ->
        if i <= amount do
          ways = (ways ++ [0])
          i = i + 1
          while_fun.(while_fun, i, ways)
        else
          {i, ways}
        end
      end
      {i, ways} = try do
          while_fun.(while_fun, i, ways)
        catch
          {:break, {i, ways}} -> {i, ways}
        end

      ways = List.replace_at(ways, 0, 1)
      {ways} = Enum.reduce([100, 50, 25, 10, 5, 1], {ways}, fn coin, {ways} ->
        j = coin
        while_fun_2 = fn while_fun_2, j, ways ->
          if j <= amount do
            ways = List.replace_at(ways, j, Enum.at(ways, j) + Enum.at(ways, j - coin))
            j = j + 1
            while_fun_2.(while_fun_2, j, ways)
          else
            {j, ways}
          end
        end
        {j, ways} = try do
            while_fun_2.(while_fun_2, j, ways)
          catch
            {:break, {j, ways}} -> {j, ways}
          end

        {ways}
      end)
      throw {:return, Enum.at(ways, amount)}
    catch
      {:return, val} -> val
    end
  end
  Process.put(:amount, 1000)
  def main() do
    mem_start = _mem()
    t_start = _now()
    IO.puts(((("amount, ways to make change: " <> Kernel.to_string(Process.get(:amount))) <> " ") <> Kernel.inspect(Main.countChange(Process.get(:amount)))))
    duration_us = div(_now() - t_start, 1000)
    mem_diff = abs(_mem() - mem_start)
    IO.puts("{\n  \"duration_us\": #{duration_us},\n  \"memory_bytes\": #{mem_diff},\n  \"name\": \"main\"\n}")
  end
end
Main.main()
