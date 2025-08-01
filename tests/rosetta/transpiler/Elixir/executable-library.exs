# Code generated by Mochi transpiler 2025-07-31 00:20 +0700
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
  defp _getenv(name) do
    System.get_env(name)
  end
  defp _environ() do
    System.get_env() |> Enum.map(fn {k, v} -> "#{k}=#{v}" end)
  end
  def hailstone(n) do
    try do
      seq = []
      x = n
      seq = (seq ++ [x])
      while_fun = fn while_fun, seq, x ->
        if x > 1 do
          x = (if rem(x, 2) == 0, do: div(x, 2), else: 3 * x + 1)
          seq = (seq ++ [x])
          while_fun.(while_fun, seq, x)
        else
          {seq, x}
        end
      end
      {seq, x} = try do
          while_fun.(while_fun, seq, x)
        catch
          {:break, {seq, x}} -> {seq, x}
        end

      throw {:return, seq}
    catch
      {:return, val} -> val
    end
  end
  def listString(xs) do
    try do
      s = "["
      i = 0
      while_fun_2 = fn while_fun_2, i, s ->
        if i < _len(xs) do
          s = (s <> Kernel.to_string(Enum.at(xs, i)))
          {s} = if i < _len(xs) - 1 do
            s = (s <> " ")
            {s}
          else
            {s}
          end
          i = i + 1
          while_fun_2.(while_fun_2, i, s)
        else
          {i, s}
        end
      end
      {i, s} = try do
          while_fun_2.(while_fun_2, i, s)
        catch
          {:break, {i, s}} -> {i, s}
        end

      s = (s <> "]")
      throw {:return, s}
    catch
      {:return, val} -> val
    end
  end
  def libMain() do
    try do
      seq = Main.hailstone(27)
      IO.puts("")
      IO.puts("Hailstone sequence for the number 27:")
      IO.puts((("  has " <> Kernel.inspect(_len(seq))) <> " elements"))
      IO.puts(("  starts with " <> Main.listString(_slice(seq, 0, 4 - 0))))
      IO.puts(("  ends with " <> Main.listString(_slice(seq, _len(seq) - 4, _len(seq) - (_len(seq) - 4)))))
      longest = 0
      length = 0
      i = 1
      while_fun_3 = fn while_fun_3, i, length, longest ->
        if i < 100000 do
          l = _len(Main.hailstone(i))
          {length, longest} = if l > length do
            longest = i
            length = l
            {length, longest}
          else
            {length, longest}
          end
          i = i + 1
          while_fun_3.(while_fun_3, i, length, longest)
        else
          {i, length, longest}
        end
      end
      {i, length, longest} = try do
          while_fun_3.(while_fun_3, i, length, longest)
        catch
          {:break, {i, length, longest}} -> {i, length, longest}
        end

      IO.puts("")
      IO.puts((((Kernel.to_string(longest) <> " has the longest Hailstone sequence, its length being ") <> Kernel.to_string(length)) <> "."))
    catch
      {:return, val} -> val
    end
  end
  def main() do
    Main.libMain()
  end
end
Main.main()
