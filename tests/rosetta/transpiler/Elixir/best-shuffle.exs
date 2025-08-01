# Code generated by Mochi transpiler 2025-07-27 05:59 +0700
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
  defp _len(x) do
    if is_binary(x), do: String.length(x), else: length(x)
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
  def nextRand(seed) do
    try do
      throw {:return, rem((seed * 1664525 + 1013904223), 2147483647)}
    catch
      {:return, val} -> val
    end
  end
  def shuffleChars(s, seed) do
    try do
      chars = []
      i = 0
      while_fun = fn while_fun, chars, i ->
        if i < String.length(s) do
          chars = (chars ++ [String.slice(s, i, i + 1 - (i))])
          i = i + 1
          while_fun.(while_fun, chars, i)
        else
          {chars, i}
        end
      end
      {chars, i} = try do
          while_fun.(while_fun, chars, i)
        catch
          :break -> {chars, i}
        end

      sd = seed
      idx = _len(chars) - 1
      while_fun_2 = fn while_fun_2, chars, idx, sd ->
        if idx > 0 do
          sd = Main.nextRand(sd)
          j = rem(sd, (idx + 1))
          tmp = Enum.at(chars, idx)
          chars = List.replace_at(chars, idx, Enum.at(chars, j))
          chars = List.replace_at(chars, j, tmp)
          idx = idx - 1
          while_fun_2.(while_fun_2, chars, idx, sd)
        else
          {chars, idx, sd}
        end
      end
      {chars, idx, sd} = try do
          while_fun_2.(while_fun_2, chars, idx, sd)
        catch
          :break -> {chars, idx, sd}
        end

      res = ""
      i = 0
      while_fun_3 = fn while_fun_3, i, res ->
        if i < _len(chars) do
          res = (res <> Enum.at(chars, i))
          i = i + 1
          while_fun_3.(while_fun_3, i, res)
        else
          {i, res}
        end
      end
      {i, res} = try do
          while_fun_3.(while_fun_3, i, res)
        catch
          :break -> {i, res}
        end

      throw {:return, [res, sd]}
    catch
      {:return, val} -> val
    end
  end
  def bestShuffle(s, seed) do
    try do
      r = Main.shuffleChars(s, seed)
      t = Enum.at(r, 0)
      sd = Enum.at(r, 1)
      arr = []
      i = 0
      while_fun_4 = fn while_fun_4, arr, i ->
        if i < _len(t) do
          arr = (arr ++ [String.slice(t, i, i + 1 - (i))])
          i = i + 1
          while_fun_4.(while_fun_4, arr, i)
        else
          {arr, i}
        end
      end
      {arr, i} = try do
          while_fun_4.(while_fun_4, arr, i)
        catch
          :break -> {arr, i}
        end

      i = 0
      while_fun_5 = fn while_fun_5, arr, i ->
        if i < _len(arr) do
          j = 0
          while_fun_6 = fn while_fun_6, arr, j ->
            if j < _len(arr) do
              {arr} = if i != j && Enum.at(arr, i) != String.slice(s, j, j + 1 - (j)) && Enum.at(arr, j) != String.slice(s, i, i + 1 - (i)) do
                tmp = Enum.at(arr, i)
                arr = List.replace_at(arr, i, Enum.at(arr, j))
                arr = List.replace_at(arr, j, tmp)
                throw :break
                {arr}
              else
                {arr}
              end
              j = j + 1
              while_fun_6.(while_fun_6, arr, j)
            else
              {arr, j}
            end
          end
          {arr, j} = try do
              while_fun_6.(while_fun_6, arr, j)
            catch
              :break -> {arr, j}
            end

          i = i + 1
          while_fun_5.(while_fun_5, arr, i)
        else
          {arr, i}
        end
      end
      {arr, i} = try do
          while_fun_5.(while_fun_5, arr, i)
        catch
          :break -> {arr, i}
        end

      count = 0
      i = 0
      while_fun_7 = fn while_fun_7, count, i ->
        if i < _len(arr) do
          {count} = if Enum.at(arr, i) == String.slice(s, i, i + 1 - (i)) do
            count = count + 1
            {count}
          else
            {count}
          end
          i = i + 1
          while_fun_7.(while_fun_7, count, i)
        else
          {count, i}
        end
      end
      {count, i} = try do
          while_fun_7.(while_fun_7, count, i)
        catch
          :break -> {count, i}
        end

      out = ""
      i = 0
      while_fun_8 = fn while_fun_8, i, out ->
        if i < _len(arr) do
          out = (out <> Enum.at(arr, i))
          i = i + 1
          while_fun_8.(while_fun_8, i, out)
        else
          {i, out}
        end
      end
      {i, out} = try do
          while_fun_8.(while_fun_8, i, out)
        catch
          :break -> {i, out}
        end

      throw {:return, [out, sd, count]}
    catch
      {:return, val} -> val
    end
  end
  def main() do
    try do
      ts = ["abracadabra", "seesaw", "elk", "grrrrrr", "up", "a"]
      seed = 1
      i = 0
      while_fun_9 = fn while_fun_9, i, seed ->
        if i < _len(ts) do
          r = Main.bestShuffle(Enum.at(ts, i), seed)
          shuf = Enum.at(r, 0)
          seed = Enum.at(r, 1)
          cnt = Enum.at(r, 2)
          IO.puts((((((Enum.at(ts, i) <> " -> ") <> shuf) <> " (") <> Kernel.inspect(cnt)) <> ")"))
          i = i + 1
          while_fun_9.(while_fun_9, i, seed)
        else
          {i, seed}
        end
      end
      {i, seed} = try do
          while_fun_9.(while_fun_9, i, seed)
        catch
          :break -> {i, seed}
        end

    catch
      {:return, val} -> val
    end
  end
end
Main.main()
