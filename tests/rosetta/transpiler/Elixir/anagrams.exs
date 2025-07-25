# Code generated by Mochi transpiler 2025-07-26 17:59 +0700
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
  def sortRunes(s) do
    try do
      arr = []
      i = 0
      while_fun = fn while_fun, arr, i ->
        if i < String.length(s) do
          arr = (arr ++ [String.slice(s, i, i + 1 - i)])
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

      n = length(arr)
      m = 0
      while_fun_2 = fn while_fun_2, arr, m ->
        if m < n do
          j = 0
          while_fun_3 = fn while_fun_3, arr, j ->
            if j < n - 1 do
              if Enum.at(arr, j) > Enum.at(arr, j + 1) do
                tmp = Enum.at(arr, j)
                arr = List.replace_at(arr, j, Enum.at(arr, j + 1))
                arr = List.replace_at(arr, j + 1, tmp)
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
              :break -> {arr, j}
            end

          m = m + 1
          while_fun_2.(while_fun_2, arr, m)
        else
          {arr, m}
        end
      end
      {arr, m} = try do
          while_fun_2.(while_fun_2, arr, m)
        catch
          :break -> {arr, m}
        end

      out = ""
      i = 0
      while_fun_4 = fn while_fun_4, i, out ->
        if i < n do
          out = (out <> Enum.at(arr, i))
          i = i + 1
          while_fun_4.(while_fun_4, i, out)
        else
          {i, out}
        end
      end
      {i, out} = try do
          while_fun_4.(while_fun_4, i, out)
        catch
          :break -> {i, out}
        end

      throw {:return, out}
    catch
      {:return, val} -> val
    end
  end
  def sortStrings(xs) do
    try do
      res = []
      tmp = xs
      while_fun_5 = fn while_fun_5, res, tmp ->
        if length(tmp) > 0 do
          min = Enum.at(tmp, 0)
          idx = 0
          i = 1
          while_fun_6 = fn while_fun_6, i, idx, min ->
            if i < length(tmp) do
              if Enum.at(tmp, i) < min do
                min = Enum.at(tmp, i)
                idx = i
              end
              i = i + 1
              while_fun_6.(while_fun_6, i, idx, min)
            else
              {i, idx, min}
            end
          end
          {i, idx, min} = try do
              while_fun_6.(while_fun_6, i, idx, min)
            catch
              :break -> {i, idx, min}
            end

          res = (res ++ [min])
          out = []
          j = 0
          while_fun_7 = fn while_fun_7, j, out ->
            if j < length(tmp) do
              if j != idx do
                out = (out ++ [Enum.at(tmp, j)])
              end
              j = j + 1
              while_fun_7.(while_fun_7, j, out)
            else
              {j, out}
            end
          end
          {j, out} = try do
              while_fun_7.(while_fun_7, j, out)
            catch
              :break -> {j, out}
            end

          tmp = out
          while_fun_5.(while_fun_5, res, tmp)
        else
          {res, tmp}
        end
      end
      {res, tmp} = try do
          while_fun_5.(while_fun_5, res, tmp)
        catch
          :break -> {res, tmp}
        end

      throw {:return, res}
    catch
      {:return, val} -> val
    end
  end
  def main() do
    try do
      words = ["abel", "able", "bale", "bela", "elba", "alger", "glare", "lager", "large", "regal", "angel", "angle", "galen", "glean", "lange", "caret", "carte", "cater", "crate", "trace", "elan", "lane", "lean", "lena", "neal", "evil", "levi", "live", "veil", "vile"]
      groups = %{}
      maxLen = 0
      {groups, maxLen} = Enum.reduce(words, {groups, maxLen}, fn w, {groups, maxLen} ->
        k = sortRunes(w)
        groups = Map.put(groups, k, (if !(Map.has_key?(groups, k)), do: [w], else: (groups[k] ++ [w])))
        if length(Map.get(groups, k, [])) > maxLen do
          maxLen = length(Map.get(groups, k, []))
        end
        {groups, maxLen}
      end)
      printed = %{}
      {printed} = Enum.reduce(words, {printed}, fn w, {printed} ->
        k = sortRunes(w)
        if length(Map.get(groups, k, [])) == maxLen do
          if !(Map.has_key?(printed, k)) do
            g = sortStrings(groups[k])
            line = ("[" <> Enum.at(g, 0))
            i = 1
            while_fun_8 = fn while_fun_8, i, line ->
              if i < length(g) do
                line = ((line <> " ") <> Enum.at(g, i))
                i = i + 1
                while_fun_8.(while_fun_8, i, line)
              else
                {i, line}
              end
            end
            {i, line} = try do
                while_fun_8.(while_fun_8, i, line)
              catch
                :break -> {i, line}
              end

            line = (line <> "]")
            IO.puts(line)
            printed = Map.put(printed, k, true)
          end
        end
        {printed}
      end)
    catch
      {:return, val} -> val
    end
  end
end
Main.main()
