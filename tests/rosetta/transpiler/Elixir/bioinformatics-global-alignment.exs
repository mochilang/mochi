# Code generated by Mochi transpiler 2025-07-27 16:31 +0700
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
  def padLeft(s, w) do
    try do
      res = ""
      n = w - _len(s)
      while_fun = fn while_fun, n, res ->
        if n > 0 do
          res = (res <> " ")
          n = n - 1
          while_fun.(while_fun, n, res)
        else
          {n, res}
        end
      end
      {n, res} = try do
          while_fun.(while_fun, n, res)
        catch
          :break -> {n, res}
        end

      throw {:return, (res <> s)}
    catch
      {:return, val} -> val
    end
  end
  def indexOfFrom(s, ch, start) do
    try do
      i = start
      while_fun_2 = fn while_fun_2, i ->
        if i < _len(s) do
          if _slice(s, i, i + 1 - (i)) == ch do
            throw {:return, i}
          end
          i = i + 1
          while_fun_2.(while_fun_2, i)
        else
          i
        end
      end
      i = try do
          while_fun_2.(while_fun_2, i)
        catch
          :break -> i
        end

      throw {:return, -1}
    catch
      {:return, val} -> val
    end
  end
  def containsStr(s, sub) do
    try do
      i = 0
      sl = _len(s)
      subl = _len(sub)
      while_fun_3 = fn while_fun_3, i ->
        if i <= sl - subl do
          if _slice(s, i, i + subl - (i)) == sub do
            throw {:return, true}
          end
          i = i + 1
          while_fun_3.(while_fun_3, i)
        else
          i
        end
      end
      i = try do
          while_fun_3.(while_fun_3, i)
        catch
          :break -> i
        end

      throw {:return, false}
    catch
      {:return, val} -> val
    end
  end
  def distinct(slist) do
    try do
      res = []
      {res} = Enum.reduce(slist, {res}, fn s, {res} ->
        try do
          found = false
          {found} = Enum.reduce(res, {found}, fn r, {found} ->
            try do
              {found} = if r == s do
                found = true
                throw :break
                {found}
              else
                {found}
              end
            catch
              :continue -> {found}
            end
            {found}
          end)
          {res} = if !found do
            res = (res ++ [s])
            {res}
          else
            {res}
          end
        catch
          :continue -> {res}
        end
        {res}
      end)
      throw {:return, res}
    catch
      {:return, val} -> val
    end
  end
  def permutations(xs) do
    try do
      if _len(xs) <= 1 do
        throw {:return, [xs]}
      end
      res = []
      i = 0
      while_fun_4 = fn while_fun_4, i, res ->
        if i < _len(xs) do
          rest = []
          j = 0
          while_fun_5 = fn while_fun_5, j, rest ->
            if j < _len(xs) do
              {rest} = if j != i do
                rest = (rest ++ [Enum.at(xs, j)])
                {rest}
              else
                {rest}
              end
              j = j + 1
              while_fun_5.(while_fun_5, j, rest)
            else
              {j, rest}
            end
          end
          {j, rest} = try do
              while_fun_5.(while_fun_5, j, rest)
            catch
              :break -> {j, rest}
            end

          subs = Main.permutations(rest)
          {res} = Enum.reduce(subs, {res}, fn p, {res} ->
            perm = [Enum.at(xs, i)]
            k = 0
            while_fun_6 = fn while_fun_6, k, perm ->
              if k < _len(p) do
                perm = (perm ++ [Enum.at(p, k)])
                k = k + 1
                while_fun_6.(while_fun_6, k, perm)
              else
                {k, perm}
              end
            end
            {k, perm} = try do
                while_fun_6.(while_fun_6, k, perm)
              catch
                :break -> {k, perm}
              end

            res = (res ++ [perm])
            {res}
          end)
          i = i + 1
          while_fun_4.(while_fun_4, i, res)
        else
          {i, res}
        end
      end
      {i, res} = try do
          while_fun_4.(while_fun_4, i, res)
        catch
          :break -> {i, res}
        end

      throw {:return, res}
    catch
      {:return, val} -> val
    end
  end
  def headTailOverlap(s1, s2) do
    try do
      start = 0
      while_fun_7 = fn while_fun_7, start ->
        if true do
          ix = Main.indexOfFrom(s1, _slice(s2, 0, 1 - 0), start)
          if ix == 0 - 1 do
            throw {:return, 0}
          end
          start = ix
          sublen = _len(s1) - start
          {sublen} = if sublen > _len(s2) do
            sublen = _len(s2)
            {sublen}
          else
            {sublen}
          end
          if _slice(s2, 0, sublen - (0)) == _slice(s1, start, start + sublen - (start)) do
            throw {:return, sublen}
          end
          start = start + 1
          while_fun_7.(while_fun_7, start)
        else
          start
        end
      end
      start = try do
          while_fun_7.(while_fun_7, start)
        catch
          :break -> start
        end

    catch
      {:return, val} -> val
    end
  end
  def deduplicate(slist) do
    try do
      arr = Main.distinct(slist)
      filtered = []
      i = 0
      while_fun_8 = fn while_fun_8, filtered, i ->
        if i < _len(arr) do
          s1 = Enum.at(arr, i)
          within = false
          j = 0
          while_fun_9 = fn while_fun_9, j, within ->
            if j < _len(arr) do
              {within} = if j != i && Main.containsStr(Enum.at(arr, j), s1) do
                within = true
                throw :break
                {within}
              else
                {within}
              end
              j = j + 1
              while_fun_9.(while_fun_9, j, within)
            else
              {j, within}
            end
          end
          {j, within} = try do
              while_fun_9.(while_fun_9, j, within)
            catch
              :break -> {j, within}
            end

          {filtered} = if !within do
            filtered = (filtered ++ [s1])
            {filtered}
          else
            {filtered}
          end
          i = i + 1
          while_fun_8.(while_fun_8, filtered, i)
        else
          {filtered, i}
        end
      end
      {filtered, i} = try do
          while_fun_8.(while_fun_8, filtered, i)
        catch
          :break -> {filtered, i}
        end

      throw {:return, filtered}
    catch
      {:return, val} -> val
    end
  end
  def joinAll(ss) do
    try do
      out = ""
      {out} = Enum.reduce(ss, {out}, fn s, {out} ->
        out = (out <> s)
        {out}
      end)
      throw {:return, out}
    catch
      {:return, val} -> val
    end
  end
  def shortestCommonSuperstring(slist) do
    try do
      ss = Main.deduplicate(slist)
      shortest = Main.joinAll(ss)
      perms = Main.permutations(ss)
      idx = 0
      while_fun_10 = fn while_fun_10, idx, shortest ->
        if idx < _len(perms) do
          perm = Enum.at(perms, idx)
          sup = Enum.at(perm, 0)
          i = 0
          while_fun_11 = fn while_fun_11, i, sup ->
            if i < _len(ss) - 1 do
              ov = Main.headTailOverlap(Enum.at(perm, i), Enum.at(perm, i + 1))
              sup = (sup <> _slice(Enum.at(perm, i + 1), ov, _len(Enum.at(perm, i + 1)) - (ov)))
              i = i + 1
              while_fun_11.(while_fun_11, i, sup)
            else
              {i, sup}
            end
          end
          {i, sup} = try do
              while_fun_11.(while_fun_11, i, sup)
            catch
              :break -> {i, sup}
            end

          {shortest} = if _len(sup) < _len(shortest) do
            shortest = sup
            {shortest}
          else
            {shortest}
          end
          idx = idx + 1
          while_fun_10.(while_fun_10, idx, shortest)
        else
          {idx, shortest}
        end
      end
      {idx, shortest} = try do
          while_fun_10.(while_fun_10, idx, shortest)
        catch
          :break -> {idx, shortest}
        end

      throw {:return, shortest}
    catch
      {:return, val} -> val
    end
  end
  def printCounts(seq) do
    try do
      a = 0
      c = 0
      g = 0
      t = 0
      i = 0
      while_fun_12 = fn while_fun_12, a, c, g, i, t ->
        if i < _len(seq) do
          ch = _slice(seq, i, i + 1 - (i))
          {a, c, g, t} = if ch == "A" do
            a = a + 1
            {a, c, g, t}
          else
            {c, g, t} = if ch == "C" do
              c = c + 1
              {c, g, t}
            else
              {g, t} = if ch == "G" do
                g = g + 1
                {g, t}
              else
                {t} = if ch == "T" do
                  t = t + 1
                  {t}
                else
                  {t}
                end
                {g, t}
              end
              {c, g, t}
            end
            {a, c, g, t}
          end
          i = i + 1
          while_fun_12.(while_fun_12, a, c, g, i, t)
        else
          {a, c, g, i, t}
        end
      end
      {a, c, g, i, t} = try do
          while_fun_12.(while_fun_12, a, c, g, i, t)
        catch
          :break -> {a, c, g, i, t}
        end

      total = _len(seq)
      IO.puts((("\nNucleotide counts for " <> seq) <> ":\n"))
      IO.puts(Kernel.inspect((Main.padLeft("A", 10) <> Main.padLeft(Kernel.to_string(a), 12))))
      IO.puts(Kernel.inspect((Main.padLeft("C", 10) <> Main.padLeft(Kernel.to_string(c), 12))))
      IO.puts(Kernel.inspect((Main.padLeft("G", 10) <> Main.padLeft(Kernel.to_string(g), 12))))
      IO.puts(Kernel.inspect((Main.padLeft("T", 10) <> Main.padLeft(Kernel.to_string(t), 12))))
      IO.puts(Kernel.inspect((Main.padLeft("Other", 10) <> Main.padLeft(Kernel.to_string(total - (a + c + g + t)), 12))))
      IO.puts("  ____________________")
      IO.puts(Kernel.inspect((Main.padLeft("Total length", 14) <> Main.padLeft(Kernel.to_string(total), 8))))
    catch
      {:return, val} -> val
    end
  end
  def main() do
    try do
      tests = [["TA", "AAG", "TA", "GAA", "TA"], ["CATTAGGG", "ATTAG", "GGG", "TA"], ["AAGAUGGA", "GGAGCGCAUC", "AUCGCAAUAAGGA"], ["ATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTAT", "GGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGT", "CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA", "TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "AACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT", "GCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTC", "CGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATTCTGCTTATAACACTATGTTCT", "TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATGCTCGTGC", "GATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATT", "TTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA", "TCTCTTAAACTCCTGCTAAATGCTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGA"]]
      Enum.each(tests, fn seqs ->
        scs = Main.shortestCommonSuperstring(seqs)
        Main.printCounts(scs)
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
