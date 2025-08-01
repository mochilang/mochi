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
  def absf(x) do
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
  def minf(a, b) do
    try do
      throw {:return, (if a < b, do: a, else: b)}
    catch
      {:return, val} -> val
    end
  end
  def max3(a, b, c) do
    try do
      m = a
      {m} = if b > m do
        m = b
        {m}
      else
        {m}
      end
      {m} = if c > m do
        m = c
        {m}
      else
        {m}
      end
      throw {:return, m}
    catch
      {:return, val} -> val
    end
  end
  def min3(a, b, c) do
    try do
      m = a
      {m} = if b < m do
        m = b
        {m}
      else
        {m}
      end
      {m} = if c < m do
        m = c
        {m}
      else
        {m}
      end
      throw {:return, m}
    catch
      {:return, val} -> val
    end
  end
  def subdivideQuadSpline(q, t) do
    try do
      s = 1.0 - t
      u = %{c0: q.c0, c1: 0.0, c2: 0.0}
      v = %{c0: 0.0, c1: 0.0, c2: q.c2}
      u = Map.put(u, :c1, s * q.c0 + t * q.c1)
      v = Map.put(v, :c1, s * q.c1 + t * q.c2)
      u = Map.put(u, :c2, s * u.c1 + t * v.c1)
      v = Map.put(v, :c0, u.c2)
      throw {:return, [u, v]}
    catch
      {:return, val} -> val
    end
  end
  def subdivideQuadCurve(q, t) do
    try do
      xs = Main.subdivideQuadSpline(q.x, t)
      ys = Main.subdivideQuadSpline(q.y, t)
      u = %{x: Enum.at(xs, 0), y: Enum.at(ys, 0)}
      v = %{x: Enum.at(xs, 1), y: Enum.at(ys, 1)}
      throw {:return, [u, v]}
    catch
      {:return, val} -> val
    end
  end
  def rectsOverlap(xa0, ya0, xa1, ya1, xb0, yb0, xb1, yb1) do
    try do
      throw {:return, xb0 <= xa1 && xa0 <= xb1 && yb0 <= ya1 && ya0 <= yb1}
    catch
      {:return, val} -> val
    end
  end
  def testIntersect(p, q, tol) do
    try do
      pxmin = Main.min3(p.x.c0, p.x.c1, p.x.c2)
      pymin = Main.min3(p.y.c0, p.y.c1, p.y.c2)
      pxmax = Main.max3(p.x.c0, p.x.c1, p.x.c2)
      pymax = Main.max3(p.y.c0, p.y.c1, p.y.c2)
      qxmin = Main.min3(q.x.c0, q.x.c1, q.x.c2)
      qymin = Main.min3(q.y.c0, q.y.c1, q.y.c2)
      qxmax = Main.max3(q.x.c0, q.x.c1, q.x.c2)
      qymax = Main.max3(q.y.c0, q.y.c1, q.y.c2)
      exclude = true
      accept = false
      inter = %{x: 0.0, y: 0.0}
      {accept, exclude, inter} = if Main.rectsOverlap(pxmin, pymin, pxmax, pymax, qxmin, qymin, qxmax, qymax) do
        exclude = false
        xmin = Main.maxf(pxmin, qxmin)
        xmax = Main.minf(pxmax, qxmax)
        {accept, inter} = if xmax - xmin <= tol do
          ymin = Main.maxf(pymin, qymin)
          ymax = Main.minf(pymax, qymax)
          {accept, inter} = if ymax - ymin <= tol do
            accept = true
            inter = Map.put(inter, :x, 0.5 * (xmin + xmax))
            inter = Map.put(inter, :y, 0.5 * (ymin + ymax))
            {accept, inter}
          else
            {accept, inter}
          end
          {accept, inter}
        else
          {accept, inter}
        end
        {accept, exclude, inter}
      else
        {accept, exclude, inter}
      end
      throw {:return, %{"exclude" => exclude, "accept" => accept, "intersect" => inter}}
    catch
      {:return, val} -> val
    end
  end
  def seemsToBeDuplicate(pts, xy, spacing) do
    try do
      i = 0
      while_fun = fn while_fun, i ->
        if i < length(pts) do
          pt = Enum.at(pts, i)
          if Main.absf(pt.x - xy.x) < spacing && Main.absf(pt.y - xy.y) < spacing do
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
          :break -> i
        end

      throw {:return, false}
    catch
      {:return, val} -> val
    end
  end
  def findIntersects(p, q, tol, spacing) do
    try do
      inters = []
      workload = [%{"p" => p, "q" => q}]
      while_fun_2 = fn while_fun_2, inters, workload ->
        if length(workload) > 0 do
          idx = length(workload) - 1
          work = Enum.at(workload, idx)
          workload = _slice(workload, 0, idx - 0)
          res = Main.testIntersect(work["p"], work["q"], tol)
          excl = res["exclude"]
          acc = res["accept"]
          inter = res["intersect"]
          {inters, workload} = if acc do
            {inters} = if !Main.seemsToBeDuplicate(inters, inter, spacing) do
              inters = (inters ++ [inter])
              {inters}
            else
              {inters}
            end
            {inters, workload}
          else
            {workload} = if !excl do
              ps = Main.subdivideQuadCurve(work["p"], 0.5)
              qs = Main.subdivideQuadCurve(work["q"], 0.5)
              p0 = Enum.at(ps, 0)
              p1 = Enum.at(ps, 1)
              q0 = Enum.at(qs, 0)
              q1 = Enum.at(qs, 1)
              workload = (workload ++ [%{"p" => p0, "q" => q0}])
              workload = (workload ++ [%{"p" => p0, "q" => q1}])
              workload = (workload ++ [%{"p" => p1, "q" => q0}])
              workload = (workload ++ [%{"p" => p1, "q" => q1}])
              {workload}
            else
              {workload}
            end
            {inters, workload}
          end
          while_fun_2.(while_fun_2, inters, workload)
        else
          {inters, workload}
        end
      end
      {inters, workload} = try do
          while_fun_2.(while_fun_2, inters, workload)
        catch
          :break -> {inters, workload}
        end

      throw {:return, inters}
    catch
      {:return, val} -> val
    end
  end
  def main() do
    try do
      p = %{x: %{c0: -1.0, c1: 0.0, c2: 1.0}, y: %{c0: 0.0, c1: 10.0, c2: 0.0}}
      q = %{x: %{c0: 2.0, c1: -8.0, c2: 2.0}, y: %{c0: 1.0, c1: 2.0, c2: 3.0}}
      tol = 0.0000001
      spacing = tol * 10.0
      inters = Main.findIntersects(p, q, tol, spacing)
      i = 0
      while_fun_3 = fn while_fun_3, i ->
        if i < length(inters) do
          pt = Enum.at(inters, i)
          IO.puts((((("(" <> Kernel.inspect(pt.x)) <> ", ") <> Kernel.inspect(pt.y)) <> ")"))
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
