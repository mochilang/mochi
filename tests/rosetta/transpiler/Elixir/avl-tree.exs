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
  def node_(data) do
    try do
      throw {:return, %{"Data" => data, "Balance" => 0, "Link" => [nil, nil]}}
    catch
      {:return, val} -> val
    end
  end
  def getLink(n, dir) do
    try do
      throw {:return, Enum.at((n["Link"]), dir)}
    catch
      {:return, val} -> val
    end
  end
  def setLink(n, dir, v) do
    try do
      links = n["Link"]
      links = List.replace_at(links, dir, v)
      n = Map.put(n, "Link", links)
    catch
      {:return, val} -> val
    end
  end
  def opp(dir) do
    try do
      throw {:return, 1 - dir}
    catch
      {:return, val} -> val
    end
  end
  def single(root, dir) do
    try do
      tmp = Main.getLink(root, Main.opp(dir))
      Main.setLink(root, Main.opp(dir), Main.getLink(tmp, dir))
      Main.setLink(tmp, dir, root)
      throw {:return, tmp}
    catch
      {:return, val} -> val
    end
  end
  def double(root, dir) do
    try do
      tmp = Main.getLink(Main.getLink(root, Main.opp(dir)), dir)
      Main.setLink(Main.getLink(root, Main.opp(dir)), dir, Main.getLink(tmp, Main.opp(dir)))
      Main.setLink(tmp, Main.opp(dir), Main.getLink(root, Main.opp(dir)))
      Main.setLink(root, Main.opp(dir), tmp)
      tmp = Main.getLink(root, Main.opp(dir))
      Main.setLink(root, Main.opp(dir), Main.getLink(tmp, dir))
      Main.setLink(tmp, dir, root)
      throw {:return, tmp}
    catch
      {:return, val} -> val
    end
  end
  def adjustBalance(root, dir, bal) do
    try do
      n = Main.getLink(root, dir)
      nn = Main.getLink(n, Main.opp(dir))
      {n, root} = if nn["Balance"] == 0 do
        root = Map.put(root, "Balance", 0)
        n = List.replace_at(n, "Balance", 0)
        {n, root}
      else
        {n, root} = if nn["Balance"] == bal do
          root = Map.put(root, "Balance", -bal)
          n = List.replace_at(n, "Balance", 0)
          {n, root}
        else
          root = Map.put(root, "Balance", 0)
          n = List.replace_at(n, "Balance", bal)
          {n, root}
        end
        {n, root}
      end
      nn = List.replace_at(nn, "Balance", 0)
    catch
      {:return, val} -> val
    end
  end
  def insertBalance(root, dir) do
    try do
      n = Main.getLink(root, dir)
      bal = 2 * dir - 1
      {n, root} = if n["Balance"] == bal do
        root = Map.put(root, "Balance", 0)
        n = List.replace_at(n, "Balance", 0)
        throw {:return, Main.single(root, Main.opp(dir))}
        {n, root}
      else
        {n, root}
      end
      Main.adjustBalance(root, dir, bal)
      throw {:return, Main.double(root, Main.opp(dir))}
    catch
      {:return, val} -> val
    end
  end
  def insertR(root, data) do
    try do
      if root == nil do
        throw {:return, %{"node" => node_(data), "done" => false}}
      end
      node_ = root
      dir = 0
      {dir} = if (trunc(node_["Data"])) < data do
        dir = 1
        {dir}
      else
        {dir}
      end
      r = Main.insertR(Main.getLink(node_, dir), data)
      Main.setLink(node_, dir, r["node"])
      if r["done"] do
        throw {:return, %{"node" => node_, "done" => true}}
      end
      node_ = List.replace_at(node_, "Balance", (trunc(node_["Balance"])) + (2 * dir - 1))
      if node_["Balance"] == 0 do
        throw {:return, %{"node" => node_, "done" => true}}
      end
      if node_["Balance"] == 1 || node_["Balance"] == (-1) do
        throw {:return, %{"node" => node_, "done" => false}}
      end
      throw {:return, %{"node" => Main.insertBalance(node_, dir), "done" => true}}
    catch
      {:return, val} -> val
    end
  end
  def insert(tree, data) do
    try do
      r = Main.insertR(tree, data)
      throw {:return, r["node"]}
    catch
      {:return, val} -> val
    end
  end
  def removeBalance(root, dir) do
    try do
      n = Main.getLink(root, Main.opp(dir))
      bal = 2 * dir - 1
      {n, root} = if n["Balance"] == (-bal) do
        root = Map.put(root, "Balance", 0)
        n = List.replace_at(n, "Balance", 0)
        throw {:return, %{"node" => Main.single(root, dir), "done" => false}}
        {n, root}
      else
        {n, root}
      end
      if n["Balance"] == bal do
        Main.adjustBalance(root, Main.opp(dir), (-bal))
        throw {:return, %{"node" => Main.double(root, dir), "done" => false}}
      end
      root = Map.put(root, "Balance", -bal)
      n = List.replace_at(n, "Balance", bal)
      throw {:return, %{"node" => Main.single(root, dir), "done" => true}}
    catch
      {:return, val} -> val
    end
  end
  def removeR(root, data) do
    try do
      if root == nil do
        throw {:return, %{"node" => nil, "done" => false}}
      end
      node_ = root
      {data, node_} = if (trunc(node_["Data"])) == data do
        if Main.getLink(node_, 0) == nil do
          throw {:return, %{"node" => Main.getLink(node_, 1), "done" => false}}
        end
        if Main.getLink(node_, 1) == nil do
          throw {:return, %{"node" => Main.getLink(node_, 0), "done" => false}}
        end
        heir = Main.getLink(node_, 0)
        while_fun = fn while_fun, heir ->
          if Main.getLink(heir, 1) != nil do
            heir = Main.getLink(heir, 1)
            while_fun.(while_fun, heir)
          else
            heir
          end
        end
        heir = try do
            while_fun.(while_fun, heir)
          catch
            :break -> heir
          end

        node_ = List.replace_at(node_, "Data", heir["Data"])
        data = trunc(heir["Data"])
        {data, node_}
      else
        {data, node_}
      end
      dir = 0
      {dir} = if (trunc(node_["Data"])) < data do
        dir = 1
        {dir}
      else
        {dir}
      end
      r = Main.removeR(Main.getLink(node_, dir), data)
      Main.setLink(node_, dir, r["node"])
      if r["done"] do
        throw {:return, %{"node" => node_, "done" => true}}
      end
      node_ = List.replace_at(node_, "Balance", (trunc(node_["Balance"])) + 1 - 2 * dir)
      if node_["Balance"] == 1 || node_["Balance"] == (-1) do
        throw {:return, %{"node" => node_, "done" => true}}
      end
      if node_["Balance"] == 0 do
        throw {:return, %{"node" => node_, "done" => false}}
      end
      throw {:return, Main.removeBalance(node_, dir)}
    catch
      {:return, val} -> val
    end
  end
  def remove(tree, data) do
    try do
      r = Main.removeR(tree, data)
      throw {:return, r["node"]}
    catch
      {:return, val} -> val
    end
  end
  def indentStr(n) do
    try do
      s = ""
      i = 0
      while_fun_2 = fn while_fun_2, i, s ->
        if i < n do
          s = (s <> " ")
          i = i + 1
          while_fun_2.(while_fun_2, i, s)
        else
          {i, s}
        end
      end
      {i, s} = try do
          while_fun_2.(while_fun_2, i, s)
        catch
          :break -> {i, s}
        end

      throw {:return, s}
    catch
      {:return, val} -> val
    end
  end
  def dumpNode(node_, indent, comma) do
    try do
      sp = Main.indentStr(indent)
      if node_ == nil do
        line = (sp <> "null")
        {line} = if comma do
          line = (line <> ",")
          {line}
        else
          {line}
        end
        IO.puts(Kernel.inspect(line))
      else
        IO.puts(Kernel.inspect((sp <> "{")))
        IO.puts(Kernel.inspect((((Main.indentStr(indent + 3) <> "\"Data\": ") <> Kernel.inspect(node_["Data"])) <> ",")))
        IO.puts(Kernel.inspect((((Main.indentStr(indent + 3) <> "\"Balance\": ") <> Kernel.inspect(node_["Balance"])) <> ",")))
        IO.puts(Kernel.inspect((Main.indentStr(indent + 3) <> "\"Link\": [")))
        Main.dumpNode(Main.getLink(node_, 0), indent + 6, true)
        Main.dumpNode(Main.getLink(node_, 1), indent + 6, false)
        IO.puts(Kernel.inspect((Main.indentStr(indent + 3) <> "]")))
        end_ = (sp <> "}")
        {end_} = if comma do
          end_ = (end_ <> ",")
          {end_}
        else
          {end_}
        end
        IO.puts(Kernel.inspect(end_))
      end
    catch
      {:return, val} -> val
    end
  end
  def dump(node_, indent) do
    try do
      Main.dumpNode(node_, indent, false)
    catch
      {:return, val} -> val
    end
  end
  def main() do
    try do
      tree = nil
      IO.puts("Empty tree:")
      Main.dump(tree, 0)
      IO.puts("")
      IO.puts("Insert test:")
      tree = insert(tree, 3)
      tree = insert(tree, 1)
      tree = insert(tree, 4)
      tree = insert(tree, 1)
      tree = insert(tree, 5)
      Main.dump(tree, 0)
      IO.puts("")
      IO.puts("Remove test:")
      tree = remove(tree, 3)
      tree = remove(tree, 1)
      t = tree
      t = List.replace_at(t, "Balance", 0)
      tree = t
      Main.dump(tree, 0)
    catch
      {:return, val} -> val
    end
  end
end
Main.main()
