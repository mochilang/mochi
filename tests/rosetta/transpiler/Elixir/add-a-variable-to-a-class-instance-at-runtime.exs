# Code generated by Mochi transpiler 2025-07-25 17:35 +0700
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
  def main() do
    try do
      ss = %{runtimeFields: %{}}
      IO.puts("Create two fields at runtime: \n")
      i = 1
      while_fun = fn while_fun, i, ss ->
        if i <= 2 do
          IO.puts((("  Field #" <> to_string(i)) <> ":\n"))
          IO.puts("       Enter name  : ")
          name = case IO.gets("") do
  nil -> ""
  :eof -> ""
  line -> String.trim(line)
end
          IO.puts("       Enter value : ")
          value = case IO.gets("") do
  nil -> ""
  :eof -> ""
  line -> String.trim(line)
end
          fields = ss.runtimeFields
          fields = List.replace_at(fields, name, value)
          ss = Map.put(ss, :runtimeFields, fields)
          IO.puts("\n")
          i = i + 1
          while_fun.(while_fun, i, ss)
        else
          {i, ss}
        end
      end
      {i, ss} = try do
          while_fun.(while_fun, i, ss)
        catch
          :break -> {i, ss}
        end

      while_fun_2 = fn while_fun_2 ->
        if true do
          IO.puts("Which field do you want to inspect ? ")
          name = case IO.gets("") do
  nil -> ""
  :eof -> ""
  line -> String.trim(line)
end
          if name in ss.runtimeFields do
            value = ss.runtimeFields[name]
            IO.puts((("Its value is '" <> value) <> "'"))
            throw {:return, nil}
          else
            IO.puts("There is no field of that name, try again\n")
          end
          while_fun_2.(while_fun_2)
        else
          nil
        end
      end
      try do
        while_fun_2.(while_fun_2)
      catch
        :break -> nil
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
