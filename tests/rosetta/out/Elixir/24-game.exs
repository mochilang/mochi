# Generated by Mochi compiler v0.10.30 on 2025-07-19T01:02:23Z
defmodule Main do
  @spec randDigit() :: integer()
  def randDigit() do
    try do
      throw({:return, rem(_now(), 9) + 1})
    catch
      {:return, v} -> v
    end
  end

  @spec main() :: nil
  def main() do
    try do
      # digits :: list(any())
      digits = []
      _ = digits

      {digits} =
        Enum.reduce(0..(4 - 1), {digits}, fn i, {digits} ->
          digits = digits ++ [randDigit()]
          {digits}
        end)

      _ = digits
      # numstr :: String.t()
      numstr = ""
      _ = numstr

      {numstr} =
        Enum.reduce(0..(4 - 1), {numstr}, fn i, {numstr} ->
          numstr = numstr <> to_string(Enum.at(digits, i))
          {numstr}
        end)

      _ = numstr
      IO.puts(("Your numbers: " <> numstr) <> "\n")
      IO.puts("Enter RPN: ")
      # expr :: String.t()
      expr = _input()
      _ = expr

      if String.length(expr) != 7 do
        IO.puts("invalid. expression length must be 7. (4 numbers, 3 operators, no spaces)")
        throw({:return, nil})
      end

      # stack :: list(any())
      stack = []
      _ = stack
      # i :: integer()
      i = 0
      _ = i
      # valid :: boolean()
      valid = true
      _ = valid

      t1 = fn t1, digits, i, stack, valid ->
        try do
          if i < String.length(expr) do
            ch = String.slice(expr, i, i + 1 - i)

            if ch >= "0" && ch <= "9" do
              if length(digits) == 0 do
                IO.puts("too many numbers.")
                throw({:return, nil})
              end

              # j :: integer()
              j = 0
              _ = j

              t2 = fn t2, j ->
                try do
                  if Enum.at(digits, j) != _int(ch) - _int("0") do
                    j = j + 1

                    if j == length(digits) do
                      IO.puts("wrong numbers.")
                      throw({:return, nil})
                    end

                    t2.(t2, j)
                  else
                    {:ok, j}
                  end
                catch
                  :break ->
                    {:ok, j}
                end
              end

              {_, j} = t2.(t2, j)
              _ = j

              digits =
                Enum.slice(digits, 0, j - 0) +
                  Enum.slice(digits, j + 1, length(String.graphemes(digits)) - (j + 1))

              stack = stack ++ [:erlang.float(_int(ch) - _int("0"))]
            else
              if length(stack) < 2 do
                IO.puts("invalid expression syntax.")
                valid = false
                throw(:break)
              end

              # b :: any()
              b = Enum.at(stack, length(stack) - 1)
              _ = b
              # a :: any()
              a = Enum.at(stack, length(stack) - 2)
              _ = a

              if ch == "+" do
                stack = List.replace_at(stack, length(stack) - 2, a + b)
              else
                if ch == "-" do
                  stack = List.replace_at(stack, length(stack) - 2, a - b)
                else
                  if ch == "*" do
                    stack = List.replace_at(stack, length(stack) - 2, a * b)
                  else
                    if ch == "/" do
                      stack = List.replace_at(stack, length(stack) - 2, a / b)
                    else
                      IO.inspect(ch <> " invalid.")
                      valid = false
                      throw(:break)
                    end
                  end
                end
              end

              stack = Enum.slice(stack, 0, length(stack) - 1 - 0)
            end

            i = i + 1
            t1.(t1, digits, i, stack, valid)
          else
            {:ok, digits, i, stack, valid}
          end
        catch
          :break ->
            {:ok, digits, i, stack, valid}
        end
      end

      {_, digits, i, stack, valid} = t1.(t1, digits, i, stack, valid)
      _ = digits
      _ = i
      _ = stack
      _ = valid

      if valid do
        if abs(Enum.at(stack, 0) - 24) > 0.000001 do
          IO.puts(("incorrect. " <> to_string(Enum.at(stack, 0))) <> " != 24")
        else
          IO.puts("correct.")
        end
      end
    catch
      {:return, v} -> v
    end
  end

  def main do
    main()
  end

  defp _input() do
    case IO.gets("") do
      :eof -> ""
      other -> String.trim(other)
    end
  end

  defp _int(v) do
    cond do
      is_integer(v) ->
        v

      is_float(v) ->
        trunc(v)

      is_binary(v) ->
        case Integer.parse(String.trim(v)) do
          {i, ""} -> i
          _ -> raise "invalid int"
        end

      true ->
        raise "invalid int"
    end
  end

  defp _now() do
    System.os_time(:millisecond)
  end
end

Main.main()
