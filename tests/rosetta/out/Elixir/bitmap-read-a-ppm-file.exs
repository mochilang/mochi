# Generated by Mochi compiler v0.10.26 on 2025-07-16T12:46:45Z
defmodule Pixel do
  @type t :: %__MODULE__{R: integer(), G: integer(), B: integer()}
  defstruct R: nil, G: nil, B: nil
end

defmodule Bitmap do
  @type t :: %__MODULE__{w: integer(), h: integer(), max: integer(), data: list(list(Pixel))}
  defstruct w: nil, h: nil, max: nil, data: nil
end

defmodule Main do
  @spec newBitmap(integer(), integer(), integer()) :: Bitmap
  def newBitmap(w, h, max) do
    try do
      rows = []
      _ = rows
      y = 0
      _ = y

      t1 = fn t1, rows, y ->
        try do
          if y < h do
            row = []
            _ = row
            x = 0
            _ = x

            t2 = fn t2, row, x ->
              try do
                if x < w do
                  row = row ++ [%Pixel{R: 0, G: 0, B: 0}]
                  x = x + 1
                  t2.(t2, row, x)
                else
                  {:ok, row, x}
                end
              catch
                :break ->
                  {:ok, row, x}
              end
            end

            {_, row, x} = t2.(t2, row, x)
            _ = row
            _ = x
            rows = rows ++ [row]
            y = y + 1
            t1.(t1, rows, y)
          else
            {:ok, rows, y}
          end
        catch
          :break ->
            {:ok, rows, y}
        end
      end

      {_, rows, y} = t1.(t1, rows, y)
      _ = rows
      _ = y
      throw({:return, %Bitmap{w: w, h: h, max: max, data: rows}})
    catch
      {:return, v} -> v
    end
  end

  @spec setPx(Bitmap, integer(), integer(), Pixel) :: nil
  def setPx(b, x, y, p) do
    try do
      rows = b.data
      _ = rows
      row = Enum.at(rows, y)
      _ = row
      row = Map.put(row, x, p)
      rows = Map.put(rows, y, row)
      b = %{b | data: rows}
    catch
      {:return, v} -> v
    end
  end

  @spec getPx(Bitmap, integer(), integer()) :: Pixel
  def getPx(b, x, y) do
    try do
      throw({:return, Enum.at(Enum.at(b.data, y), x)})
    catch
      {:return, v} -> v
    end
  end

  @spec splitLines(String.t()) :: list(String.t())
  def splitLines(s) do
    try do
      # out :: String.t()
      out = []
      _ = out
      cur = ""
      _ = cur
      i = 0
      _ = i

      t3 = fn t3, cur, i, out ->
        try do
          if i < length(s) do
            ch = _slice_string(s, i, i + 1)

            if ch == "\n" do
              out = out ++ [cur]
              cur = ""
            else
              cur = cur + ch
            end

            i = i + 1
            t3.(t3, cur, i, out)
          else
            {:ok, cur, i, out}
          end
        catch
          :break ->
            {:ok, cur, i, out}
        end
      end

      {_, cur, i, out} = t3.(t3, cur, i, out)
      _ = cur
      _ = i
      _ = out
      out = out ++ [cur]
      throw({:return, out})
    catch
      {:return, v} -> v
    end
  end

  @spec splitWS(String.t()) :: list(String.t())
  def splitWS(s) do
    try do
      # out :: String.t()
      out = []
      _ = out
      cur = ""
      _ = cur
      i = 0
      _ = i

      t4 = fn t4, cur, i, out ->
        try do
          if i < length(s) do
            ch = _slice_string(s, i, i + 1)

            if ch == " " || ch == "\t" || ch == "\r" || ch == "\n" do
              if length(cur) > 0 do
                out = out ++ [cur]
                cur = ""
              end
            else
              cur = cur + ch
            end

            i = i + 1
            t4.(t4, cur, i, out)
          else
            {:ok, cur, i, out}
          end
        catch
          :break ->
            {:ok, cur, i, out}
        end
      end

      {_, cur, i, out} = t4.(t4, cur, i, out)
      _ = cur
      _ = i
      _ = out

      if length(cur) > 0 do
        out = out ++ [cur]
      end

      throw({:return, out})
    catch
      {:return, v} -> v
    end
  end

  @spec parseIntStr(String.t()) :: integer()
  def parseIntStr(str) do
    try do
      i = 0
      _ = i
      neg = false
      _ = neg

      if length(str) > 0 && Enum.slice(str, 0, 1 - 0) == "-" do
        neg = true
        i = 1
      end

      n = 0
      _ = n

      digits = %{
        "0" => 0,
        "1" => 1,
        "2" => 2,
        "3" => 3,
        "4" => 4,
        "5" => 5,
        "6" => 6,
        "7" => 7,
        "8" => 8,
        "9" => 9
      }

      t5 = fn t5, i, n ->
        try do
          if i < length(str) do
            n = n * 10 + Enum.at(digits, Enum.slice(str, i, i + 1 - i))
            i = i + 1
            t5.(t5, i, n)
          else
            {:ok, i, n}
          end
        catch
          :break ->
            {:ok, i, n}
        end
      end

      {_, i, n} = t5.(t5, i, n)
      _ = i
      _ = n

      if neg do
        n = -n
      end

      throw({:return, n})
    catch
      {:return, v} -> v
    end
  end

  @spec tokenize(String.t()) :: list(String.t())
  def tokenize(s) do
    try do
      lines = splitLines(s)
      toks = []
      _ = toks
      i = 0
      _ = i

      t6 = fn t6, i, toks ->
        try do
          if i < length(lines) do
            line = Enum.at(lines, i)

            if length(line) > 0 && _slice_string(line, 0, 1) == "#" do
              i = i + 1
              throw(:continue)
            end

            parts = splitWS(line)
            j = 0
            _ = j

            t7 = fn t7, j, toks ->
              try do
                if j < length(parts) do
                  toks = toks ++ [Enum.at(parts, j)]
                  j = j + 1
                  t7.(t7, j, toks)
                else
                  {:ok, j, toks}
                end
              catch
                :break ->
                  {:ok, j, toks}
              end
            end

            {_, j, toks} = t7.(t7, j, toks)
            _ = j
            _ = toks
            i = i + 1
            t6.(t6, i, toks)
          else
            {:ok, i, toks}
          end
        catch
          :break ->
            {:ok, i, toks}
        end
      end

      {_, i, toks} = t6.(t6, i, toks)
      _ = i
      _ = toks
      throw({:return, toks})
    catch
      {:return, v} -> v
    end
  end

  @spec readP3(String.t()) :: Bitmap
  def readP3(text) do
    try do
      toks = tokenize(text)

      if length(toks) < 4 do
        throw({:return, newBitmap(0, 0, 0)})
      end

      if Enum.at(toks, 0) != "P3" do
        throw({:return, newBitmap(0, 0, 0)})
      end

      w = parseIntStr(Enum.at(toks, 1))
      h = parseIntStr(Enum.at(toks, 2))
      maxv = parseIntStr(Enum.at(toks, 3))
      idx = 4
      _ = idx
      # bm :: Bitmap
      bm = _structify(Bitmap, newBitmap(w, h, maxv))
      _ = bm
      y = h - 1
      _ = y

      t8 = fn t8, idx, y ->
        try do
          if y >= 0 do
            x = 0
            _ = x

            t9 = fn t9, idx, x ->
              try do
                if x < w do
                  r = parseIntStr(Enum.at(toks, idx))
                  g = parseIntStr(Enum.at(toks, idx + 1))
                  b = parseIntStr(Enum.at(toks, idx + 2))
                  setPx(bm, x, y, %Pixel{R: r, G: g, B: b})
                  idx = idx + 3
                  x = x + 1
                  t9.(t9, idx, x)
                else
                  {:ok, idx, x}
                end
              catch
                :break ->
                  {:ok, idx, x}
              end
            end

            {_, idx, x} = t9.(t9, idx, x)
            _ = idx
            _ = x
            y = y - 1
            t8.(t8, idx, y)
          else
            {:ok, idx, y}
          end
        catch
          :break ->
            {:ok, idx, y}
        end
      end

      {_, idx, y} = t8.(t8, idx, y)
      _ = idx
      _ = y
      throw({:return, bm})
    catch
      {:return, v} -> v
    end
  end

  @spec toGrey(Bitmap) :: nil
  def toGrey(b) do
    try do
      h = b.h
      w = b.w
      m = 0
      _ = m
      y = 0
      _ = y

      t10 = fn t10, m, y ->
        try do
          if y < h do
            x = 0
            _ = x

            t11 = fn t11, m, x ->
              try do
                if x < w do
                  p = getPx(b, x, y)
                  l = (p.R * 2126 + p.G * 7152 + p.B * 722) / 10000
                  _ = l

                  if l > b.max do
                    l = b.max
                  end

                  setPx(b, x, y, %Pixel{R: l, G: l, B: l})

                  if l > m do
                    m = l
                  end

                  x = x + 1
                  t11.(t11, m, x)
                else
                  {:ok, m, x}
                end
              catch
                :break ->
                  {:ok, m, x}
              end
            end

            {_, m, x} = t11.(t11, m, x)
            _ = m
            _ = x
            y = y + 1
            t10.(t10, m, y)
          else
            {:ok, m, y}
          end
        catch
          :break ->
            {:ok, m, y}
        end
      end

      {_, m, y} = t10.(t10, m, y)
      _ = m
      _ = y
      b = %{b | max: m}
    catch
      {:return, v} -> v
    end
  end

  @spec pad(integer(), integer()) :: String.t()
  def pad(n, w) do
    try do
      s = to_string(n)
      _ = s

      t12 = fn t12, s ->
        try do
          if length(s) < w do
            s = " " <> s
            t12.(t12, s)
          else
            {:ok, s}
          end
        catch
          :break ->
            {:ok, s}
        end
      end

      {_, s} = t12.(t12, s)
      _ = s
      throw({:return, s})
    catch
      {:return, v} -> v
    end
  end

  @spec writeP3(Bitmap) :: String.t()
  def writeP3(b) do
    try do
      h = b.h
      w = b.w
      # max :: (any() -> any())
      max = b.max
      _ = max
      digits = String.length(to_string(max))
      # out :: String.t()
      out =
        ((((("P3\n# generated from Bitmap.writeppmp3\n" <> to_string(w)) <> " ") <> to_string(h)) <>
            "\n") <> to_string(max)) <> "\n"

      _ = out
      y = h - 1
      _ = y

      t13 = fn t13, out, y ->
        try do
          if y >= 0 do
            line = ""
            _ = line
            x = 0
            _ = x

            t14 = fn t14, line, x ->
              try do
                if x < w do
                  p = getPx(b, x, y)

                  line =
                    (((((line <> "   ") <> pad(p.R, digits)) <> " ") <> pad(p.G, digits)) <> " ") <>
                      pad(p.B, digits)

                  x = x + 1
                  t14.(t14, line, x)
                else
                  {:ok, line, x}
                end
              catch
                :break ->
                  {:ok, line, x}
              end
            end

            {_, line, x} = t14.(t14, line, x)
            _ = line
            _ = x
            out = (out <> line) <> "\n"
            y = y - 1
            t13.(t13, out, y)
          else
            {:ok, out, y}
          end
        catch
          :break ->
            {:ok, out, y}
        end
      end

      {_, out, y} = t13.(t13, out, y)
      _ = out
      _ = y
      throw({:return, out})
    catch
      {:return, v} -> v
    end
  end

  def main do
    # ppmtxt :: String.t()
    ppmtxt =
      (((((("P3\n" <> "# feep.ppm\n") <> "4 4\n") <> "15\n") <>
           " 0  0  0    0  0  0    0  0  0   15  0 15\n") <>
          " 0  0  0    0 15  7    0  0  0    0  0  0\n") <>
         " 0  0  0    0  0  0    0 15  7    0  0  0\n") <>
        "15  0 15    0  0  0    0  0  0    0  0  0\n"

    _ = ppmtxt
    IO.puts("Original Colour PPM file")
    IO.puts(ppmtxt)
    # bm :: Bitmap
    bm = _structify(Bitmap, readP3(ppmtxt))
    _ = bm
    IO.puts("Grey PPM:")
    toGrey(bm)
    # out :: String.t()
    out = writeP3(bm)
    IO.puts(out)
  end

  defp _slice_string(s, i, j) do
    chars = String.graphemes(s)
    n = length(chars)
    start = if i < 0, do: i + n, else: i
    finish = if j < 0, do: j + n, else: j
    start = if start < 0, do: 0, else: start
    finish = if finish > n, do: n, else: finish
    finish = if finish < start, do: start, else: finish
    Enum.slice(chars, start, finish - start) |> Enum.join()
  end

  defp _structify(mod, v) do
    cond do
      is_struct(v) ->
        v

      is_map(v) ->
        m =
          Enum.reduce(v, %{}, fn {k, val}, acc ->
            Map.put(acc, String.to_atom(to_string(k)), _structify(nil, val))
          end)

        if mod, do: struct(mod, m), else: m

      is_list(v) ->
        Enum.map(v, &_structify(nil, &1))

      true ->
        v
    end
  end
end

Main.main()
