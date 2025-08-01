# Generated by Mochi compiler v0.10.26 on 2025-07-16T12:46:28Z
defmodule Main do
  @spec padLeft(String.t(), integer()) :: String.t()
  def padLeft(s, w) do
    try do
      res = ""
      _ = res
      n = w - length(s)
      _ = n

      t1 = fn t1, n, res ->
        try do
          if n > 0 do
            res = res <> " "
            n = n - 1
            t1.(t1, n, res)
          else
            {:ok, n, res}
          end
        catch
          :break ->
            {:ok, n, res}
        end
      end

      {_, n, res} = t1.(t1, n, res)
      _ = n
      _ = res
      throw({:return, res + s})
    catch
      {:return, v} -> v
    end
  end

  def main do
    # dna :: String.t()
    dna =
      ((((((((("" <> "CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATG") <>
                "CTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTG") <>
               "AGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGAT") <>
              "GGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT") <>
             "CGATTCTGCTTATAACACTATGTTCTTATGAAATGGATGTTCTGAGTTGG") <>
            "TCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA") <>
           "TTTAATTTTTCTATATAGCGATCTGTATTTAAGCAATTCATTTAGGTTAT") <>
          "CGCCGCGATGCTCGGTTCGGACCGCCAAGCATCTGGCTCCACTGCTAGTG") <>
         "TCCTAAATTTGAATGGCAAACACAAATAAGATTTAGCAATTCGTGTAGAC") <>
        "GACCGGGGACTTGCATGATGGGAGCAGCTTTGTTAAACTACGAACGTAAT"

    IO.puts("SEQUENCE:")
    # le :: integer()
    le = String.length(dna)
    # i :: integer()
    i = 0
    _ = i

    t2 = fn t2, i ->
      try do
        if i < le do
          k = i + 50
          _ = k

          if k > le do
            k = le
          end

          IO.puts((padLeft(to_string(i), 5) <> ": ") <> _slice_string(dna, i, k))
          i = i + 50
          t2.(t2, i)
        else
          {:ok, i}
        end
      catch
        :break ->
          {:ok, i}
      end
    end

    {_, i} = t2.(t2, i)
    _ = i
    # a :: integer()
    a = 0
    _ = a
    # c :: integer()
    c = 0
    _ = c
    # g :: integer()
    g = 0
    _ = g
    # t :: integer()
    t = 0
    _ = t
    # idx :: integer()
    idx = 0
    _ = idx

    t3 = fn t3, a, c, g, idx, t ->
      try do
        if idx < le do
          ch = _slice_string(dna, idx, idx + 1)

          if ch == "A" do
            a = a + 1
          else
            if ch == "C" do
              c = c + 1
            else
              if ch == "G" do
                g = g + 1
              else
                if ch == "T" do
                  t = t + 1
                end
              end
            end
          end

          idx = idx + 1
          t3.(t3, a, c, g, idx, t)
        else
          {:ok, a, c, g, idx, t}
        end
      catch
        :break ->
          {:ok, a, c, g, idx, t}
      end
    end

    {_, a, c, g, idx, t} = t3.(t3, a, c, g, idx, t)
    _ = a
    _ = c
    _ = g
    _ = idx
    _ = t
    IO.puts("")
    IO.puts("BASE COUNT:")
    IO.puts("    A: " <> padLeft(to_string(a), 3))
    IO.puts("    C: " <> padLeft(to_string(c), 3))
    IO.puts("    G: " <> padLeft(to_string(g), 3))
    IO.puts("    T: " <> padLeft(to_string(t), 3))
    IO.puts("    ------")
    IO.puts("    Σ: " <> to_string(le))
    IO.puts("    ======")
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
end

Main.main()
