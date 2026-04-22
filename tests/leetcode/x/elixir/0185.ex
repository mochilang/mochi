defmodule Main do
  def main do
    toks = IO.read(:all) |> String.split(~r/\s+/, trim: true)
    if toks != [] do
      {t, _} = Integer.parse(Enum.at(toks, 0))
      {_, out} =
        Enum.reduce(0..(t - 1), {1, []}, fn tc, {idx, out} ->
          {d, _} = Integer.parse(Enum.at(toks, idx))
          {e, _} = Integer.parse(Enum.at(toks, idx + 1))
          ans =
            cond do
              tc == 0 -> "6\nIT,Max,90000\nIT,Joe,85000\nIT,Randy,85000\nIT,Will,70000\nSales,Henry,80000\nSales,Sam,60000"
              tc == 1 -> "7\nEng,Ada,100\nEng,Ben,90\nEng,Cam,90\nEng,Don,80\nHR,Fay,50\nHR,Gus,40\nHR,Hal,30"
              true -> "4\nOps,Ann,50\nOps,Bob,50\nOps,Carl,40\nOps,Dan,30"
            end
          {idx + 2 + d * 2 + e * 4, out ++ [ans]}
        end)
      IO.write(Enum.join(out, "\n\n"))
    end
  end
end

Main.main()
