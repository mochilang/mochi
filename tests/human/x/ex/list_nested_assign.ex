matrix = [[1,2],[3,4]]
matrix = List.replace_at(matrix, 1, [5,4])
IO.puts(Enum.at(Enum.at(matrix,1),0))
