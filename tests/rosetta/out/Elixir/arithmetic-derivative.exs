# Generated by Mochi compiler v0.10.26 on 2025-07-16T12:45:08Z
defmodule Main do
	@spec primeFactors(integer()) :: list(integer())
	def primeFactors(n) do
		try do
			factors = []
			_ = factors
			x = n
			_ = x
			t1 = fn t1, factors, x ->
				try do
					if (rem(x, 2) == 0) do
						factors = factors ++ [2]
						x = String.to_integer((x / 2))
						t1.(t1, factors, x)
					else
						{:ok, factors, x}
					end
				catch :break ->
					{:ok, factors, x}
				end
			end
			{_, factors, x} = t1.(t1, factors, x)
			_ = factors
			_ = x
			p = 3
			_ = p
			t2 = fn t2, factors, p, x ->
				try do
					if ((p * p) <= x) do
						t3 = fn t3, factors, x ->
							try do
								if (rem(x, p) == 0) do
									factors = factors ++ [p]
									x = String.to_integer((x / p))
									t3.(t3, factors, x)
								else
									{:ok, factors, x}
								end
							catch :break ->
								{:ok, factors, x}
							end
						end
						{_, factors, x} = t3.(t3, factors, x)
						_ = factors
						_ = x
						p = (p + 2)
						t2.(t2, factors, p, x)
					else
						{:ok, factors, p, x}
					end
				catch :break ->
					{:ok, factors, p, x}
				end
			end
			{_, factors, p, x} = t2.(t2, factors, p, x)
			_ = factors
			_ = p
			_ = x
			if (x > 1) do
				factors = factors ++ [x]
			end
			throw {:return, factors}
		catch {:return, v} -> v end
	end
	
	@spec repeat(String.t(), integer()) :: String.t()
	def repeat(ch, n) do
		try do
			s = ""
			_ = s
			i = 0
			_ = i
			t4 = fn t4, i, s ->
				try do
					if (i < n) do
						s = (s + ch)
						i = (i + 1)
						t4.(t4, i, s)
					else
						{:ok, i, s}
					end
				catch :break ->
					{:ok, i, s}
				end
			end
			{_, i, s} = t4.(t4, i, s)
			_ = i
			_ = s
			throw {:return, s}
		catch {:return, v} -> v end
	end
	
	@spec D(float()) :: float()
	def D(n) do
		try do
			if (n < 0) do
				throw {:return, -D(-n)}
			end
			if (n < 2) do
				throw {:return, 0}
			end
			factors = []
			_ = factors
			if (n < 10000000000000000000) do
				factors = primeFactors(String.to_integer(n))
			else
				g = String.to_integer((n / 100))
				factors = primeFactors(g)
				factors = factors ++ [2]
				factors = factors ++ [2]
				factors = factors ++ [5]
				factors = factors ++ [5]
			end
			c = length(factors)
			if (c == 1) do
				throw {:return, 1}
			end
			if (c == 2) do
				throw {:return, String.to_float((Enum.at((factors), 0) + Enum.at((factors), 1)))}
			end
			d = (n / String.to_float(Enum.at((factors), 0)))
			throw {:return, ((D(d) * String.to_float(Enum.at((factors), 0))) + d)}
		catch {:return, v} -> v end
	end
	
	@spec pad(integer()) :: String.t()
	def pad(n) do
		try do
			s = to_string(n)
			_ = s
			t5 = fn t5, s ->
				try do
					if (length(s) < 4) do
						s = (" " <> s)
						t5.(t5, s)
					else
						{:ok, s}
					end
				catch :break ->
					{:ok, s}
				end
			end
			{_, s} = t5.(t5, s)
			_ = s
			throw {:return, s}
		catch {:return, v} -> v end
	end
	
	@spec main() :: nil
	def main() do
		try do
			vals = []
			_ = vals
			n = -99
			_ = n
			t6 = fn t6, n, vals ->
				try do
					if (n < 101) do
						vals = vals ++ [String.to_integer(D(String.to_float(n)))]
						n = (n + 1)
						t6.(t6, n, vals)
					else
						{:ok, n, vals}
					end
				catch :break ->
					{:ok, n, vals}
				end
			end
			{_, n, vals} = t6.(t6, n, vals)
			_ = n
			_ = vals
			i = 0
			_ = i
			t7 = fn t7, i ->
				try do
					if (i < length(vals)) do
						line = ""
						_ = line
						j = 0
						_ = j
						t8 = fn t8, j, line ->
							try do
								if (j < 10) do
									line = (line + pad(Enum.at((vals), (i + j))))
									if (j < 9) do
										line = (line <> " ")
									end
									j = (j + 1)
									t8.(t8, j, line)
								else
									{:ok, j, line}
								end
							catch :break ->
								{:ok, j, line}
							end
						end
						{_, j, line} = t8.(t8, j, line)
						_ = j
						_ = line
						IO.inspect(line)
						i = (i + 10)
						t7.(t7, i)
					else
						{:ok, i}
					end
				catch :break ->
					{:ok, i}
				end
			end
			{_, i} = t7.(t7, i)
			_ = i
			pow = 1
			_ = pow
			m = 1
			_ = m
			t9 = fn t9, m, pow ->
				try do
					if (m < 21) do
						pow = (pow * 10)
						exp = to_string(m)
						_ = exp
						if (length(exp) < 2) do
							exp = (exp <> " ")
						end
						res = (to_string(m) + repeat("0", (m - 1)))
						_ = res
						IO.inspect(((("D(10^" <> exp) <> ") / 7 = ") <> res))
						m = (m + 1)
						t9.(t9, m, pow)
					else
						{:ok, m, pow}
					end
				catch :break ->
					{:ok, m, pow}
				end
			end
			{_, m, pow} = t9.(t9, m, pow)
			_ = m
			_ = pow
		catch {:return, v} -> v end
	end
	
	def main do
		main()
	end
	end
Main.main()
