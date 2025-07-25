# Generated by Mochi compiler v0.10.26 on 2025-07-16T12:45:03Z
defmodule Main do
	@PI 3.141592653589793
	@turns 2
	@width 600
	@a 1
	@b 20
	@spec sinApprox(float()) :: float()
	def sinApprox(x) do
		try do
			term = x
			_ = term
			# sum :: (any() -> float())
			sum = x
			_ = sum
			n = 1
			_ = n
			t1 = fn t1, n, sum, term ->
				try do
					if (n <= 10) do
						# denom :: (any() -> any())
						denom = String.to_float(((2 * n) * ((2 * n) + 1)))
						term = (((-term * x) * x) / denom)
						sum = (sum + term)
						n = (n + 1)
						t1.(t1, n, sum, term)
					else
						{:ok, n, sum, term}
					end
				catch :break ->
					{:ok, n, sum, term}
				end
			end
			{_, n, sum, term} = t1.(t1, n, sum, term)
			_ = n
			_ = sum
			_ = term
			throw {:return, sum}
		catch {:return, v} -> v end
	end
	
	@spec cosApprox(float()) :: float()
	def cosApprox(x) do
		try do
			term = 1
			_ = term
			# sum :: (any() -> float())
			sum = 1
			_ = sum
			n = 1
			_ = n
			t2 = fn t2, n, sum, term ->
				try do
					if (n <= 10) do
						# denom :: (any() -> any())
						denom = String.to_float((((2 * n) - 1) * (2 * n)))
						term = (((-term * x) * x) / denom)
						sum = (sum + term)
						n = (n + 1)
						t2.(t2, n, sum, term)
					else
						{:ok, n, sum, term}
					end
				catch :break ->
					{:ok, n, sum, term}
				end
			end
			{_, n, sum, term} = t2.(t2, n, sum, term)
			_ = n
			_ = sum
			_ = term
			throw {:return, sum}
		catch {:return, v} -> v end
	end
	
	def main do
		# degreesIncr :: float()
		degreesIncr = ((0.1 * @PI) / 180)
		# stop :: float()
		stop = (((360 * @turns) * 10) * degreesIncr)
		# centre :: float()
		centre = (@width / 2)
		# theta :: float()
		theta = 0
		_ = theta
		# count :: integer()
		count = 0
		_ = count
		t3 = fn t3, count, theta ->
			try do
				if (theta < stop) do
					r = (@a + (@b * theta))
					x = (r * cosApprox(theta))
					y = (r * sinApprox(theta))
					if (rem(count, 100) == 0) do
						IO.puts(((to_string((centre + x)) <> ",") <> to_string((centre - y))))
					end
					theta = (theta + degreesIncr)
					count = (count + 1)
					t3.(t3, count, theta)
				else
					{:ok, count, theta}
				end
			catch :break ->
				{:ok, count, theta}
			end
		end
		{_, count, theta} = t3.(t3, count, theta)
		_ = count
		_ = theta
	end
	end
Main.main()
