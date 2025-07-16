def mean(v)
	if ((v).length == 0)
		return {"ok" => false}
	end
	sum = 0.0
	i = 0
	while (i < (v).length)
		sum = (sum + v[i])
		i = (i + 1)
	end
	return {"ok" => true, "mean" => (sum / ((v).length))}
end

def main()
	sets = [[], [3.0, 1.0, 4.0, 1.0, 5.0, 9.0], [100000000000000000000.0, 3.0, 1.0, 4.0, 1.0, 5.0, 9.0, (-100000000000000000000.0)], [10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.11], [10.0, 20.0, 30.0, 40.0, 50.0, (-100.0), 4.7, (-1100.0)]]
	sets.each do |v|
		puts(("Vector: " + (v).to_s))
		r = mean(v)
		if r["ok"]
			puts(((("Mean of " + ((v).length).to_s) + " numbers is ") + (r["mean"]).to_s))
		else
			puts("Mean undefined")
		end
		puts("")
	end
end

main()
