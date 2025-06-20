def mergeTwoLists(l1, l2)
	i = 0
	j = 0
	result = []
	while ((i < (l1).length) && (j < (l2).length))
		if (l1[i] <= l2[j])
			result = (result + [l1[i]])
			i = (i + 1)
		else
			result = (result + [l2[j]])
			j = (j + 1)
		end
	end
	while (i < (l1).length)
		result = (result + [l1[i]])
		i = (i + 1)
	end
	while (j < (l2).length)
		result = (result + [l2[j]])
		j = (j + 1)
	end
	return result
end

