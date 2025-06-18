def findMedianSortedArrays(nums1, nums2)
	merged = []
	i = 0
	j = 0
	while ((i < (nums1).length) || (j < (nums2).length))
		if (j >= (nums2).length)
			merged = (merged + [nums1[i]])
			i = (i + 1)
		elsif (i >= (nums1).length)
			merged = (merged + [nums2[j]])
			j = (j + 1)
		elsif (nums1[i] <= nums2[j])
			merged = (merged + [nums1[i]])
			i = (i + 1)
		else
			merged = (merged + [nums2[j]])
			j = (j + 1)
		end
	end
	total = (merged).length
	if ((total % 2) == 1)
		return merged[(total / 2)]
	end
	mid1 = merged[((total / 2) - 1)]
	mid2 = merged[(total / 2)]
	return (((mid1 + mid2)) / 2)
end

