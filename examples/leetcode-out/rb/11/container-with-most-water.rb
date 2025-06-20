def maxArea(height)
	left = 0
	right = ((height).length - 1)
	maxArea = 0
	while (left < right)
		width = (right - left)
		h = 0
		if (height[left] < height[right])
			h = height[left]
		else
			h = height[right]
		end
		area = (h * width)
		if (area > method(:maxArea))
			maxArea = area
		end
		if (height[left] < height[right])
			left = (left + 1)
		else
			right = (right - 1)
		end
	end
	return method(:maxArea)
end

