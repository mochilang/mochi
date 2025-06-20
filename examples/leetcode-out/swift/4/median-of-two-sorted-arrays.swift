import Foundation

func findMedianSortedArrays(_ nums1: [Int], _ nums2: [Int]) -> Double {
	let nums1 = nums1
	let nums2 = nums2
	
	var merged: [Int] = []
	var i = 0
	var j = 0
	while i < nums1.count || j < nums2.count {
		if j >= nums2.count {
			merged = merged + [nums1[i]]
			i = i + 1
		} else 		if i >= nums1.count {
			merged = merged + [nums2[j]]
			j = j + 1
		} else 		if nums1[i] <= nums2[j] {
			merged = merged + [nums1[i]]
			i = i + 1
		} else {
			merged = merged + [nums2[j]]
			j = j + 1
		}
	}
	let total = merged.count
	if total % 2 == 1 {
		return merged[total / 2] as! Double
	}
	let mid1 = merged[total / 2 - 1]
	let mid2 = merged[total / 2]
	return (mid1 + mid2) as! Double / 2
}

func main() {
}
main()
