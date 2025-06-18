import Foundation

func findMedianSortedArrays(_ nums1: [Int], _ nums2: [Int]) -> Double {
    var merged: [Int] = []
    var i = 0
    var j = 0
    while i < nums1.count || j < nums2.count {
        if j >= nums2.count {
            merged.append(nums1[i])
            i += 1
        } else if i >= nums1.count {
            merged.append(nums2[j])
            j += 1
        } else if nums1[i] <= nums2[j] {
            merged.append(nums1[i])
            i += 1
        } else {
            merged.append(nums2[j])
            j += 1
        }
    }
    let total = merged.count
    if total % 2 == 1 {
        return Double(merged[total / 2])
    }
    let mid1 = merged[total / 2 - 1]
    let mid2 = merged[total / 2]
    return Double(mid1 + mid2) / 2.0
}

func example_1() { assert(findMedianSortedArrays([1,3], [2]) == 2.0) }
func example_2() { assert(findMedianSortedArrays([1,2], [3,4]) == 2.5) }
func empty_first() { assert(findMedianSortedArrays([], [1]) == 1.0) }
func empty_second() { assert(findMedianSortedArrays([2], []) == 2.0) }

func main() {
    example_1()
    example_2()
    empty_first()
    empty_second()
}
main()
