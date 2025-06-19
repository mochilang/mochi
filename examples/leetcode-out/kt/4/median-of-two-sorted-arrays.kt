fun findMedianSortedArrays(nums1: List<Int>, nums2: List<Int>) : Double {
        var merged: List<Int> = listOf()
        var i = 0
        var j = 0
        while (((i < nums1.size) || (j < nums2.size))) {
                if ((j >= nums2.size)) {
                        merged = (merged + listOf(nums1[i]))
                        i = (i + 1)
                } else if ((i >= nums1.size)) {
                        merged = (merged + listOf(nums2[j]))
                        j = (j + 1)
                } else if ((nums1[i] <= nums2[j])) {
                        merged = (merged + listOf(nums1[i]))
                        i = (i + 1)
                } else {
                        merged = (merged + listOf(nums2[j]))
                        j = (j + 1)
                }
        }
        val total = merged.size
        if (((total % 2) == 1)) {
                return (merged[(total / 2)]).toDouble()
        }
        val mid1 = merged[((total / 2) - 1)]
        val mid2 = merged[(total / 2)]
        return ((((mid1 + mid2))).toDouble() / 2)
}

fun main() {
}

