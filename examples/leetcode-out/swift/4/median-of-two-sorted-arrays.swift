import Foundation

func expect(_ cond: Bool) {
  if !cond { fatalError("expect failed") }
}

func _cast<T: Decodable>(_ type: T.Type, _ v: Any) -> T {
  if let tv = v as? T { return tv }
  if let data = try? JSONSerialization.data(withJSONObject: v),
    let obj = try? JSONDecoder().decode(T.self, from: data)
  {
    return obj
  }
  fatalError("cast failed")
}
func _index<T>(_ arr: [T], _ i: Int) -> T {
  var idx = i
  let n = arr.count
  if idx < 0 { idx += n }
  if idx < 0 || idx >= n { fatalError("index out of range") }
  return arr[idx]
}

func findMedianSortedArrays(_ nums1: [Int], _ nums2: [Int]) -> Double {
  let nums1 = nums1
  let nums2 = nums2

  var merged: [Int] = []
  var i = 0
  var j = 0
  while i < nums1.count || j < nums2.count {
    if j >= nums2.count {
      merged = merged + [_index(nums1, i)]
      i = i + 1
    } else if i >= nums1.count {
      merged = merged + [_index(nums2, j)]
      j = j + 1
    } else if _index(nums1, i) <= _index(nums2, j) {
      merged = merged + [_index(nums1, i)]
      i = i + 1
    } else {
      merged = merged + [_index(nums2, j)]
      j = j + 1
    }
  }
  let total = merged.count
  if total % 2 == 1 {
    return _cast(Double.self, _index(merged, total / 2))
  }
  let mid1 = _index(merged, total / 2 - 1)
  let mid2 = _index(merged, total / 2)
  return _cast(Double.self, (mid1 + mid2)) / 2
}

func test_example_1() {
  expect(findMedianSortedArrays([1, 3], [2]) == 2)
}

func test_example_2() {
  expect(findMedianSortedArrays([1, 2], [3, 4]) == 2.5)
}

func test_empty_first() {
  expect(findMedianSortedArrays(_cast([Int].self, []), [1]) == 1)
}

func test_empty_second() {
  expect(findMedianSortedArrays([2], _cast([Int].self, [])) == 2)
}

func main() {
  test_example_1()
  test_example_2()
  test_empty_first()
  test_empty_second()
}
main()
