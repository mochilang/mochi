object Main {
    def findMedianSortedArrays(nums1: scala.collection.mutable.ArrayBuffer[Int], nums2: scala.collection.mutable.ArrayBuffer[Int]): Double = {
        var merged = scala.collection.mutable.ArrayBuffer()
        var i: Int = 0
        var j: Int = 0
        while (((i < nums1.length) || (j < nums2.length))) {
            if ((j >= nums2.length)) {
                merged = (merged ++ scala.collection.mutable.ArrayBuffer(_indexList(nums1, i)))
                i = (i + 1)
            } else             if ((i >= nums1.length)) {
                merged = (merged ++ scala.collection.mutable.ArrayBuffer(_indexList(nums2, j)))
                j = (j + 1)
            } else             if ((_indexList(nums1, i) <= _indexList(nums2, j))) {
                merged = (merged ++ scala.collection.mutable.ArrayBuffer(_indexList(nums1, i)))
                i = (i + 1)
            } else {
                merged = (merged ++ scala.collection.mutable.ArrayBuffer(_indexList(nums2, j)))
                j = (j + 1)
            }
        }
        val total: Int = merged.length
        if (((total % 2) == 1)) {
            return _cast[Double](_indexList(merged, (total / 2)))
        }
        val mid1: Int = _indexList(merged, ((total / 2) - 1))
        val mid2: Int = _indexList(merged, (total / 2))
        return (_cast[Double](((mid1 + mid2))) / 2)
    }
    
    def test_example_1(): Unit = {
        expect((findMedianSortedArrays(scala.collection.mutable.ArrayBuffer(1, 3), scala.collection.mutable.ArrayBuffer(2)) == 2))
    }
    
    def test_example_2(): Unit = {
        expect((findMedianSortedArrays(scala.collection.mutable.ArrayBuffer(1, 2), scala.collection.mutable.ArrayBuffer(3, 4)) == 2.5))
    }
    
    def test_empty_first(): Unit = {
        expect((findMedianSortedArrays(_cast[scala.collection.mutable.ArrayBuffer[Int]](scala.collection.mutable.ArrayBuffer()), scala.collection.mutable.ArrayBuffer(1)) == 1))
    }
    
    def test_empty_second(): Unit = {
        expect((findMedianSortedArrays(scala.collection.mutable.ArrayBuffer(2), _cast[scala.collection.mutable.ArrayBuffer[Int]](scala.collection.mutable.ArrayBuffer())) == 2))
    }
    
    def main(args: Array[String]): Unit = {
        test_example_1()
        test_example_2()
        test_empty_first()
        test_empty_second()
    }
}
def _cast[T](v: Any)(implicit ct: scala.reflect.ClassTag[T]): T = {
        val cls = ct.runtimeClass
        if (cls == classOf[Int]) v match {
                case i: Int => i
                case d: Double => d.toInt
                case s: String => s.toInt
                case _ => 0
        } else if (cls == classOf[Double]) v match {
                case d: Double => d
                case i: Int => i.toDouble
                case s: String => s.toDouble
                case _ => 0.0
        } else if (cls == classOf[Boolean]) v match {
                case b: Boolean => b
                case s: String => s == "true"
                case _ => false
        } else if (cls == classOf[String]) v.toString.asInstanceOf[T]
        else if (cls.isInstance(v)) v.asInstanceOf[T]
        else if (v.isInstanceOf[scala.collection.Map[_, _]]) {
                val m = v.asInstanceOf[scala.collection.Map[String, Any]]
                val ctor = cls.getConstructors.head
                val params = ctor.getParameters.map { p =>
                        m.getOrElse(p.getName, null).asInstanceOf[AnyRef]
                }
                ctor.newInstance(params: _*).asInstanceOf[T]
        } else {
                v.asInstanceOf[T]
        }
}

def expect(cond: Boolean): Unit = {
        if (!cond) throw new RuntimeException("expect failed")
}

def _indexList[T](arr: scala.collection.mutable.ArrayBuffer[T], i: Int): T = {
        var idx = i
        val n = arr.length
        if (idx < 0) idx += n
        if (idx < 0 || idx >= n) throw new RuntimeException("index out of range")
        arr(idx)
}

