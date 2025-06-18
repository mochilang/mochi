(defn findMedianSortedArrays [nums1 nums2]
  (try
    (def merged [])
    (def i 0)
    (def j 0)
    (try
      (loop []
        (when (or (< i (count nums1)) (< j (count nums2)))
          (if (>= j (count nums2))
            (do
              (def merged (vec (concat merged [(nth nums1 i)])))
              (def i (+ i 1))
            )
          
          (if (>= i (count nums1))
            (do
              (def merged (vec (concat merged [(nth nums2 j)])))
              (def j (+ j 1))
            )
          
          (if (<= (nth nums1 i) (nth nums2 j))
            (do
              (def merged (vec (concat merged [(nth nums1 i)])))
              (def i (+ i 1))
            )
          
          (do
            (def merged (vec (concat merged [(nth nums2 j)])))
            (def j (+ j 1))
          )
          )
          )
          )
          (recur)
        )
      )
    (catch clojure.lang.ExceptionInfo e
      (when-not (= (.getMessage e) "break") (throw e))
    )
  )
  (def total (count merged))
  (when (= (mod total 2) 1)
    (throw (ex-info "return" {:value (double (nth merged (quot total 2)))}))
  )
  (def mid1 (nth merged (- (quot total 2) 1)))
  (def mid2 (nth merged (quot total 2)))
  (throw (ex-info "return" {:value (/ (double (+ mid1 mid2)) 2.0)}))
(catch clojure.lang.ExceptionInfo e
  (if (= (.getMessage e) "return")
    (:value (ex-data e))
  (throw e)))
)
)

(defn test_example_1 []
(assert (= (findMedianSortedArrays [1 3] [2]) 2.0))
)

(defn test_example_2 []
(assert (= (findMedianSortedArrays [1 2] [3 4]) 2.5))
)

(defn test_empty_first []
(assert (= (findMedianSortedArrays [] [1]) 1.0))
)

(defn test_empty_second []
(assert (= (findMedianSortedArrays [2] []) 2.0))
)

(test_example_1)
(test_example_2)
(test_empty_first)
(test_empty_second)
