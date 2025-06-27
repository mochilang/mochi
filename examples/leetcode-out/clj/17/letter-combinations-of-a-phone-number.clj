(ns main)

(defn _in [item col]
  (cond
    (and (string? col) (string? item)) (clojure.string/includes? col item)
    (map? col) (contains? col item)
    (sequential? col) (some #(= item %) col)
    :else false))
(defn letterCombinations [digits]
  (try
    (when (= (count digits) 0)
      (throw (ex-info "return" {:value []}))
    )
    (def mapping {"2" ["a" "b" "c"] "3" ["d" "e" "f"] "4" ["g" "h" "i"] "5" ["j" "k" "l"] "6" ["m" "n" "o"] "7" ["p" "q" "r" "s"] "8" ["t" "u" "v"] "9" ["w" "x" "y" "z"]})
    (def result [""])
    (loop [_tmp0 (seq digits)]
      (when _tmp0
        (let [d (clojure.core/first _tmp0)]
          (let [r (try
            (when (not (_in d mapping))
              (throw (ex-info "continue" {}))
            )
            (def letters (get mapping d))
            (def next (vec (->> (for [p result ch letters] (+ p ch)))))
            (def result next)
            :next
          (catch clojure.lang.ExceptionInfo e
            (cond
              (= (.getMessage e) "continue") :next
              (= (.getMessage e) "break") :break
              :else (throw e))
            )
          )]
        (cond
          (= r :break) nil
          :else (recur (next _tmp0))
        )
      )
    )
  )
)
(throw (ex-info "return" {:value result}))
(catch clojure.lang.ExceptionInfo e
(if (= (.getMessage e) "return")
  (:value (ex-data e))
(throw e)))
)
)

(defn test_example_1 []
(assert (= (letterCombinations "23") ["ad" "ae" "af" "bd" "be" "bf" "cd" "ce" "cf"]) "expect failed")
)

(defn test_example_2 []
(assert (= (letterCombinations "") []) "expect failed")
)

(defn test_example_3 []
(assert (= (letterCombinations "2") ["a" "b" "c"]) "expect failed")
)

(defn test_single_seven []
(assert (= (letterCombinations "7") ["p" "q" "r" "s"]) "expect failed")
)

(defn test_mix []
(assert (= (letterCombinations "79") ["pw" "px" "py" "pz" "qw" "qx" "qy" "qz" "rw" "rx" "ry" "rz" "sw" "sx" "sy" "sz"]) "expect failed")
)

(defn -main []
(test_example_1)
(test_example_2)
(test_example_3)
(test_single_seven)
(test_mix)
)

(-main)
