(defn is-number [s]
  (loop [i 0 seen-digit false seen-dot false seen-exp false digit-after-exp true]
    (if (= i (count s))
      (and seen-digit digit-after-exp)
      (let [ch (.charAt s i)]
        (cond
          (Character/isDigit ch) (recur (inc i) true seen-dot seen-exp (if seen-exp true digit-after-exp))
          (or (= ch \+) (= ch \-)) (if (and (not= i 0) (not= (.charAt s (dec i)) \e) (not= (.charAt s (dec i)) \E)) false (recur (inc i) seen-digit seen-dot seen-exp digit-after-exp))
          (= ch \.) (if (or seen-dot seen-exp) false (recur (inc i) seen-digit true seen-exp digit-after-exp))
          (or (= ch \e) (= ch \E)) (if (or seen-exp (not seen-digit)) false (recur (inc i) seen-digit seen-dot true false))
          :else false)))))

(let [lines (vec (clojure.string/split-lines (slurp *in*)))]
  (when (seq lines)
    (let [t (Integer/parseInt (clojure.string/trim (lines 0)))]
      (print (clojure.string/join "\n" (for [i (range t)] (if (is-number (lines (inc i))) "true" "false")))))))
