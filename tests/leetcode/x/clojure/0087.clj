(declare dfs)

(defn dfs [s1 s2 memo i1 i2 len]
  (let [key [i1 i2 len]]
    (if (contains? @memo key)
      (@memo key)
      (let [a (subs s1 i1 (+ i1 len))
            b (subs s2 i2 (+ i2 len))]
        (cond
          (= a b) (do (swap! memo assoc key true) true)
          (not= (sort a) (sort b)) (do (swap! memo assoc key false) false)
          :else
          (let [ans (loop [k 1]
                      (cond
                        (= k len) false
                        (or (and (dfs s1 s2 memo i1 i2 k) (dfs s1 s2 memo (+ i1 k) (+ i2 k) (- len k)))
                            (and (dfs s1 s2 memo i1 (+ i2 len (- k)) k) (dfs s1 s2 memo (+ i1 k) i2 (- len k)))) true
                        :else (recur (inc k))))]
            (swap! memo assoc key ans)
            ans))))))

(let [lines (vec (clojure.string/split-lines (slurp *in*)))]
  (when (seq lines)
    (let [t (Integer/parseInt (lines 0))]
      (print
       (clojure.string/join
        "\n"
        (for [i (range t)]
          (let [s1 (lines (inc (* 2 i))) s2 (lines (+ 2 (* 2 i)))]
            (if (dfs s1 s2 (atom {}) 0 0 (count s1)) "true" "false"))))))))
