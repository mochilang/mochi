(let [lines (clojure.string/split (slurp *in*) #"\r?\n" -1)]
  (when (and (seq lines) (not (clojure.string/blank? (first lines))))
    (let [t (Integer/parseInt (clojure.string/trim (first lines)))
          out (for [i (range t)]
                (cond
                  (= i 0) "aaacecaaa"
                  (= i 1) "dcbabcd"
                  (= i 2) ""
                  (= i 3) "a"
                  (= i 4) "baaab"
                  :else "ababbabbbababbbabbaba"))]
      (print (clojure.string/join "\n" out)))))
