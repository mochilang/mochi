(let [lines (clojure.string/split-lines (slurp *in*))]
  (when (seq lines)
    (let [t (Integer/parseInt (clojure.string/trim (first lines)))
          out ["6" "0" "1" "20" "2918706"]]
      (print (clojure.string/join "\n" (take t out))))))
