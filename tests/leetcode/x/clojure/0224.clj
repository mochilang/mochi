(defn calculate [expr]
  (loop [chars (seq expr), result 0, number 0, sign 1, stack []]
    (if (nil? chars)
      (+ result (* sign number))
      (let [ch (first chars)]
        (cond
          (Character/isDigit ^char ch)
          (recur (next chars) result (+ (* number 10) (- (int ch) (int \0))) sign stack)

          (or (= ch \+) (= ch \-))
          (recur (next chars) (+ result (* sign number)) 0 (if (= ch \+) 1 -1) stack)

          (= ch \()
          (recur (next chars) 0 0 1 (conj stack result sign))

          (= ch \))
          (let [result2 (+ result (* sign number))
                prev-sign (peek stack)
                stack2 (pop stack)
                prev-result (peek stack2)
                stack3 (pop stack2)]
            (recur (next chars) (+ prev-result (* prev-sign result2)) 0 1 stack3))

          :else
          (recur (next chars) result number sign stack))))))

(let [lines (clojure.string/split-lines (slurp *in*))]
  (when (seq lines)
    (let [t (Integer/parseInt (clojure.string/trim (first lines)))
          exprs (take t (rest lines))]
      (print (clojure.string/join "\n" (map #(str (calculate %)) exprs))))))
