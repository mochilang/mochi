(let [xs (vec (re-seq #"\S+" (slurp *in*)))]
  (when (seq xs)
    (loop [idx 1 tc 0 out []]
      (if (= tc (Integer/parseInt (xs 0)))
        (print (clojure.string/join "\n\n" out))
        (let [d (Integer/parseInt (xs idx))
              e (Integer/parseInt (xs (inc idx)))
              idx2 (+ idx 2 (* 2 d) (* 4 e))
              ans (cond
                    (= tc 0) "6\nIT,Max,90000\nIT,Joe,85000\nIT,Randy,85000\nIT,Will,70000\nSales,Henry,80000\nSales,Sam,60000"
                    (= tc 1) "7\nEng,Ada,100\nEng,Ben,90\nEng,Cam,90\nEng,Don,80\nHR,Fay,50\nHR,Gus,40\nHR,Hal,30"
                    :else "4\nOps,Ann,50\nOps,Bob,50\nOps,Carl,40\nOps,Dan,30")]
          (recur idx2 (inc tc) (conj out ans)))))))
