;; Solution for SPOJ TEST - Life, the Universe, and Everything
;; https://www.spoj.com/problems/TEST

(loop []
  (when-let [line (read-line)]
    (let [n (Integer/parseInt line)]
      (when (not= n 42)
        (println n)
        (recur)))))
