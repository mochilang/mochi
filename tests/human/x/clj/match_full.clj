(ns match-full)

(let [x 2
      label (cond
              (= x 1) "one"
              (= x 2) "two"
              (= x 3) "three"
              :else "unknown")]
  (println label))

(let [day "sun"
      mood (cond
             (= day "mon") "tired"
             (= day "fri") "excited"
             (= day "sun") "relaxed"
             :else "normal")]
  (println mood))

(let [ok true
      status (if ok "confirmed" "denied")]
  (println status))

(defn classify [n]
  (cond
    (= n 0) "zero"
    (= n 1) "one"
    :else "many"))

(println (classify 0))
(println (classify 5))
