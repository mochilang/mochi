(ns update-stmt)

(def people
  (atom [{:name "Alice" :age 17 :status "minor"}
         {:name "Bob" :age 25 :status "unknown"}
         {:name "Charlie" :age 18 :status "unknown"}
         {:name "Diana" :age 16 :status "minor"}]))

(swap! people
       (fn [ps]
         (vec (map (fn [p]
                      (if (>= (:age p) 18)
                        (-> p
                            (assoc :status "adult")
                            (update :age inc))
                        p))
                    ps))))

(def expected
  [{:name "Alice" :age 17 :status "minor"}
   {:name "Bob" :age 26 :status "adult"}
   {:name "Charlie" :age 19 :status "adult"}
   {:name "Diana" :age 16 :status "minor"}])

(when (= @people expected)
  (println "ok"))
