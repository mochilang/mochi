(ns main)

(defn _cast_struct [ctor m]
  (let [fields (or (some->> ctor meta :arglists first (map keyword))
                   (keys m))]
    (apply ctor (map #(get m %) fields))))
(defn _cast_struct_list [ctor xs]
  (mapv #(_cast_struct ctor %) xs))
(declare people)

(defn Person [name age status]
  {:__name "Person" :name name :age age :status status}
)


(defn test_update_adult_status []
  (assert (= people [{:__name "Person" :name "Alice" :age 17 :status "minor"} {:__name "Person" :name "Bob" :age 26 :status "adult"} {:__name "Person" :name "Charlie" :age 19 :status "adult"} {:__name "Person" :name "Diana" :age 16 :status "minor"}]) "expect failed")
)

(defn -main []
  (def people (_cast_struct_list #'Person [{:__name "Person" :name "Alice" :age 17 :status "minor"} {:__name "Person" :name "Bob" :age 25 :status "unknown"} {:__name "Person" :name "Charlie" :age 18 :status "unknown"} {:__name "Person" :name "Diana" :age 16 :status "minor"}])) ;; list of Person
  (def people
    (vec (map (fn [_tmp0]
      (let [name (:name _tmp0) age (:age _tmp0) status (:status _tmp0)]
        (if (>= age 18) (assoc (assoc _tmp0 :status "adult") :age (+ age 1)) _tmp0)
      )
    ) people))
  )
  (println "ok")
  (test_update_adult_status)
)

(-main)
