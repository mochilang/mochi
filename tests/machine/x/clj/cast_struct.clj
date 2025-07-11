(ns main)

(defn _cast_struct [ctor m]
  (let [fields (or (some->> ctor meta :arglists first (map keyword))
                   (keys m))]
    (apply ctor (map #(get m %) fields))))
(declare todo)

(defn Todo [title]
  {:__name "Todo" :title title}
)


(defn -main []
  (def todo (_cast_struct Todo {"title" "hi"})) ;; Todo
  (println (:title todo))
)

(-main)
