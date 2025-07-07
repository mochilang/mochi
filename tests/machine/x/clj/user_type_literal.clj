(ns main)

(declare book)

(defn Person [name age]
  {:__name "Person" :name name :age age}
)


(defn Book [title author]
  {:__name "Book" :title title :author author}
)


(defn -main []
  (def book {:__name "Book" :title "Go" :author {:__name "Person" :name "Bob" :age 42}}) ;; Book
  (println (:name (:author book)))
)

(-main)
