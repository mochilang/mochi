(defrecord Person [name age])
(defrecord Book [title author])

(def book (->Book "Go" (->Person "Bob" 42)))
(println (:name (:author book)))
