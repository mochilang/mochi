(ns for-map-collection)

(def m {"a" 1 "b" 2})
(doseq [k (keys m)]
  (println k))
