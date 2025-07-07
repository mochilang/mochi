(require '[clojure.data.json :as json])
(let [m {:a 1 :b 2}]
  (println (json/write-str m)))
