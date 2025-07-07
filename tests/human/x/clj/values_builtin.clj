(require '[clojure.string :as str])
(let [m {"a" 1 "b" 2 "c" 3}]
  (println (str/join " " (vals m))))
