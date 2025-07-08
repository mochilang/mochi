(ns main)

(declare t)

(def Leaf {:__name "Leaf"})

(defn Node [left value right]
  {:__name "Node" :left left :value value :right right}
)


;; Function sum_tree takes [t: Tree] and returns int
(defn sum_tree [t]
  (try
    (throw (ex-info "return" {:value (let [t t]
  (cond
    (= (:__name t) "Leaf") 0
    (= (:__name t) "Node") (let [left (:left t) value (:value t) right (:right t)] (+ (+ (sum_tree left) value) (sum_tree right)))
    :else nil
  ))}))
  (catch clojure.lang.ExceptionInfo e
    (if (= (.getMessage e) "return")
      (:value (ex-data e))
    (throw e)))
  )
)

(defn -main []
  (def t {:__name "Node" :left Leaf :value 1 :right {:__name "Node" :left Leaf :value 2 :right Leaf}}) ;; Node
  (println (sum_tree t))
)

(-main)
