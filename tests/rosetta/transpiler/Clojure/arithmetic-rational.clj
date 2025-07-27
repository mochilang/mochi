(ns main (:refer-clojure :exclude [intSqrt sumRecip main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare intSqrt sumRecip main)

(defn intSqrt [x]
  (try (do (when (< x 2) (throw (ex-info "return" {:v x}))) (def left 1) (def right (/ x 2)) (def ans 0) (while (<= left right) (do (def mid (+ left (/ (- right left) 2))) (def sq (* mid mid)) (when (= sq x) (throw (ex-info "return" {:v mid}))) (if (< sq x) (do (def left (+ mid 1)) (def ans mid)) (def right (- mid 1))))) (throw (ex-info "return" {:v ans}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sumRecip [n]
  (try (do (def s 1) (def limit (intSqrt n)) (def f 2) (while (<= f limit) (do (when (= (mod n f) 0) (do (def s (+ s (/ n f))) (def f2 (/ n f)) (when (not= f2 f) (def s (+ s f))))) (def f (+ f 1)))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def nums [6 28 120 496 672 8128 30240 32760 523776]) (doseq [n nums] (do (def s (sumRecip n)) (when (= (mod s n) 0) (do (def val (/ s n)) (def perfect "") (when (= val 1) (def perfect "perfect!")) (println (str (str (str (str (str "Sum of recipr. factors of " (str n)) " = ") (str val)) " exactly ") perfect))))))))

(defn -main []
  (main))

(-main)
