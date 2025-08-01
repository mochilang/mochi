(ns main (:refer-clojure :exclude [createLine evalX intersection main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare createLine evalX intersection main)

(declare l1 l2 p slope x y yint)

(defn createLine [a b]
  (try (do (def slope (/ (- (:y b) (:y a)) (- (:x b) (:x a)))) (def yint (- (:y a) (* slope (:x a)))) (throw (ex-info "return" {:v {:slope slope :yint yint}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn evalX [l x]
  (try (throw (ex-info "return" {:v (+' (* (:slope l) x) (:yint l))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn intersection [l1 l2]
  (try (do (when (= (:slope l1) (:slope l2)) (throw (ex-info "return" {:v {:x 0.0 :y 0.0}}))) (def x (/ (- (:yint l2) (:yint l1)) (- (:slope l1) (:slope l2)))) (def y (evalX l1 x)) (throw (ex-info "return" {:v {:x x :y y}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def l1 (createLine {:x 4.0 :y 0.0} {:x 6.0 :y 10.0})) (def l2 (createLine {:x 0.0 :y 3.0} {:x 10.0 :y 7.0})) (def p (intersection l1 l2)) (println (str (str (str (str "{" (str (:x p))) " ") (str (:y p))) "}"))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (main)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
