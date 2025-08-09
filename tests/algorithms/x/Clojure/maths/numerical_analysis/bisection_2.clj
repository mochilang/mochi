(ns main (:refer-clojure :exclude [equation bisection]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(defn toi [s]
  (Integer/parseInt (str s)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare equation bisection)

(def ^:dynamic bisection_c nil)

(def ^:dynamic bisection_left nil)

(def ^:dynamic bisection_right nil)

(defn equation [equation_x]
  (try (throw (ex-info "return" {:v (- 10.0 (* equation_x equation_x))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bisection [bisection_a bisection_b]
  (binding [bisection_c nil bisection_left nil bisection_right nil] (try (do (when (>= (* (equation bisection_a) (equation bisection_b)) 0.0) (throw (Exception. "Wrong space!"))) (set! bisection_left bisection_a) (set! bisection_right bisection_b) (set! bisection_c bisection_left) (loop [while_flag_1 true] (when (and while_flag_1 (>= (- bisection_right bisection_left) 0.01)) (do (set! bisection_c (/ (+ bisection_left bisection_right) 2.0)) (cond (= (equation bisection_c) 0.0) (recur false) :else (do (if (< (* (equation bisection_c) (equation bisection_left)) 0.0) (set! bisection_right bisection_c) (set! bisection_left bisection_c)) (recur while_flag_1)))))) (throw (ex-info "return" {:v bisection_c}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (bisection (- 2.0) 5.0))
      (println (bisection 0.0 6.0))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
