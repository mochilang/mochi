(ns main (:refer-clojure :exclude [recursive_lucas_number dynamic_lucas_number]))

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

(declare recursive_lucas_number dynamic_lucas_number)

(def ^:dynamic dynamic_lucas_number_a nil)

(def ^:dynamic dynamic_lucas_number_b nil)

(def ^:dynamic dynamic_lucas_number_i nil)

(def ^:dynamic next_v nil)

(defn recursive_lucas_number [recursive_lucas_number_n]
  (try (do (when (= recursive_lucas_number_n 0) (throw (ex-info "return" {:v 2}))) (if (= recursive_lucas_number_n 1) 1 (+ (recursive_lucas_number (- recursive_lucas_number_n 1)) (recursive_lucas_number (- recursive_lucas_number_n 2))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn dynamic_lucas_number [dynamic_lucas_number_n]
  (binding [dynamic_lucas_number_a nil dynamic_lucas_number_b nil dynamic_lucas_number_i nil next_v nil] (try (do (set! dynamic_lucas_number_a 2) (set! dynamic_lucas_number_b 1) (set! dynamic_lucas_number_i 0) (while (< dynamic_lucas_number_i dynamic_lucas_number_n) (do (set! next_v (+ dynamic_lucas_number_a dynamic_lucas_number_b)) (set! dynamic_lucas_number_a dynamic_lucas_number_b) (set! dynamic_lucas_number_b next_v) (set! dynamic_lucas_number_i (+ dynamic_lucas_number_i 1)))) (throw (ex-info "return" {:v dynamic_lucas_number_a}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (recursive_lucas_number 1)))
      (println (str (recursive_lucas_number 20)))
      (println (str (recursive_lucas_number 0)))
      (println (str (recursive_lucas_number 5)))
      (println (str (dynamic_lucas_number 1)))
      (println (str (dynamic_lucas_number 20)))
      (println (str (dynamic_lucas_number 0)))
      (println (str (dynamic_lucas_number 25)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
