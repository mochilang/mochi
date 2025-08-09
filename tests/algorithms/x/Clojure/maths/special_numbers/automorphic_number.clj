(ns main (:refer-clojure :exclude [is_automorphic_number]))

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

(declare is_automorphic_number)

(def ^:dynamic is_automorphic_number_n nil)

(def ^:dynamic is_automorphic_number_sq nil)

(defn is_automorphic_number [is_automorphic_number_number]
  (binding [is_automorphic_number_n nil is_automorphic_number_sq nil] (try (do (when (< is_automorphic_number_number 0) (throw (ex-info "return" {:v false}))) (set! is_automorphic_number_n is_automorphic_number_number) (set! is_automorphic_number_sq (* is_automorphic_number_number is_automorphic_number_number)) (while (> is_automorphic_number_n 0) (do (when (not= (mod is_automorphic_number_n 10) (mod is_automorphic_number_sq 10)) (throw (ex-info "return" {:v false}))) (set! is_automorphic_number_n (quot is_automorphic_number_n 10)) (set! is_automorphic_number_sq (quot is_automorphic_number_sq 10)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (is_automorphic_number 0)))
      (println (str (is_automorphic_number 1)))
      (println (str (is_automorphic_number 5)))
      (println (str (is_automorphic_number 6)))
      (println (str (is_automorphic_number 7)))
      (println (str (is_automorphic_number 25)))
      (println (str (is_automorphic_number 376)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
