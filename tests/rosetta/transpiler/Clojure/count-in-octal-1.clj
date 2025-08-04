(ns main (:refer-clojure :exclude [toOct main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare toOct main)

(def ^:dynamic toOct_d nil)

(def ^:dynamic toOct_digits nil)

(def ^:dynamic toOct_out nil)

(def ^:dynamic toOct_v nil)

(defn toOct [toOct_n]
  (binding [toOct_d nil toOct_digits nil toOct_out nil toOct_v nil] (try (do (when (= toOct_n 0) (throw (ex-info "return" {:v "0"}))) (set! toOct_digits "01234567") (set! toOct_out "") (set! toOct_v toOct_n) (while (> toOct_v 0) (do (set! toOct_d (mod toOct_v 8)) (set! toOct_out (str (subs toOct_digits toOct_d (+ toOct_d 1)) toOct_out)) (set! toOct_v (quot toOct_v 8)))) (throw (ex-info "return" {:v toOct_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (dotimes [i 128] (println (toOct i))))

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
