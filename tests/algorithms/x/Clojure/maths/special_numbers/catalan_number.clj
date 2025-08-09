(ns main (:refer-clojure :exclude [catalan main]))

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

(declare catalan main)

(def ^:dynamic catalan_current nil)

(def ^:dynamic catalan_i nil)

(defn catalan [catalan_n]
  (binding [catalan_current nil catalan_i nil] (try (do (when (< catalan_n 1) (throw (Exception. (str (str "Input value of [number=" (str catalan_n)) "] must be > 0")))) (set! catalan_current 1) (set! catalan_i 1) (while (< catalan_i catalan_n) (do (set! catalan_current (* catalan_current (- (* 4 catalan_i) 2))) (set! catalan_current (long (quot catalan_current (+ catalan_i 1)))) (set! catalan_i (+ catalan_i 1)))) (throw (ex-info "return" {:v catalan_current}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (when (not= (catalan 1) 1) (throw (Exception. "catalan(1) should be 1"))) (when (not= (catalan 5) 14) (throw (Exception. "catalan(5) should be 14"))) (println (str (catalan 5)))))

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
