(ns main (:refer-clojure :exclude [floor pow10 round decimal_isolate main]))

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

(declare floor pow10 round decimal_isolate main)

(def ^:dynamic decimal_isolate_frac nil)

(def ^:dynamic decimal_isolate_whole nil)

(def ^:dynamic floor_i nil)

(def ^:dynamic pow10_i nil)

(def ^:dynamic pow10_p nil)

(def ^:dynamic round_m nil)

(defn floor [floor_x]
  (binding [floor_i nil] (try (do (set! floor_i (long floor_x)) (when (> (double floor_i) floor_x) (set! floor_i (- floor_i 1))) (throw (ex-info "return" {:v (double floor_i)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow10 [pow10_n]
  (binding [pow10_i nil pow10_p nil] (try (do (set! pow10_p 1.0) (set! pow10_i 0) (while (< pow10_i pow10_n) (do (set! pow10_p (* pow10_p 10.0)) (set! pow10_i (+ pow10_i 1)))) (throw (ex-info "return" {:v pow10_p}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn round [round_x round_n]
  (binding [round_m nil] (try (do (set! round_m (pow10 round_n)) (throw (ex-info "return" {:v (quot (floor (+ (* round_x round_m) 0.5)) round_m)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decimal_isolate [decimal_isolate_number decimal_isolate_digit_amount]
  (binding [decimal_isolate_frac nil decimal_isolate_whole nil] (try (do (set! decimal_isolate_whole (long decimal_isolate_number)) (set! decimal_isolate_frac (- decimal_isolate_number (double decimal_isolate_whole))) (if (> decimal_isolate_digit_amount 0) (round decimal_isolate_frac decimal_isolate_digit_amount) decimal_isolate_frac)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (str (decimal_isolate 1.53 0))) (println (str (decimal_isolate 35.345 1))) (println (str (decimal_isolate 35.345 2))) (println (str (decimal_isolate 35.345 3))) (println (str (decimal_isolate (- 14.789) 3))) (println (str (decimal_isolate 0.0 2))) (println (str (decimal_isolate (- 14.123) 1))) (println (str (decimal_isolate (- 14.123) 2))) (println (str (decimal_isolate (- 14.123) 3)))))

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
