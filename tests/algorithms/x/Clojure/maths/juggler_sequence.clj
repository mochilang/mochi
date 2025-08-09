(ns main (:refer-clojure :exclude [to_float sqrt floor juggler_sequence]))

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

(declare to_float sqrt floor juggler_sequence)

(def ^:dynamic floor_n nil)

(def ^:dynamic floor_y nil)

(def ^:dynamic juggler_sequence_current nil)

(def ^:dynamic juggler_sequence_r nil)

(def ^:dynamic juggler_sequence_seq nil)

(def ^:dynamic sqrt_guess nil)

(def ^:dynamic sqrt_i nil)

(defn to_float [to_float_x]
  (try (throw (ex-info "return" {:v (* to_float_x 1.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrt [sqrt_x]
  (binding [sqrt_guess nil sqrt_i nil] (try (do (when (<= sqrt_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrt_guess sqrt_x) (set! sqrt_i 0) (while (< sqrt_i 10) (do (set! sqrt_guess (/ (+ sqrt_guess (quot sqrt_x sqrt_guess)) 2.0)) (set! sqrt_i (+ sqrt_i 1)))) (throw (ex-info "return" {:v sqrt_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn floor [floor_x]
  (binding [floor_n nil floor_y nil] (try (do (set! floor_n 0) (set! floor_y floor_x) (while (>= floor_y 1.0) (do (set! floor_y (- floor_y 1.0)) (set! floor_n (+ floor_n 1)))) (throw (ex-info "return" {:v floor_n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn juggler_sequence [juggler_sequence_n]
  (binding [juggler_sequence_current nil juggler_sequence_r nil juggler_sequence_seq nil] (try (do (when (< juggler_sequence_n 1) (throw (Exception. "number must be a positive integer"))) (set! juggler_sequence_seq [juggler_sequence_n]) (set! juggler_sequence_current juggler_sequence_n) (while (not= juggler_sequence_current 1) (do (if (= (mod juggler_sequence_current 2) 0) (set! juggler_sequence_current (floor (sqrt (to_float juggler_sequence_current)))) (do (set! juggler_sequence_r (sqrt (to_float juggler_sequence_current))) (set! juggler_sequence_current (floor (* (* juggler_sequence_r juggler_sequence_r) juggler_sequence_r))))) (set! juggler_sequence_seq (conj juggler_sequence_seq juggler_sequence_current)))) (throw (ex-info "return" {:v juggler_sequence_seq}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (juggler_sequence 3)))
      (println (str (juggler_sequence 10)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
