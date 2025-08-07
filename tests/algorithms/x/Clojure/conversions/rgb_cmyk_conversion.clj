(ns main (:refer-clojure :exclude [round_int rgb_to_cmyk]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare round_int rgb_to_cmyk)

(def ^:dynamic rgb_to_cmyk_b nil)

(def ^:dynamic rgb_to_cmyk_c nil)

(def ^:dynamic rgb_to_cmyk_c_float nil)

(def ^:dynamic rgb_to_cmyk_g nil)

(def ^:dynamic rgb_to_cmyk_k nil)

(def ^:dynamic rgb_to_cmyk_k_float nil)

(def ^:dynamic rgb_to_cmyk_k_percent nil)

(def ^:dynamic rgb_to_cmyk_m nil)

(def ^:dynamic rgb_to_cmyk_m_float nil)

(def ^:dynamic rgb_to_cmyk_max_val nil)

(def ^:dynamic rgb_to_cmyk_r nil)

(def ^:dynamic rgb_to_cmyk_y nil)

(def ^:dynamic rgb_to_cmyk_y_float nil)

(defn round_int [round_int_x]
  (try (throw (ex-info "return" {:v (long (+ round_int_x 0.5))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rgb_to_cmyk [rgb_to_cmyk_r_input rgb_to_cmyk_g_input rgb_to_cmyk_b_input]
  (binding [rgb_to_cmyk_b nil rgb_to_cmyk_c nil rgb_to_cmyk_c_float nil rgb_to_cmyk_g nil rgb_to_cmyk_k nil rgb_to_cmyk_k_float nil rgb_to_cmyk_k_percent nil rgb_to_cmyk_m nil rgb_to_cmyk_m_float nil rgb_to_cmyk_max_val nil rgb_to_cmyk_r nil rgb_to_cmyk_y nil rgb_to_cmyk_y_float nil] (try (do (when (or (or (or (or (or (< rgb_to_cmyk_r_input 0) (>= rgb_to_cmyk_r_input 256)) (< rgb_to_cmyk_g_input 0)) (>= rgb_to_cmyk_g_input 256)) (< rgb_to_cmyk_b_input 0)) (>= rgb_to_cmyk_b_input 256)) (throw (Exception. "Expected int of the range 0..255"))) (set! rgb_to_cmyk_r (/ (double rgb_to_cmyk_r_input) 255.0)) (set! rgb_to_cmyk_g (/ (double rgb_to_cmyk_g_input) 255.0)) (set! rgb_to_cmyk_b (/ (double rgb_to_cmyk_b_input) 255.0)) (set! rgb_to_cmyk_max_val rgb_to_cmyk_r) (when (> rgb_to_cmyk_g rgb_to_cmyk_max_val) (set! rgb_to_cmyk_max_val rgb_to_cmyk_g)) (when (> rgb_to_cmyk_b rgb_to_cmyk_max_val) (set! rgb_to_cmyk_max_val rgb_to_cmyk_b)) (set! rgb_to_cmyk_k_float (- 1.0 rgb_to_cmyk_max_val)) (when (= rgb_to_cmyk_k_float 1.0) (throw (ex-info "return" {:v [0 0 0 100]}))) (set! rgb_to_cmyk_c_float (/ (* 100.0 (- (- 1.0 rgb_to_cmyk_r) rgb_to_cmyk_k_float)) (- 1.0 rgb_to_cmyk_k_float))) (set! rgb_to_cmyk_m_float (/ (* 100.0 (- (- 1.0 rgb_to_cmyk_g) rgb_to_cmyk_k_float)) (- 1.0 rgb_to_cmyk_k_float))) (set! rgb_to_cmyk_y_float (/ (* 100.0 (- (- 1.0 rgb_to_cmyk_b) rgb_to_cmyk_k_float)) (- 1.0 rgb_to_cmyk_k_float))) (set! rgb_to_cmyk_k_percent (* 100.0 rgb_to_cmyk_k_float)) (set! rgb_to_cmyk_c (round_int rgb_to_cmyk_c_float)) (set! rgb_to_cmyk_m (round_int rgb_to_cmyk_m_float)) (set! rgb_to_cmyk_y (round_int rgb_to_cmyk_y_float)) (set! rgb_to_cmyk_k (round_int rgb_to_cmyk_k_percent)) (throw (ex-info "return" {:v [rgb_to_cmyk_c rgb_to_cmyk_m rgb_to_cmyk_y rgb_to_cmyk_k]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (rgb_to_cmyk 255 255 255))
      (println (rgb_to_cmyk 128 128 128))
      (println (rgb_to_cmyk 0 0 0))
      (println (rgb_to_cmyk 255 0 0))
      (println (rgb_to_cmyk 0 255 0))
      (println (rgb_to_cmyk 0 0 255))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
