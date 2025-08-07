(ns main (:refer-clojure :exclude [pow2 gray_code]))

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

(declare pow2 gray_code)

(def ^:dynamic gray_code_add_val nil)

(def ^:dynamic gray_code_i nil)

(def ^:dynamic gray_code_j nil)

(def ^:dynamic gray_code_prev nil)

(def ^:dynamic gray_code_res nil)

(def ^:dynamic pow2_i nil)

(def ^:dynamic pow2_result nil)

(defn pow2 [pow2_exp]
  (binding [pow2_i nil pow2_result nil] (try (do (set! pow2_result 1) (set! pow2_i 0) (while (< pow2_i pow2_exp) (do (set! pow2_result (* pow2_result 2)) (set! pow2_i (+ pow2_i 1)))) (throw (ex-info "return" {:v pow2_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn gray_code [gray_code_bit_count]
  (binding [gray_code_add_val nil gray_code_i nil gray_code_j nil gray_code_prev nil gray_code_res nil] (try (do (when (= gray_code_bit_count 0) (throw (ex-info "return" {:v [0]}))) (set! gray_code_prev (gray_code (- gray_code_bit_count 1))) (set! gray_code_add_val (pow2 (- gray_code_bit_count 1))) (set! gray_code_res []) (set! gray_code_i 0) (while (< gray_code_i (count gray_code_prev)) (do (set! gray_code_res (conj gray_code_res (nth gray_code_prev gray_code_i))) (set! gray_code_i (+ gray_code_i 1)))) (set! gray_code_j (- (count gray_code_prev) 1)) (while (>= gray_code_j 0) (do (set! gray_code_res (conj gray_code_res (+ (nth gray_code_prev gray_code_j) gray_code_add_val))) (set! gray_code_j (- gray_code_j 1)))) (throw (ex-info "return" {:v gray_code_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_seq2 (gray_code 2))

(def ^:dynamic main_seq1 (gray_code 1))

(def ^:dynamic main_seq3 (gray_code 3))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str main_seq2))
      (println (str main_seq1))
      (println (str main_seq3))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
