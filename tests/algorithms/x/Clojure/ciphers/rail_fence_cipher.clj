(ns main (:refer-clojure :exclude [encrypt decrypt bruteforce]))

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

(declare encrypt decrypt bruteforce)

(def ^:dynamic bruteforce_key_guess nil)

(def ^:dynamic bruteforce_results nil)

(def ^:dynamic decrypt_alt nil)

(def ^:dynamic decrypt_counter nil)

(def ^:dynamic decrypt_counts nil)

(def ^:dynamic decrypt_grid nil)

(def ^:dynamic decrypt_i nil)

(def ^:dynamic decrypt_indices nil)

(def ^:dynamic decrypt_j nil)

(def ^:dynamic decrypt_length nil)

(def ^:dynamic decrypt_lowest nil)

(def ^:dynamic decrypt_num nil)

(def ^:dynamic decrypt_output nil)

(def ^:dynamic decrypt_pos nil)

(def ^:dynamic decrypt_row nil)

(def ^:dynamic decrypt_slice nil)

(def ^:dynamic encrypt_alt nil)

(def ^:dynamic encrypt_i nil)

(def ^:dynamic encrypt_j nil)

(def ^:dynamic encrypt_lowest nil)

(def ^:dynamic encrypt_num nil)

(def ^:dynamic encrypt_output nil)

(def ^:dynamic encrypt_position nil)

(def ^:dynamic encrypt_row nil)

(def ^:dynamic encrypt_temp_grid nil)

(defn encrypt [encrypt_input_string encrypt_key]
  (binding [encrypt_alt nil encrypt_i nil encrypt_j nil encrypt_lowest nil encrypt_num nil encrypt_output nil encrypt_position nil encrypt_row nil encrypt_temp_grid nil] (try (do (when (<= encrypt_key 0) (throw (Exception. "Height of grid can't be 0 or negative"))) (when (or (= encrypt_key 1) (<= (count encrypt_input_string) encrypt_key)) (throw (ex-info "return" {:v encrypt_input_string}))) (set! encrypt_lowest (- encrypt_key 1)) (set! encrypt_temp_grid []) (set! encrypt_i 0) (while (< encrypt_i encrypt_key) (do (set! encrypt_temp_grid (conj encrypt_temp_grid [])) (set! encrypt_i (+ encrypt_i 1)))) (set! encrypt_position 0) (while (< encrypt_position (count encrypt_input_string)) (do (set! encrypt_num (mod encrypt_position (* encrypt_lowest 2))) (set! encrypt_alt (- (* encrypt_lowest 2) encrypt_num)) (when (> encrypt_num encrypt_alt) (set! encrypt_num encrypt_alt)) (set! encrypt_row (nth encrypt_temp_grid encrypt_num)) (set! encrypt_row (conj encrypt_row (subs encrypt_input_string encrypt_position (min (+ encrypt_position 1) (count encrypt_input_string))))) (set! encrypt_temp_grid (assoc encrypt_temp_grid encrypt_num encrypt_row)) (set! encrypt_position (+ encrypt_position 1)))) (set! encrypt_output "") (set! encrypt_i 0) (while (< encrypt_i encrypt_key) (do (set! encrypt_row (nth encrypt_temp_grid encrypt_i)) (set! encrypt_j 0) (while (< encrypt_j (count encrypt_row)) (do (set! encrypt_output (str encrypt_output (nth encrypt_row encrypt_j))) (set! encrypt_j (+ encrypt_j 1)))) (set! encrypt_i (+ encrypt_i 1)))) (throw (ex-info "return" {:v encrypt_output}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decrypt [decrypt_input_string decrypt_key]
  (binding [decrypt_alt nil decrypt_counter nil decrypt_counts nil decrypt_grid nil decrypt_i nil decrypt_indices nil decrypt_j nil decrypt_length nil decrypt_lowest nil decrypt_num nil decrypt_output nil decrypt_pos nil decrypt_row nil decrypt_slice nil] (try (do (when (<= decrypt_key 0) (throw (Exception. "Height of grid can't be 0 or negative"))) (when (= decrypt_key 1) (throw (ex-info "return" {:v decrypt_input_string}))) (set! decrypt_lowest (- decrypt_key 1)) (set! decrypt_counts []) (set! decrypt_i 0) (while (< decrypt_i decrypt_key) (do (set! decrypt_counts (conj decrypt_counts 0)) (set! decrypt_i (+ decrypt_i 1)))) (set! decrypt_pos 0) (while (< decrypt_pos (count decrypt_input_string)) (do (set! decrypt_num (mod decrypt_pos (* decrypt_lowest 2))) (set! decrypt_alt (- (* decrypt_lowest 2) decrypt_num)) (when (> decrypt_num decrypt_alt) (set! decrypt_num decrypt_alt)) (set! decrypt_counts (assoc decrypt_counts decrypt_num (+ (nth decrypt_counts decrypt_num) 1))) (set! decrypt_pos (+ decrypt_pos 1)))) (set! decrypt_grid []) (set! decrypt_counter 0) (set! decrypt_i 0) (while (< decrypt_i decrypt_key) (do (set! decrypt_length (nth decrypt_counts decrypt_i)) (set! decrypt_slice (subs decrypt_input_string decrypt_counter (min (+ decrypt_counter decrypt_length) (count decrypt_input_string)))) (set! decrypt_row []) (set! decrypt_j 0) (while (< decrypt_j (count decrypt_slice)) (do (set! decrypt_row (conj decrypt_row (nth decrypt_slice decrypt_j))) (set! decrypt_j (+ decrypt_j 1)))) (set! decrypt_grid (conj decrypt_grid decrypt_row)) (set! decrypt_counter (+ decrypt_counter decrypt_length)) (set! decrypt_i (+ decrypt_i 1)))) (set! decrypt_indices []) (set! decrypt_i 0) (while (< decrypt_i decrypt_key) (do (set! decrypt_indices (conj decrypt_indices 0)) (set! decrypt_i (+ decrypt_i 1)))) (set! decrypt_output "") (set! decrypt_pos 0) (while (< decrypt_pos (count decrypt_input_string)) (do (set! decrypt_num (mod decrypt_pos (* decrypt_lowest 2))) (set! decrypt_alt (- (* decrypt_lowest 2) decrypt_num)) (when (> decrypt_num decrypt_alt) (set! decrypt_num decrypt_alt)) (set! decrypt_output (str decrypt_output (nth (nth decrypt_grid decrypt_num) (nth decrypt_indices decrypt_num)))) (set! decrypt_indices (assoc decrypt_indices decrypt_num (+ (nth decrypt_indices decrypt_num) 1))) (set! decrypt_pos (+ decrypt_pos 1)))) (throw (ex-info "return" {:v decrypt_output}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bruteforce [bruteforce_input_string]
  (binding [bruteforce_key_guess nil bruteforce_results nil] (try (do (set! bruteforce_results {}) (set! bruteforce_key_guess 1) (while (< bruteforce_key_guess (count bruteforce_input_string)) (do (set! bruteforce_results (assoc bruteforce_results bruteforce_key_guess (decrypt bruteforce_input_string bruteforce_key_guess))) (set! bruteforce_key_guess (+ bruteforce_key_guess 1)))) (throw (ex-info "return" {:v bruteforce_results}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_bf (bruteforce "HWe olordll"))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (encrypt "Hello World" 4))
      (println (decrypt "HWe olordll" 4))
      (println (nth main_bf 4))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
