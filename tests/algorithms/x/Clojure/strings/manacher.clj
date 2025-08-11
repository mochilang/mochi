(ns main (:refer-clojure :exclude [palindromic_string main]))

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

(declare palindromic_string main)

(def ^:dynamic palindromic_string_ch nil)

(def ^:dynamic palindromic_string_diff nil)

(def ^:dynamic palindromic_string_i nil)

(def ^:dynamic palindromic_string_idx nil)

(def ^:dynamic palindromic_string_j nil)

(def ^:dynamic palindromic_string_k nil)

(def ^:dynamic palindromic_string_left nil)

(def ^:dynamic palindromic_string_length nil)

(def ^:dynamic palindromic_string_m nil)

(def ^:dynamic palindromic_string_max_length nil)

(def ^:dynamic palindromic_string_mirror nil)

(def ^:dynamic palindromic_string_n nil)

(def ^:dynamic palindromic_string_new_input_string nil)

(def ^:dynamic palindromic_string_output_string nil)

(def ^:dynamic palindromic_string_right nil)

(def ^:dynamic palindromic_string_s nil)

(def ^:dynamic palindromic_string_start nil)

(defn palindromic_string [palindromic_string_input_string]
  (binding [palindromic_string_ch nil palindromic_string_diff nil palindromic_string_i nil palindromic_string_idx nil palindromic_string_j nil palindromic_string_k nil palindromic_string_left nil palindromic_string_length nil palindromic_string_m nil palindromic_string_max_length nil palindromic_string_mirror nil palindromic_string_n nil palindromic_string_new_input_string nil palindromic_string_output_string nil palindromic_string_right nil palindromic_string_s nil palindromic_string_start nil] (try (do (set! palindromic_string_max_length 0) (set! palindromic_string_new_input_string "") (set! palindromic_string_output_string "") (set! palindromic_string_n (count palindromic_string_input_string)) (set! palindromic_string_i 0) (while (< palindromic_string_i (- palindromic_string_n 1)) (do (set! palindromic_string_new_input_string (str (str palindromic_string_new_input_string (subs palindromic_string_input_string palindromic_string_i (min (+ palindromic_string_i 1) (count palindromic_string_input_string)))) "|")) (set! palindromic_string_i (+ palindromic_string_i 1)))) (set! palindromic_string_new_input_string (str palindromic_string_new_input_string (subs palindromic_string_input_string (- palindromic_string_n 1) (min palindromic_string_n (count palindromic_string_input_string))))) (set! palindromic_string_left 0) (set! palindromic_string_right 0) (set! palindromic_string_length []) (set! palindromic_string_i 0) (set! palindromic_string_m (count palindromic_string_new_input_string)) (while (< palindromic_string_i palindromic_string_m) (do (set! palindromic_string_length (conj palindromic_string_length 1)) (set! palindromic_string_i (+ palindromic_string_i 1)))) (set! palindromic_string_start 0) (set! palindromic_string_j 0) (while (< palindromic_string_j palindromic_string_m) (do (set! palindromic_string_k 1) (when (<= palindromic_string_j palindromic_string_right) (do (set! palindromic_string_mirror (- (+ palindromic_string_left palindromic_string_right) palindromic_string_j)) (set! palindromic_string_k (/ (nth palindromic_string_length palindromic_string_mirror) 2)) (set! palindromic_string_diff (+ (- palindromic_string_right palindromic_string_j) 1)) (when (< palindromic_string_diff palindromic_string_k) (set! palindromic_string_k palindromic_string_diff)) (when (< palindromic_string_k 1) (set! palindromic_string_k 1)))) (while (and (and (>= (- palindromic_string_j palindromic_string_k) 0) (< (+ palindromic_string_j palindromic_string_k) palindromic_string_m)) (= (subs palindromic_string_new_input_string (+ palindromic_string_j palindromic_string_k) (min (+ (+ palindromic_string_j palindromic_string_k) 1) (count palindromic_string_new_input_string))) (subs palindromic_string_new_input_string (- palindromic_string_j palindromic_string_k) (min (+ (- palindromic_string_j palindromic_string_k) 1) (count palindromic_string_new_input_string))))) (set! palindromic_string_k (+ palindromic_string_k 1))) (set! palindromic_string_length (assoc palindromic_string_length palindromic_string_j (- (* 2 palindromic_string_k) 1))) (when (> (- (+ palindromic_string_j palindromic_string_k) 1) palindromic_string_right) (do (set! palindromic_string_left (+ (- palindromic_string_j palindromic_string_k) 1)) (set! palindromic_string_right (- (+ palindromic_string_j palindromic_string_k) 1)))) (when (> (nth palindromic_string_length palindromic_string_j) palindromic_string_max_length) (do (set! palindromic_string_max_length (nth palindromic_string_length palindromic_string_j)) (set! palindromic_string_start palindromic_string_j))) (set! palindromic_string_j (+ palindromic_string_j 1)))) (set! palindromic_string_s (subs palindromic_string_new_input_string (- palindromic_string_start (/ palindromic_string_max_length 2)) (min (+ (+ palindromic_string_start (/ palindromic_string_max_length 2)) 1) (count palindromic_string_new_input_string)))) (set! palindromic_string_idx 0) (while (< palindromic_string_idx (count palindromic_string_s)) (do (set! palindromic_string_ch (subs palindromic_string_s palindromic_string_idx (min (+ palindromic_string_idx 1) (count palindromic_string_s)))) (when (not= palindromic_string_ch "|") (set! palindromic_string_output_string (str palindromic_string_output_string palindromic_string_ch))) (set! palindromic_string_idx (+ palindromic_string_idx 1)))) (throw (ex-info "return" {:v palindromic_string_output_string}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (palindromic_string "abbbaba")) (println (palindromic_string "ababa"))))

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
