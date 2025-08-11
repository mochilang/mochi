(ns main (:refer-clojure :exclude [damerau_levenshtein_distance]))

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

(declare damerau_levenshtein_distance)

(def ^:dynamic damerau_levenshtein_distance_cost nil)

(def ^:dynamic damerau_levenshtein_distance_dp_matrix nil)

(def ^:dynamic damerau_levenshtein_distance_first_char nil)

(def ^:dynamic damerau_levenshtein_distance_first_row nil)

(def ^:dynamic damerau_levenshtein_distance_insertion nil)

(def ^:dynamic damerau_levenshtein_distance_len1 nil)

(def ^:dynamic damerau_levenshtein_distance_len2 nil)

(def ^:dynamic damerau_levenshtein_distance_row nil)

(def ^:dynamic damerau_levenshtein_distance_second_char nil)

(def ^:dynamic damerau_levenshtein_distance_substitution nil)

(def ^:dynamic damerau_levenshtein_distance_transposition nil)

(def ^:dynamic damerau_levenshtein_distance_value nil)

(defn damerau_levenshtein_distance [damerau_levenshtein_distance_first_string damerau_levenshtein_distance_second_string]
  (binding [damerau_levenshtein_distance_cost nil damerau_levenshtein_distance_dp_matrix nil damerau_levenshtein_distance_first_char nil damerau_levenshtein_distance_first_row nil damerau_levenshtein_distance_insertion nil damerau_levenshtein_distance_len1 nil damerau_levenshtein_distance_len2 nil damerau_levenshtein_distance_row nil damerau_levenshtein_distance_second_char nil damerau_levenshtein_distance_substitution nil damerau_levenshtein_distance_transposition nil damerau_levenshtein_distance_value nil] (try (do (set! damerau_levenshtein_distance_len1 (count damerau_levenshtein_distance_first_string)) (set! damerau_levenshtein_distance_len2 (count damerau_levenshtein_distance_second_string)) (set! damerau_levenshtein_distance_dp_matrix []) (dotimes [_ (+ damerau_levenshtein_distance_len1 1)] (do (set! damerau_levenshtein_distance_row []) (dotimes [_2 (+ damerau_levenshtein_distance_len2 1)] (set! damerau_levenshtein_distance_row (conj damerau_levenshtein_distance_row 0))) (set! damerau_levenshtein_distance_dp_matrix (conj damerau_levenshtein_distance_dp_matrix damerau_levenshtein_distance_row)))) (dotimes [i (+ damerau_levenshtein_distance_len1 1)] (do (set! damerau_levenshtein_distance_row (nth damerau_levenshtein_distance_dp_matrix i)) (set! damerau_levenshtein_distance_row (assoc damerau_levenshtein_distance_row 0 i)) (set! damerau_levenshtein_distance_dp_matrix (assoc damerau_levenshtein_distance_dp_matrix i damerau_levenshtein_distance_row)))) (set! damerau_levenshtein_distance_first_row (nth damerau_levenshtein_distance_dp_matrix 0)) (dotimes [j (+ damerau_levenshtein_distance_len2 1)] (set! damerau_levenshtein_distance_first_row (assoc damerau_levenshtein_distance_first_row j j))) (set! damerau_levenshtein_distance_dp_matrix (assoc damerau_levenshtein_distance_dp_matrix 0 damerau_levenshtein_distance_first_row)) (doseq [i (range 1 (+ damerau_levenshtein_distance_len1 1))] (do (set! damerau_levenshtein_distance_row (nth damerau_levenshtein_distance_dp_matrix i)) (set! damerau_levenshtein_distance_first_char (subs damerau_levenshtein_distance_first_string (- i 1) (min i (count damerau_levenshtein_distance_first_string)))) (doseq [j (range 1 (+ damerau_levenshtein_distance_len2 1))] (do (set! damerau_levenshtein_distance_second_char (subs damerau_levenshtein_distance_second_string (- j 1) (min j (count damerau_levenshtein_distance_second_string)))) (set! damerau_levenshtein_distance_cost (if (= damerau_levenshtein_distance_first_char damerau_levenshtein_distance_second_char) 0 1)) (set! damerau_levenshtein_distance_value (+ (nth (nth damerau_levenshtein_distance_dp_matrix (- i 1)) j) 1)) (set! damerau_levenshtein_distance_insertion (+ (nth damerau_levenshtein_distance_row (- j 1)) 1)) (when (< damerau_levenshtein_distance_insertion damerau_levenshtein_distance_value) (set! damerau_levenshtein_distance_value damerau_levenshtein_distance_insertion)) (set! damerau_levenshtein_distance_substitution (+ (nth (nth damerau_levenshtein_distance_dp_matrix (- i 1)) (- j 1)) damerau_levenshtein_distance_cost)) (when (< damerau_levenshtein_distance_substitution damerau_levenshtein_distance_value) (set! damerau_levenshtein_distance_value damerau_levenshtein_distance_substitution)) (set! damerau_levenshtein_distance_row (assoc damerau_levenshtein_distance_row j damerau_levenshtein_distance_value)) (when (and (and (and (> i 1) (> j 1)) (= (subs damerau_levenshtein_distance_first_string (- i 1) (min i (count damerau_levenshtein_distance_first_string))) (subs damerau_levenshtein_distance_second_string (- j 2) (min (- j 1) (count damerau_levenshtein_distance_second_string))))) (= (subs damerau_levenshtein_distance_first_string (- i 2) (min (- i 1) (count damerau_levenshtein_distance_first_string))) (subs damerau_levenshtein_distance_second_string (- j 1) (min j (count damerau_levenshtein_distance_second_string))))) (do (set! damerau_levenshtein_distance_transposition (+ (nth (nth damerau_levenshtein_distance_dp_matrix (- i 2)) (- j 2)) damerau_levenshtein_distance_cost)) (when (< damerau_levenshtein_distance_transposition (nth damerau_levenshtein_distance_row j)) (set! damerau_levenshtein_distance_row (assoc damerau_levenshtein_distance_row j damerau_levenshtein_distance_transposition))))))) (set! damerau_levenshtein_distance_dp_matrix (assoc damerau_levenshtein_distance_dp_matrix i damerau_levenshtein_distance_row)))) (throw (ex-info "return" {:v (nth (nth damerau_levenshtein_distance_dp_matrix damerau_levenshtein_distance_len1) damerau_levenshtein_distance_len2)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (damerau_levenshtein_distance "cat" "cut")))
      (println (str (damerau_levenshtein_distance "kitten" "sitting")))
      (println (str (damerau_levenshtein_distance "hello" "world")))
      (println (str (damerau_levenshtein_distance "book" "back")))
      (println (str (damerau_levenshtein_distance "container" "containment")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
