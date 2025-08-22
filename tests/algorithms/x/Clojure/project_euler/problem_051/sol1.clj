(ns main (:refer-clojure :exclude [parse_int digit_replacements is_prime solution]))

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
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare parse_int digit_replacements is_prime solution)

(declare _read_file)

(def ^:dynamic count_v nil)

(def ^:dynamic digit_replacements_c nil)

(def ^:dynamic digit_replacements_counts nil)

(def ^:dynamic digit_replacements_d nil)

(def ^:dynamic digit_replacements_digit nil)

(def ^:dynamic digit_replacements_digits nil)

(def ^:dynamic digit_replacements_family nil)

(def ^:dynamic digit_replacements_i nil)

(def ^:dynamic digit_replacements_j nil)

(def ^:dynamic digit_replacements_new_str nil)

(def ^:dynamic digit_replacements_num_str nil)

(def ^:dynamic digit_replacements_repl nil)

(def ^:dynamic digit_replacements_result nil)

(def ^:dynamic first_v nil)

(def ^:dynamic is_prime_i nil)

(def ^:dynamic parse_int_i nil)

(def ^:dynamic parse_int_value nil)

(def ^:dynamic solution_candidate nil)

(def ^:dynamic solution_family nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_min_prime nil)

(def ^:dynamic solution_num nil)

(def ^:dynamic solution_r nil)

(def ^:dynamic solution_reps nil)

(defn parse_int [parse_int_s]
  (binding [parse_int_i nil parse_int_value nil] (try (do (set! parse_int_value 0) (set! parse_int_i 0) (while (< parse_int_i (count parse_int_s)) (do (set! parse_int_value (+ (* parse_int_value 10) (Long/parseLong (subs parse_int_s parse_int_i (+ parse_int_i 1))))) (set! parse_int_i (+ parse_int_i 1)))) (throw (ex-info "return" {:v parse_int_value}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn digit_replacements [digit_replacements_number]
  (binding [digit_replacements_c nil digit_replacements_counts nil digit_replacements_d nil digit_replacements_digit nil digit_replacements_digits nil digit_replacements_family nil digit_replacements_i nil digit_replacements_j nil digit_replacements_new_str nil digit_replacements_num_str nil digit_replacements_repl nil digit_replacements_result nil] (try (do (set! digit_replacements_num_str (mochi_str digit_replacements_number)) (set! digit_replacements_counts [0 0 0 0 0 0 0 0 0 0]) (set! digit_replacements_i 0) (while (< digit_replacements_i (count digit_replacements_num_str)) (do (set! digit_replacements_d (long (nth digit_replacements_num_str digit_replacements_i))) (set! digit_replacements_counts (assoc digit_replacements_counts digit_replacements_d (+ (nth digit_replacements_counts digit_replacements_d) 1))) (set! digit_replacements_i (+ digit_replacements_i 1)))) (set! digit_replacements_result []) (set! digit_replacements_digits "0123456789") (set! digit_replacements_digit 0) (while (< digit_replacements_digit 10) (do (when (> (nth digit_replacements_counts digit_replacements_digit) 1) (do (set! digit_replacements_family []) (set! digit_replacements_repl 0) (while (< digit_replacements_repl 10) (do (set! digit_replacements_new_str "") (set! digit_replacements_j 0) (while (< digit_replacements_j (count digit_replacements_num_str)) (do (set! digit_replacements_c (nth digit_replacements_num_str digit_replacements_j)) (if (= digit_replacements_c (subs digit_replacements_digits digit_replacements_digit (+ digit_replacements_digit 1))) (set! digit_replacements_new_str (str digit_replacements_new_str (subs digit_replacements_digits digit_replacements_repl (+ digit_replacements_repl 1)))) (set! digit_replacements_new_str (str digit_replacements_new_str digit_replacements_c))) (set! digit_replacements_j (+ digit_replacements_j 1)))) (set! digit_replacements_family (conj digit_replacements_family (parse_int digit_replacements_new_str))) (set! digit_replacements_repl (+ digit_replacements_repl 1)))) (set! digit_replacements_result (conj digit_replacements_result digit_replacements_family)))) (set! digit_replacements_digit (+ digit_replacements_digit 1)))) (throw (ex-info "return" {:v digit_replacements_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_prime [is_prime_num]
  (binding [is_prime_i nil] (try (do (when (< is_prime_num 2) (throw (ex-info "return" {:v false}))) (when (= (mod is_prime_num 2) 0) (throw (ex-info "return" {:v (= is_prime_num 2)}))) (set! is_prime_i 3) (while (<= (* is_prime_i is_prime_i) is_prime_num) (do (when (= (mod is_prime_num is_prime_i) 0) (throw (ex-info "return" {:v false}))) (set! is_prime_i (+ is_prime_i 2)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_family_length]
  (binding [count_v nil first_v nil solution_candidate nil solution_family nil solution_i nil solution_min_prime nil solution_num nil solution_r nil solution_reps nil] (try (do (set! solution_candidate 121313) (when (not (is_prime solution_candidate)) (throw (ex-info "return" {:v (- 1)}))) (set! solution_reps (digit_replacements solution_candidate)) (set! solution_r 0) (while (< solution_r (count solution_reps)) (do (set! solution_family (nth solution_reps solution_r)) (set! count_v 0) (set! solution_min_prime 0) (set! first_v true) (set! solution_i 0) (while (< solution_i (count solution_family)) (do (set! solution_num (nth solution_family solution_i)) (when (is_prime solution_num) (do (if first_v (do (set! solution_min_prime solution_num) (set! first_v false)) (when (< solution_num solution_min_prime) (set! solution_min_prime solution_num))) (set! count_v (+ count_v 1)))) (set! solution_i (+ solution_i 1)))) (when (= count_v solution_family_length) (throw (ex-info "return" {:v solution_min_prime}))) (set! solution_r (+ solution_r 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mochi_str (solution 8)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
