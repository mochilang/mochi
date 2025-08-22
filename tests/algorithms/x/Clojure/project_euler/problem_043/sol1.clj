(ns main (:refer-clojure :exclude [is_substring_divisible remove_at digits_to_number search solution]))

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

(declare is_substring_divisible remove_at digits_to_number search solution)

(declare _read_file)

(def ^:dynamic digits_to_number_i nil)

(def ^:dynamic digits_to_number_value nil)

(def ^:dynamic is_substring_divisible_i nil)

(def ^:dynamic is_substring_divisible_idx nil)

(def ^:dynamic is_substring_divisible_p nil)

(def ^:dynamic is_substring_divisible_primes nil)

(def ^:dynamic is_substring_divisible_val nil)

(def ^:dynamic remove_at_i nil)

(def ^:dynamic remove_at_res nil)

(def ^:dynamic search_d nil)

(def ^:dynamic search_i nil)

(def ^:dynamic search_next_prefix nil)

(def ^:dynamic search_next_remaining nil)

(def ^:dynamic search_total nil)

(def ^:dynamic solution_digits nil)

(def ^:dynamic solution_i nil)

(defn is_substring_divisible [is_substring_divisible_num]
  (binding [is_substring_divisible_i nil is_substring_divisible_idx nil is_substring_divisible_p nil is_substring_divisible_primes nil is_substring_divisible_val nil] (try (do (when (not= (mod (nth is_substring_divisible_num 3) 2) 0) (throw (ex-info "return" {:v false}))) (when (not= (mod (+ (+ (nth is_substring_divisible_num 2) (nth is_substring_divisible_num 3)) (nth is_substring_divisible_num 4)) 3) 0) (throw (ex-info "return" {:v false}))) (when (not= (mod (nth is_substring_divisible_num 5) 5) 0) (throw (ex-info "return" {:v false}))) (set! is_substring_divisible_primes [7 11 13 17]) (set! is_substring_divisible_i 0) (while (< is_substring_divisible_i (count is_substring_divisible_primes)) (do (set! is_substring_divisible_p (nth is_substring_divisible_primes is_substring_divisible_i)) (set! is_substring_divisible_idx (+ is_substring_divisible_i 4)) (set! is_substring_divisible_val (+ (+ (* (nth is_substring_divisible_num is_substring_divisible_idx) 100) (* (nth is_substring_divisible_num (+ is_substring_divisible_idx 1)) 10)) (nth is_substring_divisible_num (+ is_substring_divisible_idx 2)))) (when (not= (mod is_substring_divisible_val is_substring_divisible_p) 0) (throw (ex-info "return" {:v false}))) (set! is_substring_divisible_i (+ is_substring_divisible_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_at [remove_at_xs remove_at_idx]
  (binding [remove_at_i nil remove_at_res nil] (try (do (set! remove_at_res []) (set! remove_at_i 0) (while (< remove_at_i (count remove_at_xs)) (do (when (not= remove_at_i remove_at_idx) (set! remove_at_res (conj remove_at_res (nth remove_at_xs remove_at_i)))) (set! remove_at_i (+ remove_at_i 1)))) (throw (ex-info "return" {:v remove_at_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn digits_to_number [digits_to_number_xs]
  (binding [digits_to_number_i nil digits_to_number_value nil] (try (do (set! digits_to_number_value 0) (set! digits_to_number_i 0) (while (< digits_to_number_i (count digits_to_number_xs)) (do (set! digits_to_number_value (+ (* digits_to_number_value 10) (nth digits_to_number_xs digits_to_number_i))) (set! digits_to_number_i (+ digits_to_number_i 1)))) (throw (ex-info "return" {:v digits_to_number_value}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn search [search_prefix search_remaining]
  (binding [search_d nil search_i nil search_next_prefix nil search_next_remaining nil search_total nil] (try (do (when (= (count search_remaining) 0) (do (when (is_substring_divisible search_prefix) (throw (ex-info "return" {:v (digits_to_number search_prefix)}))) (throw (ex-info "return" {:v 0})))) (set! search_total 0) (set! search_i 0) (while (< search_i (count search_remaining)) (do (set! search_d (nth search_remaining search_i)) (set! search_next_prefix (conj search_prefix search_d)) (set! search_next_remaining (remove_at search_remaining search_i)) (set! search_total (+ search_total (search search_next_prefix search_next_remaining))) (set! search_i (+ search_i 1)))) (throw (ex-info "return" {:v search_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_n]
  (binding [solution_digits nil solution_i nil] (try (do (set! solution_digits []) (set! solution_i 0) (while (< solution_i solution_n) (do (set! solution_digits (conj solution_digits solution_i)) (set! solution_i (+ solution_i 1)))) (throw (ex-info "return" {:v (search [] solution_digits)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "solution() =" (solution 10))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
