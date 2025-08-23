(ns main (:refer-clojure :exclude [is_palindrome_str to_binary solution]))

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

(declare is_palindrome_str to_binary solution)

(declare _read_file)

(def ^:dynamic is_palindrome_str_i nil)

(def ^:dynamic is_palindrome_str_j nil)

(def ^:dynamic solution_bin nil)

(def ^:dynamic solution_dec nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_total nil)

(def ^:dynamic to_binary_res nil)

(def ^:dynamic to_binary_x nil)

(defn is_palindrome_str [is_palindrome_str_s]
  (binding [is_palindrome_str_i nil is_palindrome_str_j nil] (try (do (set! is_palindrome_str_i 0) (set! is_palindrome_str_j (- (count is_palindrome_str_s) 1)) (while (< is_palindrome_str_i is_palindrome_str_j) (do (when (not= (subs is_palindrome_str_s is_palindrome_str_i (min (+ is_palindrome_str_i 1) (count is_palindrome_str_s))) (subs is_palindrome_str_s is_palindrome_str_j (min (+ is_palindrome_str_j 1) (count is_palindrome_str_s)))) (throw (ex-info "return" {:v false}))) (set! is_palindrome_str_i (+ is_palindrome_str_i 1)) (set! is_palindrome_str_j (- is_palindrome_str_j 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_binary [to_binary_n]
  (binding [to_binary_res nil to_binary_x nil] (try (do (when (= to_binary_n 0) (throw (ex-info "return" {:v "0"}))) (set! to_binary_res "") (set! to_binary_x to_binary_n) (while (> to_binary_x 0) (do (set! to_binary_res (str (mochi_str (mod to_binary_x 2)) to_binary_res)) (set! to_binary_x (quot to_binary_x 2)))) (throw (ex-info "return" {:v to_binary_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_n]
  (binding [solution_bin nil solution_dec nil solution_i nil solution_total nil] (try (do (set! solution_total 0) (set! solution_i 1) (while (< solution_i solution_n) (do (set! solution_dec (mochi_str solution_i)) (set! solution_bin (to_binary solution_i)) (when (and (is_palindrome_str solution_dec) (is_palindrome_str solution_bin)) (set! solution_total (+ solution_total solution_i))) (set! solution_i (+ solution_i 1)))) (throw (ex-info "return" {:v solution_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (solution 1000000))
      (println (solution 500000))
      (println (solution 100000))
      (println (solution 1000))
      (println (solution 100))
      (println (solution 10))
      (println (solution 2))
      (println (solution 1))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
