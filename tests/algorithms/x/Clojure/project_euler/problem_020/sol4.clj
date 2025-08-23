(ns main (:refer-clojure :exclude [factorial_digit_sum main]))

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

(declare factorial_digit_sum main)

(declare _read_file)

(def ^:dynamic factorial_digit_sum_factorial nil)

(def ^:dynamic factorial_digit_sum_i nil)

(def ^:dynamic factorial_digit_sum_j nil)

(def ^:dynamic factorial_digit_sum_result nil)

(def ^:dynamic factorial_digit_sum_s nil)

(def ^:dynamic main_n nil)

(defn factorial_digit_sum [factorial_digit_sum_num]
  (binding [factorial_digit_sum_factorial nil factorial_digit_sum_i nil factorial_digit_sum_j nil factorial_digit_sum_result nil factorial_digit_sum_s nil] (try (do (set! factorial_digit_sum_factorial 1) (set! factorial_digit_sum_i 1) (while (<= factorial_digit_sum_i factorial_digit_sum_num) (do (set! factorial_digit_sum_factorial (* factorial_digit_sum_factorial factorial_digit_sum_i)) (set! factorial_digit_sum_i (+ factorial_digit_sum_i 1)))) (set! factorial_digit_sum_s (mochi_str factorial_digit_sum_factorial)) (set! factorial_digit_sum_result 0) (set! factorial_digit_sum_j 0) (while (< factorial_digit_sum_j (count factorial_digit_sum_s)) (do (set! factorial_digit_sum_result (+ factorial_digit_sum_result (long (nth factorial_digit_sum_s factorial_digit_sum_j)))) (set! factorial_digit_sum_j (+ factorial_digit_sum_j 1)))) (throw (ex-info "return" {:v factorial_digit_sum_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_n nil] (do (println "Enter the Number: ") (set! main_n (toi (read-line))) (println (mochi_str (factorial_digit_sum main_n))))))

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
