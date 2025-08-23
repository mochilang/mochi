(ns main (:refer-clojure :exclude [factorial digit_sum solution]))

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

(declare factorial digit_sum solution)

(declare _read_file)

(def ^:dynamic digit_sum_digit_str nil)

(def ^:dynamic digit_sum_i nil)

(def ^:dynamic digit_sum_s nil)

(def ^:dynamic digit_sum_total nil)

(def ^:dynamic factorial_i nil)

(def ^:dynamic factorial_result nil)

(defn factorial [factorial_n]
  (binding [factorial_i nil factorial_result nil] (try (do (set! factorial_result 1) (set! factorial_i 2) (while (<= factorial_i factorial_n) (do (set! factorial_result (* factorial_result factorial_i)) (set! factorial_i (+ factorial_i 1)))) (throw (ex-info "return" {:v factorial_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn digit_sum [digit_sum_n]
  (binding [digit_sum_digit_str nil digit_sum_i nil digit_sum_s nil digit_sum_total nil] (try (do (set! digit_sum_s (mochi_str digit_sum_n)) (set! digit_sum_total 0) (set! digit_sum_i 0) (while (< digit_sum_i (count digit_sum_s)) (do (set! digit_sum_digit_str (subs digit_sum_s digit_sum_i (min (+ digit_sum_i 1) (count digit_sum_s)))) (set! digit_sum_total (+ digit_sum_total (toi digit_sum_digit_str))) (set! digit_sum_i (+ digit_sum_i 1)))) (throw (ex-info "return" {:v digit_sum_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_num]
  (try (throw (ex-info "return" {:v (digit_sum (factorial solution_num))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mochi_str (solution 100)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
