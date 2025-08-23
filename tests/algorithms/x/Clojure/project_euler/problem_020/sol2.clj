(ns main (:refer-clojure :exclude [factorial digit_sum solution main]))

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

(declare factorial digit_sum solution main)

(declare _read_file)

(def ^:dynamic digit_sum_m nil)

(def ^:dynamic digit_sum_total nil)

(def ^:dynamic factorial_i nil)

(def ^:dynamic factorial_result nil)

(def ^:dynamic solution_f nil)

(defn factorial [factorial_n]
  (binding [factorial_i nil factorial_result nil] (try (do (set! factorial_result 1) (set! factorial_i 2) (while (<= factorial_i factorial_n) (do (set! factorial_result (* factorial_result factorial_i)) (set! factorial_i (+ factorial_i 1)))) (throw (ex-info "return" {:v factorial_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn digit_sum [digit_sum_n]
  (binding [digit_sum_m nil digit_sum_total nil] (try (do (set! digit_sum_total 0) (set! digit_sum_m digit_sum_n) (while (> digit_sum_m 0) (do (set! digit_sum_total (+ digit_sum_total (mod digit_sum_m 10))) (set! digit_sum_m (quot digit_sum_m 10)))) (throw (ex-info "return" {:v digit_sum_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_num]
  (binding [solution_f nil] (try (do (set! solution_f (factorial solution_num)) (throw (ex-info "return" {:v (digit_sum solution_f)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (solution 100)) (println (solution 50)) (println (solution 10)) (println (solution 5)) (println (solution 3)) (println (solution 2)) (println (solution 1))))

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
