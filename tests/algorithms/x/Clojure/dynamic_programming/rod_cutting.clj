(ns main (:refer-clojure :exclude [enforce_args bottom_up_cut_rod]))

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

(declare enforce_args bottom_up_cut_rod)

(def ^:dynamic bottom_up_cut_rod_best nil)

(def ^:dynamic bottom_up_cut_rod_candidate nil)

(def ^:dynamic bottom_up_cut_rod_i nil)

(def ^:dynamic bottom_up_cut_rod_j nil)

(def ^:dynamic bottom_up_cut_rod_length nil)

(def ^:dynamic bottom_up_cut_rod_max_rev nil)

(defn enforce_args [enforce_args_n enforce_args_prices]
  (do (when (< enforce_args_n 0) (throw (Exception. "n must be non-negative"))) (when (> enforce_args_n (count enforce_args_prices)) (throw (Exception. "price list is shorter than n")))))

(defn bottom_up_cut_rod [bottom_up_cut_rod_n bottom_up_cut_rod_prices]
  (binding [bottom_up_cut_rod_best nil bottom_up_cut_rod_candidate nil bottom_up_cut_rod_i nil bottom_up_cut_rod_j nil bottom_up_cut_rod_length nil bottom_up_cut_rod_max_rev nil] (try (do (enforce_args bottom_up_cut_rod_n bottom_up_cut_rod_prices) (set! bottom_up_cut_rod_max_rev []) (set! bottom_up_cut_rod_i 0) (while (<= bottom_up_cut_rod_i bottom_up_cut_rod_n) (do (if (= bottom_up_cut_rod_i 0) (set! bottom_up_cut_rod_max_rev (conj bottom_up_cut_rod_max_rev 0)) (set! bottom_up_cut_rod_max_rev (conj bottom_up_cut_rod_max_rev (- 2147483648)))) (set! bottom_up_cut_rod_i (+ bottom_up_cut_rod_i 1)))) (set! bottom_up_cut_rod_length 1) (while (<= bottom_up_cut_rod_length bottom_up_cut_rod_n) (do (set! bottom_up_cut_rod_best (nth bottom_up_cut_rod_max_rev bottom_up_cut_rod_length)) (set! bottom_up_cut_rod_j 1) (while (<= bottom_up_cut_rod_j bottom_up_cut_rod_length) (do (set! bottom_up_cut_rod_candidate (+ (nth bottom_up_cut_rod_prices (- bottom_up_cut_rod_j 1)) (nth bottom_up_cut_rod_max_rev (- bottom_up_cut_rod_length bottom_up_cut_rod_j)))) (when (> bottom_up_cut_rod_candidate bottom_up_cut_rod_best) (set! bottom_up_cut_rod_best bottom_up_cut_rod_candidate)) (set! bottom_up_cut_rod_j (+ bottom_up_cut_rod_j 1)))) (set! bottom_up_cut_rod_max_rev (assoc bottom_up_cut_rod_max_rev bottom_up_cut_rod_length bottom_up_cut_rod_best)) (set! bottom_up_cut_rod_length (+ bottom_up_cut_rod_length 1)))) (throw (ex-info "return" {:v (nth bottom_up_cut_rod_max_rev bottom_up_cut_rod_n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_prices [1 5 8 9 10 17 17 20 24 30])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (bottom_up_cut_rod 4 main_prices))
      (println (bottom_up_cut_rod 10 main_prices))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
