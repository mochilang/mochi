(ns main (:refer-clojure :exclude [calculation_span print_array]))

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

(declare calculation_span print_array)

(def ^:dynamic calculation_span_n nil)

(def ^:dynamic calculation_span_s nil)

(def ^:dynamic calculation_span_span nil)

(def ^:dynamic calculation_span_st nil)

(defn calculation_span [calculation_span_price]
  (binding [calculation_span_n nil calculation_span_s nil calculation_span_span nil calculation_span_st nil] (try (do (set! calculation_span_n (count calculation_span_price)) (set! calculation_span_st []) (set! calculation_span_span []) (set! calculation_span_st (conj calculation_span_st 0)) (set! calculation_span_span (conj calculation_span_span 1)) (doseq [i (range 1 calculation_span_n)] (do (while (and (> (count calculation_span_st) 0) (<= (nth calculation_span_price (nth calculation_span_st (- (count calculation_span_st) 1))) (nth calculation_span_price i))) (set! calculation_span_st (subvec calculation_span_st 0 (min (- (count calculation_span_st) 1) (count calculation_span_st))))) (set! calculation_span_s (if (<= (count calculation_span_st) 0) (+ i 1) (- i (nth calculation_span_st (- (count calculation_span_st) 1))))) (set! calculation_span_span (conj calculation_span_span calculation_span_s)) (set! calculation_span_st (conj calculation_span_st i)))) (throw (ex-info "return" {:v calculation_span_span}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_array [print_array_arr]
  (dotimes [i (count print_array_arr)] (println (nth print_array_arr i))))

(def ^:dynamic main_price [10 4 5 90 120 80])

(def ^:dynamic main_spans (calculation_span main_price))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (print_array main_spans)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
