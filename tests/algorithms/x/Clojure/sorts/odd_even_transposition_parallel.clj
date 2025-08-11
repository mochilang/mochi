(ns main (:refer-clojure :exclude [odd_even_transposition main]))

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare odd_even_transposition main)

(def ^:dynamic main_data nil)

(def ^:dynamic main_sorted nil)

(def ^:dynamic odd_even_transposition_arr nil)

(def ^:dynamic odd_even_transposition_i nil)

(def ^:dynamic odd_even_transposition_n nil)

(def ^:dynamic odd_even_transposition_phase nil)

(def ^:dynamic odd_even_transposition_start nil)

(def ^:dynamic odd_even_transposition_tmp nil)

(defn odd_even_transposition [odd_even_transposition_xs]
  (binding [odd_even_transposition_arr nil odd_even_transposition_i nil odd_even_transposition_n nil odd_even_transposition_phase nil odd_even_transposition_start nil odd_even_transposition_tmp nil] (try (do (set! odd_even_transposition_arr odd_even_transposition_xs) (set! odd_even_transposition_n (count odd_even_transposition_arr)) (set! odd_even_transposition_phase 0) (while (< odd_even_transposition_phase odd_even_transposition_n) (do (set! odd_even_transposition_start (if (= (mod odd_even_transposition_phase 2) 0) 0 1)) (set! odd_even_transposition_i odd_even_transposition_start) (while (< (+ odd_even_transposition_i 1) odd_even_transposition_n) (do (when (> (nth odd_even_transposition_arr odd_even_transposition_i) (nth odd_even_transposition_arr (+ odd_even_transposition_i 1))) (do (set! odd_even_transposition_tmp (nth odd_even_transposition_arr odd_even_transposition_i)) (set! odd_even_transposition_arr (assoc odd_even_transposition_arr odd_even_transposition_i (nth odd_even_transposition_arr (+ odd_even_transposition_i 1)))) (set! odd_even_transposition_arr (assoc odd_even_transposition_arr (+ odd_even_transposition_i 1) odd_even_transposition_tmp)))) (set! odd_even_transposition_i (+ odd_even_transposition_i 2)))) (set! odd_even_transposition_phase (+ odd_even_transposition_phase 1)))) (throw (ex-info "return" {:v odd_even_transposition_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_data nil main_sorted nil] (do (set! main_data [10 9 8 7 6 5 4 3 2 1]) (println "Initial List") (println (str main_data)) (set! main_sorted (odd_even_transposition main_data)) (println "Sorted List") (println (str main_sorted)))))

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
