(ns main (:refer-clojure :exclude [odd_even_transposition]))

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

(declare odd_even_transposition)

(def ^:dynamic odd_even_transposition_arr nil)

(def ^:dynamic odd_even_transposition_i nil)

(def ^:dynamic odd_even_transposition_n nil)

(def ^:dynamic odd_even_transposition_pass nil)

(def ^:dynamic odd_even_transposition_tmp nil)

(defn odd_even_transposition [odd_even_transposition_arr_p]
  (binding [odd_even_transposition_arr nil odd_even_transposition_i nil odd_even_transposition_n nil odd_even_transposition_pass nil odd_even_transposition_tmp nil] (try (do (set! odd_even_transposition_arr odd_even_transposition_arr_p) (set! odd_even_transposition_n (count odd_even_transposition_arr)) (set! odd_even_transposition_pass 0) (while (< odd_even_transposition_pass odd_even_transposition_n) (do (set! odd_even_transposition_i (mod odd_even_transposition_pass 2)) (while (< odd_even_transposition_i (- odd_even_transposition_n 1)) (do (when (< (nth odd_even_transposition_arr (+ odd_even_transposition_i 1)) (nth odd_even_transposition_arr odd_even_transposition_i)) (do (set! odd_even_transposition_tmp (nth odd_even_transposition_arr odd_even_transposition_i)) (set! odd_even_transposition_arr (assoc odd_even_transposition_arr odd_even_transposition_i (nth odd_even_transposition_arr (+ odd_even_transposition_i 1)))) (set! odd_even_transposition_arr (assoc odd_even_transposition_arr (+ odd_even_transposition_i 1) odd_even_transposition_tmp)))) (set! odd_even_transposition_i (+ odd_even_transposition_i 2)))) (set! odd_even_transposition_pass (+ odd_even_transposition_pass 1)))) (throw (ex-info "return" {:v odd_even_transposition_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (odd_even_transposition [5.0 4.0 3.0 2.0 1.0])))
      (println (str (odd_even_transposition [13.0 11.0 18.0 0.0 (- 1.0)])))
      (println (str (odd_even_transposition [(- 0.1) 1.1 0.1 (- 2.9)])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
