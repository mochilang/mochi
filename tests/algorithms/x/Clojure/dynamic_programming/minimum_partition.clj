(ns main (:refer-clojure :exclude [find_min]))

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

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare find_min)

(def ^:dynamic find_min_diff nil)

(def ^:dynamic find_min_dp nil)

(def ^:dynamic find_min_i nil)

(def ^:dynamic find_min_idx nil)

(def ^:dynamic find_min_j nil)

(def ^:dynamic find_min_n nil)

(def ^:dynamic find_min_row nil)

(def ^:dynamic find_min_s nil)

(defn find_min [find_min_numbers]
  (binding [find_min_diff nil find_min_dp nil find_min_i nil find_min_idx nil find_min_j nil find_min_n nil find_min_row nil find_min_s nil] (try (do (set! find_min_n (count find_min_numbers)) (set! find_min_s 0) (set! find_min_idx 0) (while (< find_min_idx find_min_n) (do (set! find_min_s (+ find_min_s (nth find_min_numbers find_min_idx))) (set! find_min_idx (+ find_min_idx 1)))) (set! find_min_dp []) (set! find_min_i 0) (while (<= find_min_i find_min_n) (do (set! find_min_row []) (set! find_min_j 0) (while (<= find_min_j find_min_s) (do (set! find_min_row (conj find_min_row false)) (set! find_min_j (+ find_min_j 1)))) (set! find_min_dp (conj find_min_dp find_min_row)) (set! find_min_i (+ find_min_i 1)))) (set! find_min_i 0) (while (<= find_min_i find_min_n) (do (set! find_min_dp (assoc-in find_min_dp [find_min_i 0] true)) (set! find_min_i (+ find_min_i 1)))) (set! find_min_j 1) (while (<= find_min_j find_min_s) (do (set! find_min_dp (assoc-in find_min_dp [0 find_min_j] false)) (set! find_min_j (+ find_min_j 1)))) (set! find_min_i 1) (while (<= find_min_i find_min_n) (do (set! find_min_j 1) (while (<= find_min_j find_min_s) (do (set! find_min_dp (assoc-in find_min_dp [find_min_i find_min_j] (nth (nth find_min_dp (- find_min_i 1)) find_min_j))) (when (<= (nth find_min_numbers (- find_min_i 1)) find_min_j) (when (nth (nth find_min_dp (- find_min_i 1)) (- find_min_j (nth find_min_numbers (- find_min_i 1)))) (set! find_min_dp (assoc-in find_min_dp [find_min_i find_min_j] true)))) (set! find_min_j (+ find_min_j 1)))) (set! find_min_i (+ find_min_i 1)))) (set! find_min_diff 0) (set! find_min_j (/ find_min_s 2)) (loop [while_flag_1 true] (when (and while_flag_1 (>= find_min_j 0)) (cond (nth (nth find_min_dp find_min_n) find_min_j) (do (set! find_min_diff (- find_min_s (* 2 find_min_j))) (recur false)) :else (do (set! find_min_j (- find_min_j 1)) (recur while_flag_1))))) (throw (ex-info "return" {:v find_min_diff}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (find_min [1 2 3 4 5])))
      (println (str (find_min [5 5 5 5 5])))
      (println (str (find_min [5 5 5 5])))
      (println (str (find_min [3])))
      (println (str (find_min [])))
      (println (str (find_min [1 2 3 4])))
      (println (str (find_min [0 0 0 0])))
      (println (str (find_min [(- 1) (- 5) 5 1])))
      (println (str (find_min [9 9 9 9 9])))
      (println (str (find_min [1 5 10 3])))
      (println (str (find_min [(- 1) 0 1])))
      (println (str (find_min [10 9 8 7 6 5 4 3 2 1])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
