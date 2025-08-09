(ns main (:refer-clojure :exclude [neville_interpolate test_neville main]))

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

(declare neville_interpolate test_neville main)

(def ^:dynamic main_r nil)

(def ^:dynamic main_xs nil)

(def ^:dynamic main_ys nil)

(def ^:dynamic neville_interpolate_col nil)

(def ^:dynamic neville_interpolate_i nil)

(def ^:dynamic neville_interpolate_j nil)

(def ^:dynamic neville_interpolate_n nil)

(def ^:dynamic neville_interpolate_q nil)

(def ^:dynamic neville_interpolate_row nil)

(def ^:dynamic neville_interpolate_row_idx nil)

(def ^:dynamic test_neville_r1 nil)

(def ^:dynamic test_neville_r2 nil)

(def ^:dynamic test_neville_xs nil)

(def ^:dynamic test_neville_ys nil)

(defn neville_interpolate [neville_interpolate_x_points neville_interpolate_y_points neville_interpolate_x0]
  (binding [neville_interpolate_col nil neville_interpolate_i nil neville_interpolate_j nil neville_interpolate_n nil neville_interpolate_q nil neville_interpolate_row nil neville_interpolate_row_idx nil] (try (do (set! neville_interpolate_n (count neville_interpolate_x_points)) (set! neville_interpolate_q []) (set! neville_interpolate_i 0) (while (< neville_interpolate_i neville_interpolate_n) (do (set! neville_interpolate_row []) (set! neville_interpolate_j 0) (while (< neville_interpolate_j neville_interpolate_n) (do (set! neville_interpolate_row (conj neville_interpolate_row 0.0)) (set! neville_interpolate_j (+ neville_interpolate_j 1)))) (set! neville_interpolate_q (conj neville_interpolate_q neville_interpolate_row)) (set! neville_interpolate_i (+ neville_interpolate_i 1)))) (set! neville_interpolate_i 0) (while (< neville_interpolate_i neville_interpolate_n) (do (set! neville_interpolate_q (assoc-in neville_interpolate_q [neville_interpolate_i 1] (nth neville_interpolate_y_points neville_interpolate_i))) (set! neville_interpolate_i (+ neville_interpolate_i 1)))) (set! neville_interpolate_col 2) (while (< neville_interpolate_col neville_interpolate_n) (do (set! neville_interpolate_row_idx neville_interpolate_col) (while (< neville_interpolate_row_idx neville_interpolate_n) (do (set! neville_interpolate_q (assoc-in neville_interpolate_q [neville_interpolate_row_idx neville_interpolate_col] (quot (- (* (- neville_interpolate_x0 (nth neville_interpolate_x_points (+ (- neville_interpolate_row_idx neville_interpolate_col) 1))) (nth (nth neville_interpolate_q neville_interpolate_row_idx) (- neville_interpolate_col 1))) (* (- neville_interpolate_x0 (nth neville_interpolate_x_points neville_interpolate_row_idx)) (nth (nth neville_interpolate_q (- neville_interpolate_row_idx 1)) (- neville_interpolate_col 1)))) (- (nth neville_interpolate_x_points neville_interpolate_row_idx) (nth neville_interpolate_x_points (+ (- neville_interpolate_row_idx neville_interpolate_col) 1)))))) (set! neville_interpolate_row_idx (+ neville_interpolate_row_idx 1)))) (set! neville_interpolate_col (+ neville_interpolate_col 1)))) (throw (ex-info "return" {:v {:table neville_interpolate_q :value (nth (nth neville_interpolate_q (- neville_interpolate_n 1)) (- neville_interpolate_n 1))}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_neville []
  (binding [test_neville_r1 nil test_neville_r2 nil test_neville_xs nil test_neville_ys nil] (do (set! test_neville_xs [1.0 2.0 3.0 4.0 6.0]) (set! test_neville_ys [6.0 7.0 8.0 9.0 11.0]) (set! test_neville_r1 (neville_interpolate test_neville_xs test_neville_ys 5.0)) (when (not= (:value test_neville_r1) 10.0) (throw (Exception. "neville_interpolate at 5 failed"))) (set! test_neville_r2 (neville_interpolate test_neville_xs test_neville_ys 99.0)) (when (not= (:value test_neville_r2) 104.0) (throw (Exception. "neville_interpolate at 99 failed"))))))

(defn main []
  (binding [main_r nil main_xs nil main_ys nil] (do (test_neville) (set! main_xs [1.0 2.0 3.0 4.0 6.0]) (set! main_ys [6.0 7.0 8.0 9.0 11.0]) (set! main_r (neville_interpolate main_xs main_ys 5.0)) (println (:value main_r)))))

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
