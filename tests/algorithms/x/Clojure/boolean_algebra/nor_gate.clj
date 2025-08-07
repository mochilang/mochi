(ns main (:refer-clojure :exclude [nor_gate center make_table_row truth_table]))

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

(declare nor_gate center make_table_row truth_table)

(def ^:dynamic center_i nil)

(def ^:dynamic center_j nil)

(def ^:dynamic center_left nil)

(def ^:dynamic center_res nil)

(def ^:dynamic center_right nil)

(def ^:dynamic center_total nil)

(def ^:dynamic make_table_row_output nil)

(defn nor_gate [nor_gate_input_1 nor_gate_input_2]
  (try (if (and (= nor_gate_input_1 0) (= nor_gate_input_2 0)) 1 0) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn center [center_s center_width]
  (binding [center_i nil center_j nil center_left nil center_res nil center_right nil center_total nil] (try (do (set! center_total (- center_width (count center_s))) (when (<= center_total 0) (throw (ex-info "return" {:v center_s}))) (set! center_left (quot center_total 2)) (set! center_right (- center_total center_left)) (set! center_res center_s) (set! center_i 0) (while (< center_i center_left) (do (set! center_res (str " " center_res)) (set! center_i (+ center_i 1)))) (set! center_j 0) (while (< center_j center_right) (do (set! center_res (str center_res " ")) (set! center_j (+ center_j 1)))) (throw (ex-info "return" {:v center_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn make_table_row [make_table_row_i make_table_row_j]
  (binding [make_table_row_output nil] (try (do (set! make_table_row_output (nor_gate make_table_row_i make_table_row_j)) (throw (ex-info "return" {:v (str (str (str (str (str (str "| " (center (str make_table_row_i) 8)) " | ") (center (str make_table_row_j) 8)) " | ") (center (str make_table_row_output) 8)) " |")}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn truth_table []
  (try (throw (ex-info "return" {:v (str (str (str (str (str (str (str (str "Truth Table of NOR Gate:\n" "| Input 1 | Input 2 | Output  |\n") (make_table_row 0 0)) "\n") (make_table_row 0 1)) "\n") (make_table_row 1 0)) "\n") (make_table_row 1 1))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (nor_gate 0 0))
      (println (nor_gate 0 1))
      (println (nor_gate 1 0))
      (println (nor_gate 1 1))
      (println (truth_table))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
