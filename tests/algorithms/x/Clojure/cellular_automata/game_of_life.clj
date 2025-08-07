(ns main (:refer-clojure :exclude [count_alive_neighbours next_state step show]))

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

(declare count_alive_neighbours next_state step show)

(def ^:dynamic count_alive_neighbours_alive nil)

(def ^:dynamic count_alive_neighbours_dc nil)

(def ^:dynamic count_alive_neighbours_dr nil)

(def ^:dynamic count_alive_neighbours_nc nil)

(def ^:dynamic count_alive_neighbours_nr nil)

(def ^:dynamic count_alive_neighbours_size nil)

(def ^:dynamic main_board nil)

(def ^:dynamic main_i nil)

(def ^:dynamic next_state_state nil)

(def ^:dynamic show_c nil)

(def ^:dynamic show_line nil)

(def ^:dynamic show_r nil)

(def ^:dynamic step_alive nil)

(def ^:dynamic step_c nil)

(def ^:dynamic step_cell nil)

(def ^:dynamic step_new_board nil)

(def ^:dynamic step_new_row nil)

(def ^:dynamic step_r nil)

(def ^:dynamic step_size nil)

(def ^:dynamic step_updated nil)

(defn count_alive_neighbours [count_alive_neighbours_board count_alive_neighbours_row count_alive_neighbours_col]
  (binding [count_alive_neighbours_alive nil count_alive_neighbours_dc nil count_alive_neighbours_dr nil count_alive_neighbours_nc nil count_alive_neighbours_nr nil count_alive_neighbours_size nil] (try (do (set! count_alive_neighbours_size (count count_alive_neighbours_board)) (set! count_alive_neighbours_alive 0) (set! count_alive_neighbours_dr (- 1)) (while (< count_alive_neighbours_dr 2) (do (set! count_alive_neighbours_dc (- 1)) (while (< count_alive_neighbours_dc 2) (do (set! count_alive_neighbours_nr (+ count_alive_neighbours_row count_alive_neighbours_dr)) (set! count_alive_neighbours_nc (+ count_alive_neighbours_col count_alive_neighbours_dc)) (when (and (and (and (and (not (and (= count_alive_neighbours_dr 0) (= count_alive_neighbours_dc 0))) (>= count_alive_neighbours_nr 0)) (< count_alive_neighbours_nr count_alive_neighbours_size)) (>= count_alive_neighbours_nc 0)) (< count_alive_neighbours_nc count_alive_neighbours_size)) (when (nth (nth count_alive_neighbours_board count_alive_neighbours_nr) count_alive_neighbours_nc) (set! count_alive_neighbours_alive (+ count_alive_neighbours_alive 1)))) (set! count_alive_neighbours_dc (+ count_alive_neighbours_dc 1)))) (set! count_alive_neighbours_dr (+ count_alive_neighbours_dr 1)))) (throw (ex-info "return" {:v count_alive_neighbours_alive}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn next_state [next_state_current next_state_alive]
  (binding [next_state_state nil] (try (do (set! next_state_state next_state_current) (if next_state_current (if (< next_state_alive 2) (set! next_state_state false) (if (or (= next_state_alive 2) (= next_state_alive 3)) (set! next_state_state true) (set! next_state_state false))) (when (= next_state_alive 3) (set! next_state_state true))) (throw (ex-info "return" {:v next_state_state}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn step [step_board]
  (binding [step_alive nil step_c nil step_cell nil step_new_board nil step_new_row nil step_r nil step_size nil step_updated nil] (try (do (set! step_size (count step_board)) (set! step_new_board []) (set! step_r 0) (while (< step_r step_size) (do (set! step_new_row []) (set! step_c 0) (while (< step_c step_size) (do (set! step_alive (count_alive_neighbours step_board step_r step_c)) (set! step_cell (nth (nth step_board step_r) step_c)) (set! step_updated (next_state step_cell step_alive)) (set! step_new_row (conj step_new_row step_updated)) (set! step_c (+ step_c 1)))) (set! step_new_board (conj step_new_board step_new_row)) (set! step_r (+ step_r 1)))) (throw (ex-info "return" {:v step_new_board}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn show [show_board]
  (binding [show_c nil show_line nil show_r nil] (do (set! show_r 0) (while (< show_r (count show_board)) (do (set! show_line "") (set! show_c 0) (while (< show_c (count (nth show_board show_r))) (do (if (nth (nth show_board show_r) show_c) (set! show_line (str show_line "#")) (set! show_line (str show_line "."))) (set! show_c (+ show_c 1)))) (println show_line) (set! show_r (+ show_r 1)))))))

(def ^:dynamic main_glider [[false true false false false] [false false true false false] [true true true false false] [false false false false false] [false false false false false]])

(def ^:dynamic main_board main_glider)

(def ^:dynamic main_i 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "Initial")
      (show main_board)
      (while (< main_i 4) (do (def main_board (step main_board)) (println (str "\nStep " (str (+ main_i 1)))) (show main_board) (def main_i (+ main_i 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
