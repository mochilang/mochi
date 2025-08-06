(ns main (:refer-clojure :exclude [is_valid place_word remove_word solve_crossword]))

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

(declare is_valid place_word remove_word solve_crossword)

(def ^:dynamic place_word_ch nil)

(def ^:dynamic place_word_puzzle nil)

(def ^:dynamic remove_word_puzzle nil)

(def ^:dynamic solve_crossword_used nil)

(def ^:dynamic solve_crossword_word nil)

(defn is_valid [is_valid_puzzle is_valid_word is_valid_row is_valid_col is_valid_vertical]
  (try (do (dotimes [i (count is_valid_word)] (if is_valid_vertical (when (or (>= (+ is_valid_row i) (count is_valid_puzzle)) (not= (nth (nth is_valid_puzzle (+ is_valid_row i)) is_valid_col) "")) (throw (ex-info "return" {:v false}))) (when (or (>= (+ is_valid_col i) (count (nth is_valid_puzzle 0))) (not= (nth (nth is_valid_puzzle is_valid_row) (+ is_valid_col i)) "")) (throw (ex-info "return" {:v false}))))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn place_word [place_word_puzzle_p place_word_word place_word_row place_word_col place_word_vertical]
  (binding [place_word_ch nil place_word_puzzle nil] (do (set! place_word_puzzle place_word_puzzle_p) (dotimes [i (count place_word_word)] (do (set! place_word_ch (nth place_word_word i)) (if place_word_vertical (set! place_word_puzzle (assoc-in place_word_puzzle [(+ place_word_row i) place_word_col] place_word_ch)) (set! place_word_puzzle (assoc-in place_word_puzzle [place_word_row (+ place_word_col i)] place_word_ch))))))))

(defn remove_word [remove_word_puzzle_p remove_word_word remove_word_row remove_word_col remove_word_vertical]
  (binding [remove_word_puzzle nil] (do (set! remove_word_puzzle remove_word_puzzle_p) (dotimes [i (count remove_word_word)] (if remove_word_vertical (set! remove_word_puzzle (assoc-in remove_word_puzzle [(+ remove_word_row i) remove_word_col] "")) (set! remove_word_puzzle (assoc-in remove_word_puzzle [remove_word_row (+ remove_word_col i)] "")))))))

(defn solve_crossword [solve_crossword_puzzle solve_crossword_words solve_crossword_used_p]
  (binding [solve_crossword_used nil solve_crossword_word nil] (try (do (set! solve_crossword_used solve_crossword_used_p) (dotimes [row (count solve_crossword_puzzle)] (dotimes [col (count (nth solve_crossword_puzzle 0))] (when (= (nth (nth solve_crossword_puzzle row) col) "") (do (dotimes [i (count solve_crossword_words)] (when (not (nth solve_crossword_used i)) (do (set! solve_crossword_word (nth solve_crossword_words i)) (doseq [vertical [true false]] (when (is_valid solve_crossword_puzzle solve_crossword_word row col vertical) (do (place_word solve_crossword_puzzle solve_crossword_word row col vertical) (set! solve_crossword_used (assoc solve_crossword_used i true)) (when (solve_crossword solve_crossword_puzzle solve_crossword_words solve_crossword_used) (throw (ex-info "return" {:v true}))) (set! solve_crossword_used (assoc solve_crossword_used i false)) (remove_word solve_crossword_puzzle solve_crossword_word row col vertical))))))) (throw (ex-info "return" {:v false})))))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_puzzle [["" "" ""] ["" "" ""] ["" "" ""]])

(def ^:dynamic main_words ["cat" "dog" "car"])

(def ^:dynamic main_used [false false false])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (if (solve_crossword main_puzzle main_words main_used) (do (println "Solution found:") (doseq [row main_puzzle] (println row))) (println "No solution found:"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
