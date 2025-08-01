(ns main (:refer-clojure :exclude [randInt newBoard copyBoard flipRow flipCol boardsEqual shuffleBoard solve applySolution printBoard main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare randInt newBoard copyBoard flipRow flipCol boardsEqual shuffleBoard solve applySolution printBoard main)

(declare ares board col diff i idx j k line moves n nb next_v r res row s seed sol sres target val)

(defn randInt [seed n]
  (try (do (def next_v (mod (+' (* seed 1664525) 1013904223) 2147483647)) (throw (ex-info "return" {:v [next_v (mod next_v n)]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn newBoard [n seed]
  (try (do (def board []) (def s seed) (def i 0) (while (< i n) (do (def row []) (def j 0) (while (< j n) (do (def r (randInt s 2)) (def s (nth r 0)) (def row (conj row (nth r 1))) (def j (+' j 1)))) (def board (conj board row)) (def i (+' i 1)))) (throw (ex-info "return" {:v [board s]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn copyBoard [b]
  (try (do (def nb []) (def i 0) (while (< i (count b)) (do (def row []) (def j 0) (while (< j (count (nth b i))) (do (def row (conj row (nth (nth b i) j))) (def j (+' j 1)))) (def nb (conj nb row)) (def i (+' i 1)))) (throw (ex-info "return" {:v nb}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn flipRow [b_p r]
  (try (do (def b b_p) (def j 0) (while (< j (count (nth b r))) (do (def b (assoc-in b [r j] (- 1 (nth (nth b r) j)))) (def j (+' j 1)))) (throw (ex-info "return" {:v b}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn flipCol [b_p c]
  (try (do (def b b_p) (def i 0) (while (< i (count b)) (do (def b (assoc-in b [i c] (- 1 (nth (nth b i) c)))) (def i (+' i 1)))) (throw (ex-info "return" {:v b}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn boardsEqual [a b]
  (try (do (def i 0) (while (< i (count a)) (do (def j 0) (while (< j (count (nth a i))) (do (when (not= (nth (nth a i) j) (nth (nth b i) j)) (throw (ex-info "return" {:v false}))) (def j (+' j 1)))) (def i (+' i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn shuffleBoard [b_p seed]
  (try (do (def b b_p) (def s seed) (def n (count b)) (def k 0) (while (< k (* 2 n)) (do (def r (randInt s n)) (def s (nth r 0)) (def idx (int (nth r 1))) (if (= (mod k 2) 0) (def b (flipRow b idx)) (def b (flipCol b idx))) (def k (+' k 1)))) (throw (ex-info "return" {:v [b s]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn solve [board target]
  (try (do (def n (count board)) (def row []) (def col []) (def i 0) (while (< i n) (do (def diff (if (not= (nth (nth board i) 0) (nth (nth target i) 0)) 1 0)) (def row (conj row diff)) (def i (+' i 1)))) (def j 0) (while (< j n) (do (def diff (if (not= (nth (nth board 0) j) (nth (nth target 0) j)) 1 0)) (def val (mod (+' diff (nth row 0)) 2)) (def col (conj col val)) (def j (+' j 1)))) (throw (ex-info "return" {:v {"row" row "col" col}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn applySolution [b sol]
  (try (do (def board b) (def moves 0) (def i 0) (while (< i (count (get sol "row"))) (do (when (= (nth (get sol "row") i) 1) (do (def board (flipRow board i)) (def moves (+' moves 1)))) (def i (+' i 1)))) (def j 0) (while (< j (count (get sol "col"))) (do (when (= (nth (get sol "col") j) 1) (do (def board (flipCol board j)) (def moves (+' moves 1)))) (def j (+' j 1)))) (throw (ex-info "return" {:v [board moves]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn printBoard [b]
  (do (def i 0) (while (< i (count b)) (do (def line "") (def j 0) (while (< j (count (nth b i))) (do (def line (str line (str (nth (nth b i) j)))) (when (< j (- (count (nth b i)) 1)) (def line (str line " "))) (def j (+' j 1)))) (println line) (def i (+' i 1))))))

(defn main []
  (do (def n 3) (def seed 1) (def res (newBoard n seed)) (def target (nth res 0)) (def seed (int (nth res 1))) (def board (copyBoard target)) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (def sres (shuffleBoard (copyBoard board) seed)) (def board (nth sres 0)) (def seed (int (nth sres 1))) (cond (not (boardsEqual board target)) (recur false) :else (recur while_flag_1))))) (println "Target:") (printBoard target) (println "Board:") (printBoard board) (def sol (solve board target)) (def ares (applySolution board sol)) (def board (nth ares 0)) (def moves (int (nth ares 1))) (println "Solved:") (printBoard board) (println (str "Moves: " (str moves)))))

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
