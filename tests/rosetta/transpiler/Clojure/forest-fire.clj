(ns main (:refer-clojure :exclude [repeat chance newBoard step printBoard]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare repeat chance newBoard step printBoard)

(declare b board burning c cc cell cols dc dr dst f i line next_v p r row rows rr s threshold)

(def rows 20)

(def cols 30)

(def p 0.01)

(def f 0.001)

(defn repeat [ch n]
  (try (do (def s "") (def i 0) (while (< i n) (do (def s (str s ch)) (def i (+' i 1)))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn chance [prob]
  (try (do (def threshold (Integer/parseInt (* prob 1000.0))) (throw (ex-info "return" {:v (< (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) 1000) threshold)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn newBoard []
  (try (do (def b []) (def r 0) (while (< r rows) (do (def row []) (def c 0) (while (< c cols) (do (if (= (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) 2) 0) (def row (conj row "T")) (def row (conj row " "))) (def c (+' c 1)))) (def b (conj b row)) (def r (+' r 1)))) (throw (ex-info "return" {:v b}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn step [src]
  (try (do (def dst []) (def r 0) (while (< r rows) (do (def row []) (def c 0) (while (< c cols) (do (def cell (nth (nth src r) c)) (def next_v cell) (if (= cell "#") (def next_v " ") (if (= cell "T") (do (def burning false) (def dr (- 1)) (while (<= dr 1) (do (def dc (- 1)) (while (<= dc 1) (do (when (or (not= dr 0) (not= dc 0)) (do (def rr (+' r dr)) (def cc (+' c dc)) (when (and (and (and (>= rr 0) (< rr rows)) (>= cc 0)) (< cc cols)) (when (= (nth (nth src rr) cc) "#") (def burning true))))) (def dc (+' dc 1)))) (def dr (+' dr 1)))) (when (or burning (chance f)) (def next_v "#"))) (when (chance p) (def next_v "T")))) (def row (conj row next_v)) (def c (+' c 1)))) (def dst (conj dst row)) (def r (+' r 1)))) (throw (ex-info "return" {:v dst}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn printBoard [b]
  (do (println (str (repeat "__" cols) "\n\n")) (def r 0) (while (< r rows) (do (def line "") (def c 0) (while (< c cols) (do (def cell (nth (nth b r) c)) (if (= cell " ") (def line (str line "  ")) (def line (str (str line " ") cell))) (def c (+' c 1)))) (println (str line "\n")) (def r (+' r 1))))))

(def board (newBoard))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (printBoard board)
      (def board (step board))
      (printBoard board)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
