(ns main (:refer-clojure :exclude [image histogram medianThreshold threshold printImage main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare image histogram medianThreshold threshold printImage main)

(defn image []
  (try (throw (ex-info "return" {:v [[0 0 10000] [65535 65535 65535] [65535 65535 65535]]})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn histogram [g bins_p]
  (try (do (def bins bins_p) (when (<= bins 0) (def bins (count (nth g 0)))) (def h []) (def i 0) (while (< i bins) (do (def h (conj h 0)) (def i (+ i 1)))) (def y 0) (while (< y (count g)) (do (def row (nth g y)) (def x 0) (while (< x (count row)) (do (def p (nth row x)) (def idx (int (/ (* p (- bins 1)) 65535))) (def h (assoc h idx (+ (nth h idx) 1))) (def x (+ x 1)))) (def y (+ y 1)))) (throw (ex-info "return" {:v h}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn medianThreshold [h]
  (try (do (def lb 0) (def ub (- (count h) 1)) (def lSum 0) (def uSum 0) (while (<= lb ub) (if (< (+ lSum (nth h lb)) (+ uSum (nth h ub))) (do (def lSum (+ lSum (nth h lb))) (def lb (+ lb 1))) (do (def uSum (+ uSum (nth h ub))) (def ub (- ub 1))))) (throw (ex-info "return" {:v (int (/ (* ub 65535) (count h)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn threshold [g t]
  (try (do (def out []) (def y 0) (while (< y (count g)) (do (def row (nth g y)) (def newRow []) (def x 0) (while (< x (count row)) (do (if (< (nth row x) t) (def newRow (conj newRow 0)) (def newRow (conj newRow 65535))) (def x (+ x 1)))) (def out (conj out newRow)) (def y (+ y 1)))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn printImage [g]
  (do (def y 0) (while (< y (count g)) (do (def row (nth g y)) (def line "") (def x 0) (while (< x (count row)) (do (if (= (nth row x) 0) (def line (str line "0")) (def line (str line "1"))) (def x (+ x 1)))) (println line) (def y (+ y 1))))))

(defn main []
  (do (def img (image)) (def h (histogram img 0)) (println (str "Histogram: " (str h))) (def t (medianThreshold h)) (println (str "Threshold: " (str t))) (def bw (threshold img t)) (printImage bw)))

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
