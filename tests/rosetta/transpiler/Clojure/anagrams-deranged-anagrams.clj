(ns main (:refer-clojure :exclude [sortRunes deranged main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare sortRunes deranged main)

(defn sortRunes [s]
  (try (do (def arr []) (def i 0) (while (< i (count s)) (do (def arr (conj arr (subs s i (+ i 1)))) (def i (+ i 1)))) (def n (count arr)) (def m 0) (while (< m n) (do (def j 0) (while (< j (- n 1)) (do (when (> (nth arr j) (nth arr (+ j 1))) (do (def tmp (nth arr j)) (def arr (assoc arr j (nth arr (+ j 1)))) (def arr (assoc arr (+ j 1) tmp)))) (def j (+ j 1)))) (def m (+ m 1)))) (def out "") (def i 0) (while (< i n) (do (def out (str out (nth arr i))) (def i (+ i 1)))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn deranged [a b]
  (try (do (when (not= (count a) (count b)) (throw (ex-info "return" {:v false}))) (def i 0) (while (< i (count a)) (do (when (= (subs a i (+ i 1)) (subs b i (+ i 1))) (throw (ex-info "return" {:v false}))) (def i (+ i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def words ["constitutionalism" "misconstitutional"]) (def m {}) (def bestLen 0) (def w1 "") (def w2 "") (loop [w_seq words] (when (seq w_seq) (let [w (first w_seq)] (cond (<= (count w) bestLen) (recur (rest w_seq)) (not (in k m)) (do (def m (assoc m k [w])) (recur (rest w_seq))) :else (do (def k (sortRunes w)) (loop [c_seq (nth m k)] (when (seq c_seq) (let [c (first c_seq)] (cond (deranged w c) (do (def bestLen (count w)) (def w1 c) (def w2 w) (recur nil)) :else (recur (rest c_seq)))))) (def m (assoc m k (conj (nth m k) w))) (recur (rest w_seq))))))) (println (str (str (str (str w1 " ") w2) " : Length ") (str bestLen)))))

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
