(ns main (:refer-clojure :exclude [sortRunes sortStrings main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare sortRunes sortStrings main)

(defn sortRunes [s]
  (try (do (def arr []) (def i 0) (while (< i (count s)) (do (def arr (conj arr (subs s i (+ i 1)))) (def i (+ i 1)))) (def n (count arr)) (def m 0) (while (< m n) (do (def j 0) (while (< j (- n 1)) (do (when (> (nth arr j) (nth arr (+ j 1))) (do (def tmp (nth arr j)) (def arr (assoc arr j (nth arr (+ j 1)))) (def arr (assoc arr (+ j 1) tmp)))) (def j (+ j 1)))) (def m (+ m 1)))) (def out "") (def i 0) (while (< i n) (do (def out (str out (nth arr i))) (def i (+ i 1)))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sortStrings [xs]
  (try (do (def res []) (def tmp xs) (while (> (count tmp) 0) (do (def min (nth tmp 0)) (def idx 0) (def i 1) (while (< i (count tmp)) (do (when (< (compare (nth tmp i) min) 0) (do (def min (nth tmp i)) (def idx i))) (def i (+ i 1)))) (def res (conj res min)) (def out []) (def j 0) (while (< j (count tmp)) (do (when (not= j idx) (def out (conj out (nth tmp j)))) (def j (+ j 1)))) (def tmp out))) (throw (ex-info "return" {:v res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def words ["abel" "able" "bale" "bela" "elba" "alger" "glare" "lager" "large" "regal" "angel" "angle" "galen" "glean" "lange" "caret" "carte" "cater" "crate" "trace" "elan" "lane" "lean" "lena" "neal" "evil" "levi" "live" "veil" "vile"]) (def groups {}) (def maxLen 0) (doseq [w words] (do (def k (sortRunes w)) (if (not (in k groups)) (def groups (assoc groups k [w])) (def groups (assoc groups k (conj (nth groups k) w)))) (when (> (count (nth groups k)) maxLen) (def maxLen (count (nth groups k)))))) (def printed {}) (doseq [w words] (do (def k (sortRunes w)) (when (= (count (nth groups k)) maxLen) (when (not (in k printed)) (do (def g (sortStrings (nth groups k))) (def line (str "[" (nth g 0))) (def i 1) (while (< i (count g)) (do (def line (str (str line " ") (nth g i))) (def i (+ i 1)))) (def line (str line "]")) (println line) (def printed (assoc printed k true)))))))))

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
