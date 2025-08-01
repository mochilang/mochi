(ns main (:refer-clojure :exclude [repeat trimRightSpace block2text text2block]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare repeat trimRightSpace block2text text2block)

(declare blocks count_v i le lines out outLines pad s)

(defn repeat [s n]
  (try (do (def out "") (def i 0) (while (< i n) (do (def out (str out s)) (def i (+' i 1)))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn trimRightSpace [s]
  (try (do (def i (- (count s) 1)) (while (and (>= i 0) (= (subs s i (+' i 1)) " ")) (def i (- i 1))) (throw (ex-info "return" {:v (subs s 0 (+' i 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn block2text [block]
  (try (do (def out []) (doseq [b block] (def out (conj out (trimRightSpace b)))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn text2block [lines]
  (try (do (def out []) (def count_v 0) (doseq [line lines] (do (def s line) (def le (count s)) (if (> le 64) (def s (subvec s 0 64)) (when (< le 64) (def s (+' s (repeat " " (- 64 le)))))) (def out (conj out s)) (def count_v (+' count_v 1)))) (when (not= (mod count_v 16) 0) (do (def pad (- 16 (mod count_v 16))) (def i 0) (while (< i pad) (do (def out (conj out (repeat " " 64))) (def i (+' i 1)))))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def lines ["alpha" "beta" "gamma"])

(def blocks (text2block lines))

(def outLines (block2text blocks))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (doseq [l outLines] (when (not= l "") (println l)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
