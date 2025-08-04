(ns main (:refer-clojure :exclude [countOccurrences main]))

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

(declare countOccurrences main)

(def ^:dynamic countOccurrences_cnt nil)

(def ^:dynamic countOccurrences_i nil)

(def ^:dynamic countOccurrences_step nil)

(defn countOccurrences [countOccurrences_s countOccurrences_sub]
  (binding [countOccurrences_cnt nil countOccurrences_i nil countOccurrences_step nil] (try (do (when (= (count countOccurrences_sub) 0) (throw (ex-info "return" {:v (+ (count countOccurrences_s) 1)}))) (set! countOccurrences_cnt 0) (set! countOccurrences_i 0) (set! countOccurrences_step (count countOccurrences_sub)) (while (<= (+ countOccurrences_i countOccurrences_step) (count countOccurrences_s)) (if (= (subs countOccurrences_s countOccurrences_i (+ countOccurrences_i countOccurrences_step)) countOccurrences_sub) (do (set! countOccurrences_cnt (+ countOccurrences_cnt 1)) (set! countOccurrences_i (+ countOccurrences_i countOccurrences_step))) (set! countOccurrences_i (+ countOccurrences_i 1)))) (throw (ex-info "return" {:v countOccurrences_cnt}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (str (countOccurrences "the three truths" "th"))) (println (str (countOccurrences "ababababab" "abab")))))

(defn -main []
  (main))

(-main)
