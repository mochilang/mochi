(ns main (:refer-clojure :exclude [listStr pointerDemo sliceDemo]))

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

(declare listStr pointerDemo sliceDemo)

(def ^:dynamic listStr_i nil)

(def ^:dynamic listStr_s nil)

(def ^:dynamic pointerDemo_i nil)

(def ^:dynamic sliceDemo_a nil)

(def ^:dynamic sliceDemo_data nil)

(def ^:dynamic sliceDemo_idx nil)

(def ^:dynamic sliceDemo_s nil)

(defn listStr [listStr_xs]
  (binding [listStr_i nil listStr_s nil] (try (do (set! listStr_s "[") (set! listStr_i 0) (while (< listStr_i (count listStr_xs)) (do (set! listStr_s (str listStr_s (str (nth listStr_xs listStr_i)))) (when (< listStr_i (- (count listStr_xs) 1)) (set! listStr_s (str listStr_s " "))) (set! listStr_i (+ listStr_i 1)))) (set! listStr_s (str listStr_s "]")) (throw (ex-info "return" {:v listStr_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pointerDemo []
  (binding [pointerDemo_i nil] (do (println "Pointer:") (set! pointerDemo_i 0) (println "Before:") (println (str (str (str "\t<address>: " (str pointerDemo_i)) ", ") (str pointerDemo_i))) (set! pointerDemo_i 3) (println "After:") (println (str (str (str "\t<address>: " (str pointerDemo_i)) ", ") (str pointerDemo_i))))))

(defn sliceDemo []
  (binding [sliceDemo_a nil sliceDemo_data nil sliceDemo_idx nil sliceDemo_s nil] (do (println "Slice:") (set! sliceDemo_a []) (dotimes [_ 10] (set! sliceDemo_a (conj sliceDemo_a 0))) (set! sliceDemo_s sliceDemo_a) (println "Before:") (println (str "\ts: " (listStr sliceDemo_s))) (println (str "\ta: " (listStr sliceDemo_a))) (set! sliceDemo_data [65 32 115 116 114 105 110 103 46]) (set! sliceDemo_idx 0) (while (< sliceDemo_idx (count sliceDemo_data)) (do (set! sliceDemo_s (assoc sliceDemo_s sliceDemo_idx (nth sliceDemo_data sliceDemo_idx))) (set! sliceDemo_idx (+ sliceDemo_idx 1)))) (println "After:") (println (str "\ts: " (listStr sliceDemo_s))) (println (str "\ta: " (listStr sliceDemo_a))))))

(defn -main []
  (pointerDemo)
  (println "")
  (sliceDemo))

(-main)
