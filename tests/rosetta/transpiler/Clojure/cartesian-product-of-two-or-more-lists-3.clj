(ns main (:refer-clojure :exclude [listStr llStr concat cartN main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare listStr llStr concat cartN main)

(def ^:dynamic cartN_a nil)

(def ^:dynamic cartN_out nil)

(def ^:dynamic concat_out nil)

(def ^:dynamic listStr_i nil)

(def ^:dynamic listStr_s nil)

(def ^:dynamic llStr_i nil)

(def ^:dynamic llStr_s nil)

(def ^:dynamic rest_v nil)

(defn listStr [listStr_xs]
  (binding [listStr_i nil listStr_s nil] (try (do (set! listStr_s "[") (set! listStr_i 0) (while (< listStr_i (count listStr_xs)) (do (set! listStr_s (str listStr_s (str (nth listStr_xs listStr_i)))) (when (< listStr_i (- (count listStr_xs) 1)) (set! listStr_s (str listStr_s " "))) (set! listStr_i (+ listStr_i 1)))) (set! listStr_s (str listStr_s "]")) (throw (ex-info "return" {:v listStr_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn llStr [llStr_lst]
  (binding [llStr_i nil llStr_s nil] (try (do (set! llStr_s "[") (set! llStr_i 0) (while (< llStr_i (count llStr_lst)) (do (set! llStr_s (str llStr_s (listStr (nth llStr_lst llStr_i)))) (when (< llStr_i (- (count llStr_lst) 1)) (set! llStr_s (str llStr_s " "))) (set! llStr_i (+ llStr_i 1)))) (set! llStr_s (str llStr_s "]")) (throw (ex-info "return" {:v llStr_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn concat [concat_a concat_b]
  (binding [concat_out nil] (try (do (set! concat_out []) (doseq [v concat_a] (set! concat_out (conj concat_out v))) (doseq [v concat_b] (set! concat_out (conj concat_out v))) (throw (ex-info "return" {:v concat_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn cartN [cartN_lists]
  (binding [cartN_a nil cartN_out nil rest_v nil] (try (do (when (= cartN_lists nil) (throw (ex-info "return" {:v []}))) (set! cartN_a cartN_lists) (when (= (count cartN_a) 0) (throw (ex-info "return" {:v [[]]}))) (set! cartN_out []) (set! rest_v (cartN (subvec cartN_a 1 (count cartN_a)))) (doseq [x (nth cartN_a 0)] (doseq [p rest_v] (set! cartN_out (conj cartN_out (concat [x] p))))) (throw (ex-info "return" {:v cartN_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (llStr (cartN [[1 2] [3 4]]))) (println (llStr (cartN [[3 4] [1 2]]))) (println (llStr (cartN [[1 2] []]))) (println (llStr (cartN [[] [1 2]]))) (println "") (println "[") (doseq [p (cartN [[1776 1789] [7 12] [4 14 23] [0 1]])] (println (str " " (listStr p)))) (println "]") (println (llStr (cartN [[1 2 3] [30] [500 100]]))) (println (llStr (cartN [[1 2 3] [] [500 100]]))) (println "") (println (llStr (cartN nil))) (println (llStr (cartN [])))))

(defn -main []
  (main))

(-main)
