(ns main (:refer-clojure :exclude [make_list trapped_rainwater]))

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

(declare make_list trapped_rainwater)

(def ^:dynamic make_list_arr nil)

(def ^:dynamic make_list_i nil)

(def ^:dynamic trapped_rainwater_i nil)

(def ^:dynamic trapped_rainwater_last nil)

(def ^:dynamic trapped_rainwater_left nil)

(def ^:dynamic trapped_rainwater_left_max nil)

(def ^:dynamic trapped_rainwater_length nil)

(def ^:dynamic trapped_rainwater_right nil)

(def ^:dynamic trapped_rainwater_right_max nil)

(def ^:dynamic trapped_rainwater_smaller nil)

(def ^:dynamic trapped_rainwater_total nil)

(defn make_list [make_list_len make_list_value]
  (binding [make_list_arr nil make_list_i nil] (try (do (set! make_list_arr []) (set! make_list_i 0) (while (< make_list_i make_list_len) (do (set! make_list_arr (conj make_list_arr make_list_value)) (set! make_list_i (+ make_list_i 1)))) (throw (ex-info "return" {:v make_list_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn trapped_rainwater [trapped_rainwater_heights]
  (binding [trapped_rainwater_i nil trapped_rainwater_last nil trapped_rainwater_left nil trapped_rainwater_left_max nil trapped_rainwater_length nil trapped_rainwater_right nil trapped_rainwater_right_max nil trapped_rainwater_smaller nil trapped_rainwater_total nil] (try (do (when (= (count trapped_rainwater_heights) 0) (throw (ex-info "return" {:v 0}))) (set! trapped_rainwater_i 0) (while (< trapped_rainwater_i (count trapped_rainwater_heights)) (do (when (< (nth trapped_rainwater_heights trapped_rainwater_i) 0) (throw (Exception. "No height can be negative"))) (set! trapped_rainwater_i (+ trapped_rainwater_i 1)))) (set! trapped_rainwater_length (count trapped_rainwater_heights)) (set! trapped_rainwater_left_max (make_list trapped_rainwater_length 0)) (set! trapped_rainwater_left_max (assoc trapped_rainwater_left_max 0 (nth trapped_rainwater_heights 0))) (set! trapped_rainwater_i 1) (while (< trapped_rainwater_i trapped_rainwater_length) (do (if (> (nth trapped_rainwater_heights trapped_rainwater_i) (nth trapped_rainwater_left_max (- trapped_rainwater_i 1))) (set! trapped_rainwater_left_max (assoc trapped_rainwater_left_max trapped_rainwater_i (nth trapped_rainwater_heights trapped_rainwater_i))) (set! trapped_rainwater_left_max (assoc trapped_rainwater_left_max trapped_rainwater_i (nth trapped_rainwater_left_max (- trapped_rainwater_i 1))))) (set! trapped_rainwater_i (+ trapped_rainwater_i 1)))) (set! trapped_rainwater_right_max (make_list trapped_rainwater_length 0)) (set! trapped_rainwater_last (- trapped_rainwater_length 1)) (set! trapped_rainwater_right_max (assoc trapped_rainwater_right_max trapped_rainwater_last (nth trapped_rainwater_heights trapped_rainwater_last))) (set! trapped_rainwater_i (- trapped_rainwater_last 1)) (while (>= trapped_rainwater_i 0) (do (if (> (nth trapped_rainwater_heights trapped_rainwater_i) (nth trapped_rainwater_right_max (+ trapped_rainwater_i 1))) (set! trapped_rainwater_right_max (assoc trapped_rainwater_right_max trapped_rainwater_i (nth trapped_rainwater_heights trapped_rainwater_i))) (set! trapped_rainwater_right_max (assoc trapped_rainwater_right_max trapped_rainwater_i (nth trapped_rainwater_right_max (+ trapped_rainwater_i 1))))) (set! trapped_rainwater_i (- trapped_rainwater_i 1)))) (set! trapped_rainwater_total 0) (set! trapped_rainwater_i 0) (while (< trapped_rainwater_i trapped_rainwater_length) (do (set! trapped_rainwater_left (nth trapped_rainwater_left_max trapped_rainwater_i)) (set! trapped_rainwater_right (nth trapped_rainwater_right_max trapped_rainwater_i)) (set! trapped_rainwater_smaller (if (< trapped_rainwater_left trapped_rainwater_right) trapped_rainwater_left trapped_rainwater_right)) (set! trapped_rainwater_total (+ trapped_rainwater_total (- trapped_rainwater_smaller (nth trapped_rainwater_heights trapped_rainwater_i)))) (set! trapped_rainwater_i (+ trapped_rainwater_i 1)))) (throw (ex-info "return" {:v trapped_rainwater_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (trapped_rainwater [0 1 0 2 1 0 1 3 2 1 2 1])))
      (println (str (trapped_rainwater [7 1 5 3 6 4])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
