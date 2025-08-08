(ns main (:refer-clojure :exclude [index_of ord djb2]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(defn toi [s]
  (Integer/parseInt (str s)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare index_of ord djb2)

(def ^:dynamic djb2_hash_value nil)

(def ^:dynamic djb2_i nil)

(def ^:dynamic index_of_i nil)

(def ^:dynamic ord_digits nil)

(def ^:dynamic ord_idx nil)

(def ^:dynamic ord_lower nil)

(def ^:dynamic ord_upper nil)

(defn index_of [index_of_s index_of_ch]
  (binding [index_of_i nil] (try (do (set! index_of_i 0) (while (< index_of_i (count index_of_s)) (do (when (= (nth index_of_s index_of_i) index_of_ch) (throw (ex-info "return" {:v index_of_i}))) (set! index_of_i (+ index_of_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ord [ord_ch]
  (binding [ord_digits nil ord_idx nil ord_lower nil ord_upper nil] (try (do (set! ord_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! ord_lower "abcdefghijklmnopqrstuvwxyz") (set! ord_digits "0123456789") (set! ord_idx (index_of ord_upper ord_ch)) (when (>= ord_idx 0) (throw (ex-info "return" {:v (+ 65 ord_idx)}))) (set! ord_idx (index_of ord_lower ord_ch)) (when (>= ord_idx 0) (throw (ex-info "return" {:v (+ 97 ord_idx)}))) (set! ord_idx (index_of ord_digits ord_ch)) (when (>= ord_idx 0) (throw (ex-info "return" {:v (+ 48 ord_idx)}))) (if (= ord_ch " ") 32 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn djb2 [djb2_s]
  (binding [djb2_hash_value nil djb2_i nil] (try (do (set! djb2_hash_value 5381) (set! djb2_i 0) (while (< djb2_i (count djb2_s)) (do (set! djb2_hash_value (+ (* djb2_hash_value 33) (ord (nth djb2_s djb2_i)))) (set! djb2_hash_value (mod djb2_hash_value 4294967296)) (set! djb2_i (+ djb2_i 1)))) (throw (ex-info "return" {:v djb2_hash_value}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (djb2 "Algorithms"))
      (println (djb2 "scramble bits"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
