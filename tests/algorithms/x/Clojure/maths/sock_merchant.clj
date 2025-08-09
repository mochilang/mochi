(ns main (:refer-clojure :exclude [sock_merchant test_sock_merchant main]))

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

(declare sock_merchant test_sock_merchant main)

(def ^:dynamic count_v nil)

(def ^:dynamic main_example1 nil)

(def ^:dynamic main_example2 nil)

(def ^:dynamic sock_merchant_a nil)

(def ^:dynamic sock_merchant_arr nil)

(def ^:dynamic sock_merchant_b nil)

(def ^:dynamic sock_merchant_i nil)

(def ^:dynamic sock_merchant_min_idx nil)

(def ^:dynamic sock_merchant_n nil)

(def ^:dynamic sock_merchant_pairs nil)

(def ^:dynamic sock_merchant_temp nil)

(def ^:dynamic test_sock_merchant_example1 nil)

(def ^:dynamic test_sock_merchant_example2 nil)

(defn sock_merchant [sock_merchant_colors]
  (binding [count_v nil sock_merchant_a nil sock_merchant_arr nil sock_merchant_b nil sock_merchant_i nil sock_merchant_min_idx nil sock_merchant_n nil sock_merchant_pairs nil sock_merchant_temp nil] (try (do (set! sock_merchant_arr []) (set! sock_merchant_i 0) (while (< sock_merchant_i (count sock_merchant_colors)) (do (set! sock_merchant_arr (conj sock_merchant_arr (nth sock_merchant_colors sock_merchant_i))) (set! sock_merchant_i (+ sock_merchant_i 1)))) (set! sock_merchant_n (count sock_merchant_arr)) (set! sock_merchant_a 0) (while (< sock_merchant_a sock_merchant_n) (do (set! sock_merchant_min_idx sock_merchant_a) (set! sock_merchant_b (+ sock_merchant_a 1)) (while (< sock_merchant_b sock_merchant_n) (do (when (< (nth sock_merchant_arr sock_merchant_b) (nth sock_merchant_arr sock_merchant_min_idx)) (set! sock_merchant_min_idx sock_merchant_b)) (set! sock_merchant_b (+ sock_merchant_b 1)))) (set! sock_merchant_temp (nth sock_merchant_arr sock_merchant_a)) (set! sock_merchant_arr (assoc sock_merchant_arr sock_merchant_a (nth sock_merchant_arr sock_merchant_min_idx))) (set! sock_merchant_arr (assoc sock_merchant_arr sock_merchant_min_idx sock_merchant_temp)) (set! sock_merchant_a (+ sock_merchant_a 1)))) (set! sock_merchant_pairs 0) (set! sock_merchant_i 0) (while (< sock_merchant_i sock_merchant_n) (do (set! count_v 1) (while (and (< (+ sock_merchant_i 1) sock_merchant_n) (= (nth sock_merchant_arr sock_merchant_i) (nth sock_merchant_arr (+ sock_merchant_i 1)))) (do (set! count_v (+ count_v 1)) (set! sock_merchant_i (+ sock_merchant_i 1)))) (set! sock_merchant_pairs (+ sock_merchant_pairs (quot count_v 2))) (set! sock_merchant_i (+ sock_merchant_i 1)))) (throw (ex-info "return" {:v sock_merchant_pairs}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_sock_merchant []
  (binding [test_sock_merchant_example1 nil test_sock_merchant_example2 nil] (do (set! test_sock_merchant_example1 [10 20 20 10 10 30 50 10 20]) (when (not= (sock_merchant test_sock_merchant_example1) 3) (throw (Exception. "example1 failed"))) (set! test_sock_merchant_example2 [1 1 3 3]) (when (not= (sock_merchant test_sock_merchant_example2) 2) (throw (Exception. "example2 failed"))))))

(defn main []
  (binding [main_example1 nil main_example2 nil] (do (test_sock_merchant) (set! main_example1 [10 20 20 10 10 30 50 10 20]) (println (str (sock_merchant main_example1))) (set! main_example2 [1 1 3 3]) (println (str (sock_merchant main_example2))))))

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
