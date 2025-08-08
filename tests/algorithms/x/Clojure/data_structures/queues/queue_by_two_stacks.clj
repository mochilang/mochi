(ns main (:refer-clojure :exclude [new_queue len_queue str_queue put get]))

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

(declare new_queue len_queue str_queue put get)

(def ^:dynamic get_i nil)

(def ^:dynamic get_idx nil)

(def ^:dynamic get_idx2 nil)

(def ^:dynamic get_j nil)

(def ^:dynamic get_new_s1 nil)

(def ^:dynamic get_new_s2 nil)

(def ^:dynamic get_s1 nil)

(def ^:dynamic get_s2 nil)

(def ^:dynamic get_v nil)

(def ^:dynamic get_value nil)

(def ^:dynamic main_q nil)

(def ^:dynamic put_s1 nil)

(def ^:dynamic str_queue_i nil)

(def ^:dynamic str_queue_items nil)

(def ^:dynamic str_queue_j nil)

(def ^:dynamic str_queue_k nil)

(def ^:dynamic str_queue_s nil)

(defn new_queue [new_queue_items]
  (try (throw (ex-info "return" {:v {:stack1 new_queue_items :stack2 []}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn len_queue [len_queue_q]
  (try (throw (ex-info "return" {:v (+ (count (:stack1 len_queue_q)) (count (:stack2 len_queue_q)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn str_queue [str_queue_q]
  (binding [str_queue_i nil str_queue_items nil str_queue_j nil str_queue_k nil str_queue_s nil] (try (do (set! str_queue_items []) (set! str_queue_i (- (count (:stack2 str_queue_q)) 1)) (while (>= str_queue_i 0) (do (set! str_queue_items (conj str_queue_items (get (:stack2 str_queue_q) str_queue_i))) (set! str_queue_i (- str_queue_i 1)))) (set! str_queue_j 0) (while (< str_queue_j (count (:stack1 str_queue_q))) (do (set! str_queue_items (conj str_queue_items (get (:stack1 str_queue_q) str_queue_j))) (set! str_queue_j (+ str_queue_j 1)))) (set! str_queue_s "Queue((") (set! str_queue_k 0) (while (< str_queue_k (count str_queue_items)) (do (set! str_queue_s (str str_queue_s (str (nth str_queue_items str_queue_k)))) (when (< str_queue_k (- (count str_queue_items) 1)) (set! str_queue_s (str str_queue_s ", "))) (set! str_queue_k (+ str_queue_k 1)))) (set! str_queue_s (str str_queue_s "))")) (throw (ex-info "return" {:v str_queue_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn put [put_q put_item]
  (binding [put_s1 nil] (try (do (set! put_s1 (:stack1 put_q)) (set! put_s1 (conj put_s1 put_item)) (throw (ex-info "return" {:v {:stack1 put_s1 :stack2 (:stack2 put_q)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get [get_q]
  (binding [get_i nil get_idx nil get_idx2 nil get_j nil get_new_s1 nil get_new_s2 nil get_s1 nil get_s2 nil get_v nil get_value nil] (try (do (set! get_s1 (:stack1 get_q)) (set! get_s2 (:stack2 get_q)) (when (= (count get_s2) 0) (while (> (count get_s1) 0) (do (set! get_idx (- (count get_s1) 1)) (set! get_v (nth get_s1 get_idx)) (set! get_new_s1 []) (set! get_i 0) (while (< get_i get_idx) (do (set! get_new_s1 (conj get_new_s1 (nth get_s1 get_i))) (set! get_i (+ get_i 1)))) (set! get_s1 get_new_s1) (set! get_s2 (conj get_s2 get_v))))) (when (= (count get_s2) 0) (throw (Exception. "Queue is empty"))) (set! get_idx2 (- (count get_s2) 1)) (set! get_value (nth get_s2 get_idx2)) (set! get_new_s2 []) (set! get_j 0) (while (< get_j get_idx2) (do (set! get_new_s2 (conj get_new_s2 (nth get_s2 get_j))) (set! get_j (+ get_j 1)))) (set! get_s2 get_new_s2) (throw (ex-info "return" {:v {:queue {:stack1 get_s1 :stack2 get_s2} :value get_value}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_q (new_queue [10 20 30]))

(def ^:dynamic main_r1 (get main_q))

(def ^:dynamic main_r2 (get main_q))

(def ^:dynamic main_r3 (get main_q))

(def ^:dynamic main_r4 (get main_q))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (def main_q (:queue main_r1))
      (println (:value main_r1))
      (def main_q (put main_q 40))
      (def main_q (:queue main_r2))
      (println (:value main_r2))
      (def main_q (:queue main_r3))
      (println (:value main_r3))
      (println (len_queue main_q))
      (def main_q (:queue main_r4))
      (println (:value main_r4))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
