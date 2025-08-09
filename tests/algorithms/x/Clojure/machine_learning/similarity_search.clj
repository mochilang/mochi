(ns main (:refer-clojure :exclude [sqrt euclidean similarity_search cosine_similarity]))

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

(declare sqrt euclidean similarity_search cosine_similarity)

(def ^:dynamic cosine_similarity_dot nil)

(def ^:dynamic cosine_similarity_i nil)

(def ^:dynamic cosine_similarity_norm_a nil)

(def ^:dynamic cosine_similarity_norm_b nil)

(def ^:dynamic euclidean_diff nil)

(def ^:dynamic euclidean_i nil)

(def ^:dynamic euclidean_res nil)

(def ^:dynamic euclidean_sum nil)

(def ^:dynamic main_k nil)

(def ^:dynamic similarity_search_d nil)

(def ^:dynamic similarity_search_dim nil)

(def ^:dynamic similarity_search_dist nil)

(def ^:dynamic similarity_search_i nil)

(def ^:dynamic similarity_search_j nil)

(def ^:dynamic similarity_search_nb nil)

(def ^:dynamic similarity_search_result nil)

(def ^:dynamic similarity_search_value nil)

(def ^:dynamic similarity_search_vec nil)

(def ^:dynamic sqrt_guess nil)

(def ^:dynamic sqrt_i nil)

(defn sqrt [sqrt_x]
  (binding [sqrt_guess nil sqrt_i nil] (try (do (when (<= sqrt_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrt_guess sqrt_x) (set! sqrt_i 0) (while (< sqrt_i 10) (do (set! sqrt_guess (/ (+ sqrt_guess (quot sqrt_x sqrt_guess)) 2.0)) (set! sqrt_i (+ sqrt_i 1)))) (throw (ex-info "return" {:v sqrt_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn euclidean [euclidean_a euclidean_b]
  (binding [euclidean_diff nil euclidean_i nil euclidean_res nil euclidean_sum nil] (try (do (set! euclidean_sum 0.0) (set! euclidean_i 0) (while (< euclidean_i (count euclidean_a)) (do (set! euclidean_diff (- (nth euclidean_a euclidean_i) (nth euclidean_b euclidean_i))) (set! euclidean_sum (+ euclidean_sum (* euclidean_diff euclidean_diff))) (set! euclidean_i (+ euclidean_i 1)))) (set! euclidean_res (sqrt euclidean_sum)) (throw (ex-info "return" {:v euclidean_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn similarity_search [similarity_search_dataset similarity_search_value_array]
  (binding [similarity_search_d nil similarity_search_dim nil similarity_search_dist nil similarity_search_i nil similarity_search_j nil similarity_search_nb nil similarity_search_result nil similarity_search_value nil similarity_search_vec nil] (try (do (set! similarity_search_dim (count (nth similarity_search_dataset 0))) (when (not= similarity_search_dim (count (nth similarity_search_value_array 0))) (throw (ex-info "return" {:v []}))) (set! similarity_search_result []) (set! similarity_search_i 0) (while (< similarity_search_i (count similarity_search_value_array)) (do (set! similarity_search_value (nth similarity_search_value_array similarity_search_i)) (set! similarity_search_dist (euclidean similarity_search_value (nth similarity_search_dataset 0))) (set! similarity_search_vec (nth similarity_search_dataset 0)) (set! similarity_search_j 1) (while (< similarity_search_j (count similarity_search_dataset)) (do (set! similarity_search_d (euclidean similarity_search_value (nth similarity_search_dataset similarity_search_j))) (when (< similarity_search_d similarity_search_dist) (do (set! similarity_search_dist similarity_search_d) (set! similarity_search_vec (nth similarity_search_dataset similarity_search_j)))) (set! similarity_search_j (+ similarity_search_j 1)))) (set! similarity_search_nb {:distance similarity_search_dist :vector similarity_search_vec}) (set! similarity_search_result (conj similarity_search_result similarity_search_nb)) (set! similarity_search_i (+ similarity_search_i 1)))) (throw (ex-info "return" {:v similarity_search_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn cosine_similarity [cosine_similarity_a cosine_similarity_b]
  (binding [cosine_similarity_dot nil cosine_similarity_i nil cosine_similarity_norm_a nil cosine_similarity_norm_b nil] (try (do (set! cosine_similarity_dot 0.0) (set! cosine_similarity_norm_a 0.0) (set! cosine_similarity_norm_b 0.0) (set! cosine_similarity_i 0) (while (< cosine_similarity_i (count cosine_similarity_a)) (do (set! cosine_similarity_dot (+ cosine_similarity_dot (* (nth cosine_similarity_a cosine_similarity_i) (nth cosine_similarity_b cosine_similarity_i)))) (set! cosine_similarity_norm_a (+ cosine_similarity_norm_a (* (nth cosine_similarity_a cosine_similarity_i) (nth cosine_similarity_a cosine_similarity_i)))) (set! cosine_similarity_norm_b (+ cosine_similarity_norm_b (* (nth cosine_similarity_b cosine_similarity_i) (nth cosine_similarity_b cosine_similarity_i)))) (set! cosine_similarity_i (+ cosine_similarity_i 1)))) (if (or (= cosine_similarity_norm_a 0.0) (= cosine_similarity_norm_b 0.0)) 0.0 (quot cosine_similarity_dot (* (sqrt cosine_similarity_norm_a) (sqrt cosine_similarity_norm_b))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_dataset [[0.0 0.0 0.0] [1.0 1.0 1.0] [2.0 2.0 2.0]])

(def ^:dynamic main_value_array [[0.0 0.0 0.0] [0.0 0.0 1.0]])

(def ^:dynamic main_neighbors (similarity_search main_dataset main_value_array))

(def ^:dynamic main_k 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< main_k (count main_neighbors)) (do (def ^:dynamic main_n (nth main_neighbors main_k)) (println (str (str (str (str "[" (str (:vector main_n))) ", ") (str (:distance main_n))) "]")) (def main_k (+ main_k 1))))
      (println (str (cosine_similarity [1.0 2.0] [6.0 32.0])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
