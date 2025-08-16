(ns main (:refer-clojure :exclude [get_winner update list_to_string matrix_to_string main]))

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare get_winner update list_to_string matrix_to_string main)

(def ^:dynamic get_winner_d0 nil)

(def ^:dynamic get_winner_d1 nil)

(def ^:dynamic get_winner_diff0 nil)

(def ^:dynamic get_winner_diff1 nil)

(def ^:dynamic get_winner_i nil)

(def ^:dynamic list_to_string_i nil)

(def ^:dynamic list_to_string_s nil)

(def ^:dynamic main__ nil)

(def ^:dynamic main_alpha nil)

(def ^:dynamic main_epochs nil)

(def ^:dynamic main_j nil)

(def ^:dynamic main_sample nil)

(def ^:dynamic main_training_samples nil)

(def ^:dynamic main_weights nil)

(def ^:dynamic main_winner nil)

(def ^:dynamic matrix_to_string_i nil)

(def ^:dynamic matrix_to_string_s nil)

(def ^:dynamic update_i nil)

(def ^:dynamic update_weights nil)

(defn get_winner [get_winner_weights get_winner_sample]
  (binding [get_winner_d0 nil get_winner_d1 nil get_winner_diff0 nil get_winner_diff1 nil get_winner_i nil] (try (do (set! get_winner_d0 0.0) (set! get_winner_d1 0.0) (dotimes [get_winner_i (count get_winner_sample)] (do (set! get_winner_diff0 (- (nth get_winner_sample get_winner_i) (nth (nth get_winner_weights 0) get_winner_i))) (set! get_winner_diff1 (- (nth get_winner_sample get_winner_i) (nth (nth get_winner_weights 1) get_winner_i))) (set! get_winner_d0 (+ get_winner_d0 (* get_winner_diff0 get_winner_diff0))) (set! get_winner_d1 (+ get_winner_d1 (* get_winner_diff1 get_winner_diff1))) (throw (ex-info "return" {:v (if (> get_winner_d0 get_winner_d1) 0 1)})))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn update [update_weights_p update_sample update_j update_alpha]
  (binding [update_weights update_weights_p update_i nil] (try (do (dotimes [update_i (count update_weights)] (set! update_weights (assoc-in update_weights [update_j update_i] (+ (nth (nth update_weights update_j) update_i) (* update_alpha (- (nth update_sample update_i) (nth (nth update_weights update_j) update_i))))))) (throw (ex-info "return" {:v update_weights}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var update_weights) (constantly update_weights))))))

(defn list_to_string [list_to_string_xs]
  (binding [list_to_string_i nil list_to_string_s nil] (try (do (set! list_to_string_s "[") (set! list_to_string_i 0) (while (< list_to_string_i (count list_to_string_xs)) (do (set! list_to_string_s (str list_to_string_s (str (nth list_to_string_xs list_to_string_i)))) (when (< list_to_string_i (- (count list_to_string_xs) 1)) (set! list_to_string_s (str list_to_string_s ", "))) (set! list_to_string_i (+ list_to_string_i 1)))) (set! list_to_string_s (str list_to_string_s "]")) (throw (ex-info "return" {:v list_to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_to_string [matrix_to_string_m]
  (binding [matrix_to_string_i nil matrix_to_string_s nil] (try (do (set! matrix_to_string_s "[") (set! matrix_to_string_i 0) (while (< matrix_to_string_i (count matrix_to_string_m)) (do (set! matrix_to_string_s (str matrix_to_string_s (list_to_string (nth matrix_to_string_m matrix_to_string_i)))) (when (< matrix_to_string_i (- (count matrix_to_string_m) 1)) (set! matrix_to_string_s (str matrix_to_string_s ", "))) (set! matrix_to_string_i (+ matrix_to_string_i 1)))) (set! matrix_to_string_s (str matrix_to_string_s "]")) (throw (ex-info "return" {:v matrix_to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main__ nil main_alpha nil main_epochs nil main_j nil main_sample nil main_training_samples nil main_weights nil main_winner nil] (do (set! main_training_samples [[1 1 0 0] [0 0 0 1] [1 0 0 0] [0 0 1 1]]) (set! main_weights [[0.2 0.6 0.5 0.9] [0.8 0.4 0.7 0.3]]) (set! main_epochs 3) (set! main_alpha 0.5) (dotimes [main__ main_epochs] (dotimes [main_j (count main_training_samples)] (do (set! main_sample (nth main_training_samples main_j)) (set! main_winner (get_winner main_weights main_sample)) (set! main_weights (let [__res (update main_weights main_sample main_winner main_alpha)] (do (set! main_weights update_weights) __res)))))) (set! main_sample [0 0 0 1]) (set! main_winner (get_winner main_weights main_sample)) (println (str "Clusters that the test sample belongs to : " (str main_winner))) (println (str "Weights that have been trained : " (matrix_to_string main_weights))))))

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
