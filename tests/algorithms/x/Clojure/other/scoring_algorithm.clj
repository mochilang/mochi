(ns main (:refer-clojure :exclude [get_data calculate_each_score generate_final_scores procentual_proximity]))

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

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare get_data calculate_each_score generate_final_scores procentual_proximity)

(def ^:dynamic calculate_each_score_dlist nil)

(def ^:dynamic calculate_each_score_i nil)

(def ^:dynamic calculate_each_score_item nil)

(def ^:dynamic calculate_each_score_j nil)

(def ^:dynamic calculate_each_score_maxd nil)

(def ^:dynamic calculate_each_score_mind nil)

(def ^:dynamic calculate_each_score_score nil)

(def ^:dynamic calculate_each_score_score_lists nil)

(def ^:dynamic calculate_each_score_val nil)

(def ^:dynamic calculate_each_score_weight nil)

(def ^:dynamic count_v nil)

(def ^:dynamic generate_final_scores_final_scores nil)

(def ^:dynamic generate_final_scores_i nil)

(def ^:dynamic generate_final_scores_j nil)

(def ^:dynamic generate_final_scores_slist nil)

(def ^:dynamic get_data_data_lists nil)

(def ^:dynamic get_data_empty nil)

(def ^:dynamic get_data_i nil)

(def ^:dynamic get_data_j nil)

(def ^:dynamic get_data_row nil)

(def ^:dynamic main_vehicles nil)

(def ^:dynamic procentual_proximity_data_lists nil)

(def ^:dynamic procentual_proximity_final_scores nil)

(def ^:dynamic procentual_proximity_i nil)

(def ^:dynamic procentual_proximity_score_lists nil)

(def ^:dynamic procentual_proximity_source_data nil)

(defn get_data [get_data_source_data]
  (binding [get_data_data_lists nil get_data_empty nil get_data_i nil get_data_j nil get_data_row nil] (try (do (set! get_data_data_lists []) (set! get_data_i 0) (while (< get_data_i (count get_data_source_data)) (do (set! get_data_row (nth get_data_source_data get_data_i)) (set! get_data_j 0) (while (< get_data_j (count get_data_row)) (do (when (< (count get_data_data_lists) (+ get_data_j 1)) (do (set! get_data_empty []) (set! get_data_data_lists (conj get_data_data_lists get_data_empty)))) (set! get_data_data_lists (assoc get_data_data_lists get_data_j (conj (nth get_data_data_lists get_data_j) (nth get_data_row get_data_j)))) (set! get_data_j (+ get_data_j 1)))) (set! get_data_i (+ get_data_i 1)))) (throw (ex-info "return" {:v get_data_data_lists}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn calculate_each_score [calculate_each_score_data_lists calculate_each_score_weights]
  (binding [calculate_each_score_dlist nil calculate_each_score_i nil calculate_each_score_item nil calculate_each_score_j nil calculate_each_score_maxd nil calculate_each_score_mind nil calculate_each_score_score nil calculate_each_score_score_lists nil calculate_each_score_val nil calculate_each_score_weight nil] (try (do (set! calculate_each_score_score_lists []) (set! calculate_each_score_i 0) (while (< calculate_each_score_i (count calculate_each_score_data_lists)) (do (set! calculate_each_score_dlist (nth calculate_each_score_data_lists calculate_each_score_i)) (set! calculate_each_score_weight (nth calculate_each_score_weights calculate_each_score_i)) (set! calculate_each_score_mind (nth calculate_each_score_dlist 0)) (set! calculate_each_score_maxd (nth calculate_each_score_dlist 0)) (set! calculate_each_score_j 1) (while (< calculate_each_score_j (count calculate_each_score_dlist)) (do (set! calculate_each_score_val (nth calculate_each_score_dlist calculate_each_score_j)) (when (< calculate_each_score_val calculate_each_score_mind) (set! calculate_each_score_mind calculate_each_score_val)) (when (> calculate_each_score_val calculate_each_score_maxd) (set! calculate_each_score_maxd calculate_each_score_val)) (set! calculate_each_score_j (+ calculate_each_score_j 1)))) (set! calculate_each_score_score []) (set! calculate_each_score_j 0) (if (= calculate_each_score_weight 0) (while (< calculate_each_score_j (count calculate_each_score_dlist)) (do (set! calculate_each_score_item (nth calculate_each_score_dlist calculate_each_score_j)) (if (= (- calculate_each_score_maxd calculate_each_score_mind) 0.0) (set! calculate_each_score_score (conj calculate_each_score_score 1.0)) (set! calculate_each_score_score (conj calculate_each_score_score (- 1.0 (quot (- calculate_each_score_item calculate_each_score_mind) (- calculate_each_score_maxd calculate_each_score_mind)))))) (set! calculate_each_score_j (+ calculate_each_score_j 1)))) (while (< calculate_each_score_j (count calculate_each_score_dlist)) (do (set! calculate_each_score_item (nth calculate_each_score_dlist calculate_each_score_j)) (if (= (- calculate_each_score_maxd calculate_each_score_mind) 0.0) (set! calculate_each_score_score (conj calculate_each_score_score 0.0)) (set! calculate_each_score_score (conj calculate_each_score_score (quot (- calculate_each_score_item calculate_each_score_mind) (- calculate_each_score_maxd calculate_each_score_mind))))) (set! calculate_each_score_j (+ calculate_each_score_j 1))))) (set! calculate_each_score_score_lists (conj calculate_each_score_score_lists calculate_each_score_score)) (set! calculate_each_score_i (+ calculate_each_score_i 1)))) (throw (ex-info "return" {:v calculate_each_score_score_lists}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn generate_final_scores [generate_final_scores_score_lists]
  (binding [count_v nil generate_final_scores_final_scores nil generate_final_scores_i nil generate_final_scores_j nil generate_final_scores_slist nil] (try (do (set! count_v (count (nth generate_final_scores_score_lists 0))) (set! generate_final_scores_final_scores []) (set! generate_final_scores_i 0) (while (< generate_final_scores_i count_v) (do (set! generate_final_scores_final_scores (conj generate_final_scores_final_scores 0.0)) (set! generate_final_scores_i (+ generate_final_scores_i 1)))) (set! generate_final_scores_i 0) (while (< generate_final_scores_i (count generate_final_scores_score_lists)) (do (set! generate_final_scores_slist (nth generate_final_scores_score_lists generate_final_scores_i)) (set! generate_final_scores_j 0) (while (< generate_final_scores_j (count generate_final_scores_slist)) (do (set! generate_final_scores_final_scores (assoc generate_final_scores_final_scores generate_final_scores_j (+ (nth generate_final_scores_final_scores generate_final_scores_j) (nth generate_final_scores_slist generate_final_scores_j)))) (set! generate_final_scores_j (+ generate_final_scores_j 1)))) (set! generate_final_scores_i (+ generate_final_scores_i 1)))) (throw (ex-info "return" {:v generate_final_scores_final_scores}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn procentual_proximity [procentual_proximity_source_data_p procentual_proximity_weights]
  (binding [procentual_proximity_source_data procentual_proximity_source_data_p procentual_proximity_data_lists nil procentual_proximity_final_scores nil procentual_proximity_i nil procentual_proximity_score_lists nil] (try (do (set! procentual_proximity_data_lists (get_data procentual_proximity_source_data)) (set! procentual_proximity_score_lists (calculate_each_score procentual_proximity_data_lists procentual_proximity_weights)) (set! procentual_proximity_final_scores (generate_final_scores procentual_proximity_score_lists)) (set! procentual_proximity_i 0) (while (< procentual_proximity_i (count procentual_proximity_final_scores)) (do (set! procentual_proximity_source_data (assoc procentual_proximity_source_data procentual_proximity_i (conj (nth procentual_proximity_source_data procentual_proximity_i) (nth procentual_proximity_final_scores procentual_proximity_i)))) (set! procentual_proximity_i (+ procentual_proximity_i 1)))) (throw (ex-info "return" {:v procentual_proximity_source_data}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var procentual_proximity_source_data) (constantly procentual_proximity_source_data))))))

(def ^:dynamic main_vehicles [])

(def ^:dynamic main_weights [0 0 1])

(def ^:dynamic main_result nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_vehicles) (constantly (conj main_vehicles [20.0 60.0 2012.0])))
      (alter-var-root (var main_vehicles) (constantly (conj main_vehicles [23.0 90.0 2015.0])))
      (alter-var-root (var main_vehicles) (constantly (conj main_vehicles [22.0 50.0 2011.0])))
      (alter-var-root (var main_result) (constantly (let [__res (procentual_proximity main_vehicles main_weights)] (do (alter-var-root (var main_vehicles) (constantly procentual_proximity_source_data)) __res))))
      (println (mochi_str main_result))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
