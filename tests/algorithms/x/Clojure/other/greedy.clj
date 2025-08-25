(ns main (:refer-clojure :exclude [get_value get_weight get_name value_weight build_menu sort_desc greedy thing_to_string list_to_string]))

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
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare get_value get_weight get_name value_weight build_menu sort_desc greedy thing_to_string list_to_string)

(declare _read_file)

(def ^:dynamic build_menu_i nil)

(def ^:dynamic build_menu_menu nil)

(def ^:dynamic greedy_i nil)

(def ^:dynamic greedy_it nil)

(def ^:dynamic greedy_items_copy nil)

(def ^:dynamic greedy_result nil)

(def ^:dynamic greedy_total_cost nil)

(def ^:dynamic greedy_total_value nil)

(def ^:dynamic greedy_w nil)

(def ^:dynamic list_to_string_i nil)

(def ^:dynamic list_to_string_s nil)

(def ^:dynamic sort_desc_arr nil)

(def ^:dynamic sort_desc_i nil)

(def ^:dynamic sort_desc_j nil)

(def ^:dynamic sort_desc_k nil)

(def ^:dynamic sort_desc_key_item nil)

(def ^:dynamic sort_desc_key_val nil)

(defn get_value [get_value_t]
  (try (throw (ex-info "return" {:v (:value get_value_t)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_weight [get_weight_t]
  (try (throw (ex-info "return" {:v (:weight get_weight_t)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_name [get_name_t]
  (try (throw (ex-info "return" {:v (:name get_name_t)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn value_weight [value_weight_t]
  (try (throw (ex-info "return" {:v (/ (:value value_weight_t) (:weight value_weight_t))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn build_menu [build_menu_names build_menu_values build_menu_weights]
  (binding [build_menu_i nil build_menu_menu nil] (try (do (set! build_menu_menu []) (set! build_menu_i 0) (while (and (and (< build_menu_i (count build_menu_values)) (< build_menu_i (count build_menu_names))) (< build_menu_i (count build_menu_weights))) (do (set! build_menu_menu (conj build_menu_menu {:name (nth build_menu_names build_menu_i) :value (nth build_menu_values build_menu_i) :weight (nth build_menu_weights build_menu_i)})) (set! build_menu_i (+' build_menu_i 1)))) (throw (ex-info "return" {:v build_menu_menu}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sort_desc [sort_desc_items sort_desc_key_func]
  (binding [sort_desc_arr nil sort_desc_i nil sort_desc_j nil sort_desc_k nil sort_desc_key_item nil sort_desc_key_val nil] (try (do (set! sort_desc_arr []) (set! sort_desc_i 0) (while (< sort_desc_i (count sort_desc_items)) (do (set! sort_desc_arr (conj sort_desc_arr (nth sort_desc_items sort_desc_i))) (set! sort_desc_i (+' sort_desc_i 1)))) (set! sort_desc_j 1) (while (< sort_desc_j (count sort_desc_arr)) (do (set! sort_desc_key_item (nth sort_desc_arr sort_desc_j)) (set! sort_desc_key_val (sort_desc_key_func sort_desc_key_item)) (set! sort_desc_k (- sort_desc_j 1)) (while (and (>= sort_desc_k 0) (< (sort_desc_key_func (nth sort_desc_arr sort_desc_k)) sort_desc_key_val)) (do (set! sort_desc_arr (assoc sort_desc_arr (+' sort_desc_k 1) (nth sort_desc_arr sort_desc_k))) (set! sort_desc_k (- sort_desc_k 1)))) (set! sort_desc_arr (assoc sort_desc_arr (+' sort_desc_k 1) sort_desc_key_item)) (set! sort_desc_j (+' sort_desc_j 1)))) (throw (ex-info "return" {:v sort_desc_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn greedy [greedy_items greedy_max_cost greedy_key_func]
  (binding [greedy_i nil greedy_it nil greedy_items_copy nil greedy_result nil greedy_total_cost nil greedy_total_value nil greedy_w nil] (try (do (set! greedy_items_copy (sort_desc greedy_items greedy_key_func)) (set! greedy_result []) (set! greedy_total_value 0.0) (set! greedy_total_cost 0.0) (set! greedy_i 0) (while (< greedy_i (count greedy_items_copy)) (do (set! greedy_it (nth greedy_items_copy greedy_i)) (set! greedy_w (get_weight greedy_it)) (when (<= (+' greedy_total_cost greedy_w) greedy_max_cost) (do (set! greedy_result (conj greedy_result greedy_it)) (set! greedy_total_cost (+' greedy_total_cost greedy_w)) (set! greedy_total_value (+' greedy_total_value (get_value greedy_it))))) (set! greedy_i (+' greedy_i 1)))) (throw (ex-info "return" {:v {:items greedy_result :total_value greedy_total_value}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn thing_to_string [thing_to_string_t]
  (try (throw (ex-info "return" {:v (str (str (str (str (str (str "Thing(" (:name thing_to_string_t)) ", ") (mochi_str (:value thing_to_string_t))) ", ") (mochi_str (:weight thing_to_string_t))) ")")})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn list_to_string [list_to_string_ts]
  (binding [list_to_string_i nil list_to_string_s nil] (try (do (set! list_to_string_s "[") (set! list_to_string_i 0) (while (< list_to_string_i (count list_to_string_ts)) (do (set! list_to_string_s (str list_to_string_s (thing_to_string (nth list_to_string_ts list_to_string_i)))) (when (< list_to_string_i (- (count list_to_string_ts) 1)) (set! list_to_string_s (str list_to_string_s ", "))) (set! list_to_string_i (+' list_to_string_i 1)))) (set! list_to_string_s (str list_to_string_s "]")) (throw (ex-info "return" {:v list_to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_food nil)

(def ^:dynamic main_value nil)

(def ^:dynamic main_weight nil)

(def ^:dynamic main_foods nil)

(def ^:dynamic main_res nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_food) (constantly ["Burger" "Pizza" "Coca Cola" "Rice" "Sambhar" "Chicken" "Fries" "Milk"]))
      (alter-var-root (var main_value) (constantly [80.0 100.0 60.0 70.0 50.0 110.0 90.0 60.0]))
      (alter-var-root (var main_weight) (constantly [40.0 60.0 40.0 70.0 100.0 85.0 55.0 70.0]))
      (alter-var-root (var main_foods) (constantly (build_menu main_food main_value main_weight)))
      (println (list_to_string main_foods))
      (alter-var-root (var main_res) (constantly (greedy main_foods 500.0 get_value)))
      (println (list_to_string (:items main_res)))
      (println (mochi_str (:total_value main_res)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
