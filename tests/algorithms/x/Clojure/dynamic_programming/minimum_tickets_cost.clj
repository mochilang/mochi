(ns main (:refer-clojure :exclude [make_list max_int min_int min3 minimum_tickets_cost]))

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

(declare make_list max_int min_int min3 minimum_tickets_cost)

(def ^:dynamic make_list_arr nil)

(def ^:dynamic make_list_i nil)

(def ^:dynamic minimum_tickets_cost_cost1 nil)

(def ^:dynamic minimum_tickets_cost_cost30 nil)

(def ^:dynamic minimum_tickets_cost_cost7 nil)

(def ^:dynamic minimum_tickets_cost_d nil)

(def ^:dynamic minimum_tickets_cost_day_index nil)

(def ^:dynamic minimum_tickets_cost_dp nil)

(def ^:dynamic minimum_tickets_cost_last_day nil)

(defn make_list [make_list_len make_list_value]
  (binding [make_list_arr nil make_list_i nil] (try (do (set! make_list_arr []) (set! make_list_i 0) (while (< make_list_i make_list_len) (do (set! make_list_arr (conj make_list_arr make_list_value)) (set! make_list_i (+ make_list_i 1)))) (throw (ex-info "return" {:v make_list_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn max_int [max_int_a max_int_b]
  (try (if (> max_int_a max_int_b) (throw (ex-info "return" {:v max_int_a})) (throw (ex-info "return" {:v max_int_b}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn min_int [min_int_a min_int_b]
  (try (if (< min_int_a min_int_b) (throw (ex-info "return" {:v min_int_a})) (throw (ex-info "return" {:v min_int_b}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn min3 [min3_a min3_b min3_c]
  (try (throw (ex-info "return" {:v (min_int (min_int min3_a min3_b) min3_c)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn minimum_tickets_cost [minimum_tickets_cost_days minimum_tickets_cost_costs]
  (binding [minimum_tickets_cost_cost1 nil minimum_tickets_cost_cost30 nil minimum_tickets_cost_cost7 nil minimum_tickets_cost_d nil minimum_tickets_cost_day_index nil minimum_tickets_cost_dp nil minimum_tickets_cost_last_day nil] (try (do (when (= (count minimum_tickets_cost_days) 0) (throw (ex-info "return" {:v 0}))) (set! minimum_tickets_cost_last_day (nth minimum_tickets_cost_days (- (count minimum_tickets_cost_days) 1))) (set! minimum_tickets_cost_dp (make_list (+ minimum_tickets_cost_last_day 1) 0)) (set! minimum_tickets_cost_day_index 0) (set! minimum_tickets_cost_d 1) (while (<= minimum_tickets_cost_d minimum_tickets_cost_last_day) (do (if (and (< minimum_tickets_cost_day_index (count minimum_tickets_cost_days)) (= minimum_tickets_cost_d (nth minimum_tickets_cost_days minimum_tickets_cost_day_index))) (do (set! minimum_tickets_cost_cost1 (+ (nth minimum_tickets_cost_dp (- minimum_tickets_cost_d 1)) (nth minimum_tickets_cost_costs 0))) (set! minimum_tickets_cost_cost7 (+ (nth minimum_tickets_cost_dp (max_int 0 (- minimum_tickets_cost_d 7))) (nth minimum_tickets_cost_costs 1))) (set! minimum_tickets_cost_cost30 (+ (nth minimum_tickets_cost_dp (max_int 0 (- minimum_tickets_cost_d 30))) (nth minimum_tickets_cost_costs 2))) (set! minimum_tickets_cost_dp (assoc minimum_tickets_cost_dp minimum_tickets_cost_d (min3 minimum_tickets_cost_cost1 minimum_tickets_cost_cost7 minimum_tickets_cost_cost30))) (set! minimum_tickets_cost_day_index (+ minimum_tickets_cost_day_index 1))) (set! minimum_tickets_cost_dp (assoc minimum_tickets_cost_dp minimum_tickets_cost_d (nth minimum_tickets_cost_dp (- minimum_tickets_cost_d 1))))) (set! minimum_tickets_cost_d (+ minimum_tickets_cost_d 1)))) (throw (ex-info "return" {:v (nth minimum_tickets_cost_dp minimum_tickets_cost_last_day)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (minimum_tickets_cost [1 4 6 7 8 20] [2 7 15])))
      (println (str (minimum_tickets_cost [1 2 3 4 5 6 7 8 9 10 30 31] [2 7 15])))
      (println (str (minimum_tickets_cost [1 2 3 4 5 6 7 8 9 10 30 31] [2 90 150])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
