(ns main (:refer-clojure :exclude [starts_with all_digits is_sri_lankan_phone_number]))

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

(declare starts_with all_digits is_sri_lankan_phone_number)

(def ^:dynamic all_digits_c nil)

(def ^:dynamic all_digits_i nil)

(def ^:dynamic is_sri_lankan_phone_number_allowed nil)

(def ^:dynamic is_sri_lankan_phone_number_idx nil)

(def ^:dynamic is_sri_lankan_phone_number_p nil)

(def ^:dynamic is_sri_lankan_phone_number_second nil)

(def ^:dynamic is_sri_lankan_phone_number_sep nil)

(def ^:dynamic rest_v nil)

(defn starts_with [starts_with_s starts_with_prefix]
  (try (if (< (count starts_with_s) (count starts_with_prefix)) false (= (subs starts_with_s 0 (min (count starts_with_prefix) (count starts_with_s))) starts_with_prefix)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn all_digits [all_digits_s]
  (binding [all_digits_c nil all_digits_i nil] (try (do (set! all_digits_i 0) (while (< all_digits_i (count all_digits_s)) (do (set! all_digits_c (subs all_digits_s all_digits_i (+ all_digits_i 1))) (when (or (< (compare all_digits_c "0") 0) (> (compare all_digits_c "9") 0)) (throw (ex-info "return" {:v false}))) (set! all_digits_i (+ all_digits_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_sri_lankan_phone_number [is_sri_lankan_phone_number_phone]
  (binding [is_sri_lankan_phone_number_allowed nil is_sri_lankan_phone_number_idx nil is_sri_lankan_phone_number_p nil is_sri_lankan_phone_number_second nil is_sri_lankan_phone_number_sep nil rest_v nil] (try (do (set! is_sri_lankan_phone_number_p is_sri_lankan_phone_number_phone) (if (starts_with is_sri_lankan_phone_number_p "+94") (set! is_sri_lankan_phone_number_p (subs is_sri_lankan_phone_number_p 3 (min (count is_sri_lankan_phone_number_p) (count is_sri_lankan_phone_number_p)))) (if (starts_with is_sri_lankan_phone_number_p "0094") (set! is_sri_lankan_phone_number_p (subs is_sri_lankan_phone_number_p 4 (min (count is_sri_lankan_phone_number_p) (count is_sri_lankan_phone_number_p)))) (if (starts_with is_sri_lankan_phone_number_p "94") (set! is_sri_lankan_phone_number_p (subs is_sri_lankan_phone_number_p 2 (min (count is_sri_lankan_phone_number_p) (count is_sri_lankan_phone_number_p)))) (if (starts_with is_sri_lankan_phone_number_p "0") (set! is_sri_lankan_phone_number_p (subs is_sri_lankan_phone_number_p 1 (min (count is_sri_lankan_phone_number_p) (count is_sri_lankan_phone_number_p)))) (throw (ex-info "return" {:v false})))))) (when (and (not= (count is_sri_lankan_phone_number_p) 9) (not= (count is_sri_lankan_phone_number_p) 10)) (throw (ex-info "return" {:v false}))) (when (not= (subs is_sri_lankan_phone_number_p 0 (+ 0 1)) "7") (throw (ex-info "return" {:v false}))) (set! is_sri_lankan_phone_number_second (subs is_sri_lankan_phone_number_p 1 (+ 1 1))) (set! is_sri_lankan_phone_number_allowed ["0" "1" "2" "4" "5" "6" "7" "8"]) (when (not (in is_sri_lankan_phone_number_second is_sri_lankan_phone_number_allowed)) (throw (ex-info "return" {:v false}))) (set! is_sri_lankan_phone_number_idx 2) (when (= (count is_sri_lankan_phone_number_p) 10) (do (set! is_sri_lankan_phone_number_sep (subs is_sri_lankan_phone_number_p 2 (+ 2 1))) (when (and (not= is_sri_lankan_phone_number_sep "-") (not= is_sri_lankan_phone_number_sep " ")) (throw (ex-info "return" {:v false}))) (set! is_sri_lankan_phone_number_idx 3))) (when (not= (- (count is_sri_lankan_phone_number_p) is_sri_lankan_phone_number_idx) 7) (throw (ex-info "return" {:v false}))) (set! rest_v (subs is_sri_lankan_phone_number_p is_sri_lankan_phone_number_idx (min (count is_sri_lankan_phone_number_p) (count is_sri_lankan_phone_number_p)))) (throw (ex-info "return" {:v (all_digits rest_v)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_phone "0094702343221")

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (is_sri_lankan_phone_number main_phone)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
