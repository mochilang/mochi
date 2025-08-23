(ns main (:refer-clojure :exclude [join_digits digits_to_int contains_int remove_at is_combination_valid search]))

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

(declare join_digits digits_to_int contains_int remove_at is_combination_valid search)

(declare _read_file)

(def ^:dynamic contains_int_i nil)

(def ^:dynamic is_combination_valid_mul1 nil)

(def ^:dynamic is_combination_valid_mul2 nil)

(def ^:dynamic is_combination_valid_mul3 nil)

(def ^:dynamic is_combination_valid_mul4 nil)

(def ^:dynamic is_combination_valid_prod nil)

(def ^:dynamic join_digits_i nil)

(def ^:dynamic join_digits_s nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_products nil)

(def ^:dynamic main_total nil)

(def ^:dynamic remove_at_i nil)

(def ^:dynamic remove_at_res nil)

(def ^:dynamic search_i nil)

(def ^:dynamic search_next_prefix nil)

(def ^:dynamic search_next_remaining nil)

(def ^:dynamic search_p nil)

(def ^:dynamic search_products nil)

(defn join_digits [join_digits_xs]
  (binding [join_digits_i nil join_digits_s nil] (try (do (set! join_digits_s "") (set! join_digits_i 0) (while (< join_digits_i (count join_digits_xs)) (do (set! join_digits_s (str join_digits_s (nth join_digits_xs join_digits_i))) (set! join_digits_i (+ join_digits_i 1)))) (throw (ex-info "return" {:v join_digits_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn digits_to_int [digits_to_int_xs]
  (try (throw (ex-info "return" {:v (toi (join_digits digits_to_int_xs))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn contains_int [contains_int_xs contains_int_value]
  (binding [contains_int_i nil] (try (do (set! contains_int_i 0) (while (< contains_int_i (count contains_int_xs)) (do (when (= (nth contains_int_xs contains_int_i) contains_int_value) (throw (ex-info "return" {:v true}))) (set! contains_int_i (+ contains_int_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_at [remove_at_xs remove_at_idx]
  (binding [remove_at_i nil remove_at_res nil] (try (do (set! remove_at_res []) (set! remove_at_i 0) (while (< remove_at_i (count remove_at_xs)) (do (when (not= remove_at_i remove_at_idx) (set! remove_at_res (conj remove_at_res (nth remove_at_xs remove_at_i)))) (set! remove_at_i (+ remove_at_i 1)))) (throw (ex-info "return" {:v remove_at_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_combination_valid [is_combination_valid_comb]
  (binding [is_combination_valid_mul1 nil is_combination_valid_mul2 nil is_combination_valid_mul3 nil is_combination_valid_mul4 nil is_combination_valid_prod nil] (try (do (set! is_combination_valid_prod (digits_to_int (subvec is_combination_valid_comb 5 9))) (set! is_combination_valid_mul2 (digits_to_int (subvec is_combination_valid_comb 0 2))) (set! is_combination_valid_mul3 (digits_to_int (subvec is_combination_valid_comb 2 5))) (when (= (* is_combination_valid_mul2 is_combination_valid_mul3) is_combination_valid_prod) (throw (ex-info "return" {:v true}))) (set! is_combination_valid_mul1 (digits_to_int (subvec is_combination_valid_comb 0 1))) (set! is_combination_valid_mul4 (digits_to_int (subvec is_combination_valid_comb 1 5))) (throw (ex-info "return" {:v (= (* is_combination_valid_mul1 is_combination_valid_mul4) is_combination_valid_prod)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn search [search_prefix search_remaining search_products_p]
  (binding [search_products search_products_p search_i nil search_next_prefix nil search_next_remaining nil search_p nil] (try (do (when (= (count search_remaining) 0) (do (when (is_combination_valid search_prefix) (do (set! search_p (digits_to_int (subvec search_prefix 5 9))) (when (not (contains_int search_products search_p)) (set! search_products (conj search_products search_p))))) (throw (ex-info "return" {:v search_products})))) (set! search_i 0) (while (< search_i (count search_remaining)) (do (set! search_next_prefix (conj search_prefix (nth search_remaining search_i))) (set! search_next_remaining (remove_at search_remaining search_i)) (set! search_products (let [__res (search search_next_prefix search_next_remaining search_products)] (do (set! search_products search_products) __res))) (set! search_i (+ search_i 1)))) (throw (ex-info "return" {:v search_products}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var search_products) (constantly search_products))))))

(def ^:dynamic main_digits nil)

(def ^:dynamic main_products nil)

(def ^:dynamic main_total nil)

(def ^:dynamic main_i nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_digits) (constantly ["1" "2" "3" "4" "5" "6" "7" "8" "9"]))
      (alter-var-root (var main_products) (constantly []))
      (alter-var-root (var main_products) (constantly (let [__res (search [] main_digits main_products)] (do (alter-var-root (var main_products) (constantly search_products)) __res))))
      (alter-var-root (var main_total) (constantly 0))
      (alter-var-root (var main_i) (constantly 0))
      (while (< main_i (count main_products)) (do (alter-var-root (var main_total) (constantly (+ main_total (nth main_products main_i)))) (alter-var-root (var main_i) (constantly (+ main_i 1)))))
      (println (mochi_str main_total))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
