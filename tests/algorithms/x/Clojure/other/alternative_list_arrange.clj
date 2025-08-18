(ns main (:refer-clojure :exclude [from_int from_string item_to_string alternative_list_arrange list_to_string]))

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

(declare from_int from_string item_to_string alternative_list_arrange list_to_string)

(def ^:dynamic alternative_list_arrange_abs_len nil)

(def ^:dynamic alternative_list_arrange_i nil)

(def ^:dynamic alternative_list_arrange_len1 nil)

(def ^:dynamic alternative_list_arrange_len2 nil)

(def ^:dynamic alternative_list_arrange_result nil)

(def ^:dynamic list_to_string_i nil)

(def ^:dynamic list_to_string_s nil)

(defn from_int [from_int_x]
  (try (throw (ex-info "return" {:v {:__tag "Int" :value from_int_x}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn from_string [from_string_s]
  (try (throw (ex-info "return" {:v {:__tag "Str" :value from_string_s}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn item_to_string [item_to_string_it]
  (try (throw (ex-info "return" {:v (cond (and (map? item_to_string_it) (= (:__tag item_to_string_it) "Int") (contains? item_to_string_it :value)) (let [item_to_string_v (:value item_to_string_it)] (mochi_str item_to_string_v)) (and (map? item_to_string_it) (= (:__tag item_to_string_it) "Str") (contains? item_to_string_it :value)) (let [item_to_string_s (:value item_to_string_it)] item_to_string_s))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn alternative_list_arrange [first_v alternative_list_arrange_second]
  (binding [alternative_list_arrange_abs_len nil alternative_list_arrange_i nil alternative_list_arrange_len1 nil alternative_list_arrange_len2 nil alternative_list_arrange_result nil] (try (do (set! alternative_list_arrange_len1 (count first_v)) (set! alternative_list_arrange_len2 (count alternative_list_arrange_second)) (set! alternative_list_arrange_abs_len (if (> alternative_list_arrange_len1 alternative_list_arrange_len2) alternative_list_arrange_len1 alternative_list_arrange_len2)) (set! alternative_list_arrange_result []) (set! alternative_list_arrange_i 0) (while (< alternative_list_arrange_i alternative_list_arrange_abs_len) (do (when (< alternative_list_arrange_i alternative_list_arrange_len1) (set! alternative_list_arrange_result (conj alternative_list_arrange_result (nth first_v alternative_list_arrange_i)))) (when (< alternative_list_arrange_i alternative_list_arrange_len2) (set! alternative_list_arrange_result (conj alternative_list_arrange_result (nth alternative_list_arrange_second alternative_list_arrange_i)))) (set! alternative_list_arrange_i (+ alternative_list_arrange_i 1)))) (throw (ex-info "return" {:v alternative_list_arrange_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn list_to_string [list_to_string_xs]
  (binding [list_to_string_i nil list_to_string_s nil] (try (do (set! list_to_string_s "[") (set! list_to_string_i 0) (while (< list_to_string_i (count list_to_string_xs)) (do (set! list_to_string_s (str list_to_string_s (item_to_string (nth list_to_string_xs list_to_string_i)))) (when (< list_to_string_i (- (count list_to_string_xs) 1)) (set! list_to_string_s (str list_to_string_s ", "))) (set! list_to_string_i (+ list_to_string_i 1)))) (set! list_to_string_s (str list_to_string_s "]")) (throw (ex-info "return" {:v list_to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_example1 nil)

(def ^:dynamic main_example2 nil)

(def ^:dynamic main_example3 nil)

(def ^:dynamic main_example4 nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_example1) (constantly (alternative_list_arrange [(from_int 1) (from_int 2) (from_int 3) (from_int 4) (from_int 5)] [(from_string "A") (from_string "B") (from_string "C")])))
      (println (list_to_string main_example1))
      (alter-var-root (var main_example2) (constantly (alternative_list_arrange [(from_string "A") (from_string "B") (from_string "C")] [(from_int 1) (from_int 2) (from_int 3) (from_int 4) (from_int 5)])))
      (println (list_to_string main_example2))
      (alter-var-root (var main_example3) (constantly (alternative_list_arrange [(from_string "X") (from_string "Y") (from_string "Z")] [(from_int 9) (from_int 8) (from_int 7) (from_int 6)])))
      (println (list_to_string main_example3))
      (alter-var-root (var main_example4) (constantly (alternative_list_arrange [(from_int 1) (from_int 2) (from_int 3) (from_int 4) (from_int 5)] [])))
      (println (list_to_string main_example4))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
