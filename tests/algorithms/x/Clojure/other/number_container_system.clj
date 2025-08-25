(ns main (:refer-clojure :exclude [remove_at insert_at binary_search_delete binary_search_insert change find]))

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

(declare remove_at insert_at binary_search_delete binary_search_insert change find)

(declare _read_file)

(def ^:dynamic binary_search_delete_arr nil)

(def ^:dynamic binary_search_delete_high nil)

(def ^:dynamic binary_search_delete_low nil)

(def ^:dynamic binary_search_delete_mid nil)

(def ^:dynamic binary_search_insert_arr nil)

(def ^:dynamic binary_search_insert_high nil)

(def ^:dynamic binary_search_insert_low nil)

(def ^:dynamic binary_search_insert_mid nil)

(def ^:dynamic change_indexes nil)

(def ^:dynamic change_indexmap nil)

(def ^:dynamic change_numbermap nil)

(def ^:dynamic change_old nil)

(def ^:dynamic find_arr nil)

(def ^:dynamic find_numbermap nil)

(def ^:dynamic insert_at_i nil)

(def ^:dynamic insert_at_res nil)

(def ^:dynamic main_cont nil)

(def ^:dynamic remove_at_i nil)

(def ^:dynamic remove_at_res nil)

(defn remove_at [remove_at_xs remove_at_idx]
  (binding [remove_at_i nil remove_at_res nil] (try (do (set! remove_at_res []) (set! remove_at_i 0) (while (< remove_at_i (count remove_at_xs)) (do (when (not= remove_at_i remove_at_idx) (set! remove_at_res (conj remove_at_res (nth remove_at_xs remove_at_i)))) (set! remove_at_i (+' remove_at_i 1)))) (throw (ex-info "return" {:v remove_at_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn insert_at [insert_at_xs insert_at_idx insert_at_val]
  (binding [insert_at_i nil insert_at_res nil] (try (do (set! insert_at_res []) (set! insert_at_i 0) (while (< insert_at_i (count insert_at_xs)) (do (when (= insert_at_i insert_at_idx) (set! insert_at_res (conj insert_at_res insert_at_val))) (set! insert_at_res (conj insert_at_res (nth insert_at_xs insert_at_i))) (set! insert_at_i (+' insert_at_i 1)))) (when (= insert_at_idx (count insert_at_xs)) (set! insert_at_res (conj insert_at_res insert_at_val))) (throw (ex-info "return" {:v insert_at_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn binary_search_delete [binary_search_delete_array binary_search_delete_item]
  (binding [binary_search_delete_arr nil binary_search_delete_high nil binary_search_delete_low nil binary_search_delete_mid nil] (try (do (set! binary_search_delete_low 0) (set! binary_search_delete_high (- (count binary_search_delete_array) 1)) (set! binary_search_delete_arr binary_search_delete_array) (while (<= binary_search_delete_low binary_search_delete_high) (do (set! binary_search_delete_mid (/ (+' binary_search_delete_low binary_search_delete_high) 2)) (if (= (nth binary_search_delete_arr binary_search_delete_mid) binary_search_delete_item) (do (set! binary_search_delete_arr (remove_at binary_search_delete_arr binary_search_delete_mid)) (throw (ex-info "return" {:v binary_search_delete_arr}))) (if (< (nth binary_search_delete_arr binary_search_delete_mid) binary_search_delete_item) (set! binary_search_delete_low (+' binary_search_delete_mid 1)) (set! binary_search_delete_high (- binary_search_delete_mid 1)))))) (println "ValueError: Either the item is not in the array or the array was unsorted") (throw (ex-info "return" {:v binary_search_delete_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn binary_search_insert [binary_search_insert_array binary_search_insert_index]
  (binding [binary_search_insert_arr nil binary_search_insert_high nil binary_search_insert_low nil binary_search_insert_mid nil] (try (do (set! binary_search_insert_low 0) (set! binary_search_insert_high (- (count binary_search_insert_array) 1)) (set! binary_search_insert_arr binary_search_insert_array) (while (<= binary_search_insert_low binary_search_insert_high) (do (set! binary_search_insert_mid (/ (+' binary_search_insert_low binary_search_insert_high) 2)) (if (= (nth binary_search_insert_arr binary_search_insert_mid) binary_search_insert_index) (do (set! binary_search_insert_arr (insert_at binary_search_insert_arr (+' binary_search_insert_mid 1) binary_search_insert_index)) (throw (ex-info "return" {:v binary_search_insert_arr}))) (if (< (nth binary_search_insert_arr binary_search_insert_mid) binary_search_insert_index) (set! binary_search_insert_low (+' binary_search_insert_mid 1)) (set! binary_search_insert_high (- binary_search_insert_mid 1)))))) (set! binary_search_insert_arr (insert_at binary_search_insert_arr binary_search_insert_low binary_search_insert_index)) (throw (ex-info "return" {:v binary_search_insert_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn change [change_cont change_idx change_num]
  (binding [change_indexes nil change_indexmap nil change_numbermap nil change_old nil] (try (do (set! change_numbermap (:numbermap change_cont)) (set! change_indexmap (:indexmap change_cont)) (when (in change_idx change_indexmap) (do (set! change_old (get change_indexmap change_idx)) (set! change_indexes (get change_numbermap change_old)) (if (= (count change_indexes) 1) (set! change_numbermap (assoc change_numbermap change_old [])) (set! change_numbermap (assoc change_numbermap change_old (binary_search_delete change_indexes change_idx)))))) (set! change_indexmap (assoc change_indexmap change_idx change_num)) (if (in change_num change_numbermap) (set! change_numbermap (assoc change_numbermap change_num (binary_search_insert (get change_numbermap change_num) change_idx))) (set! change_numbermap (assoc change_numbermap change_num [change_idx]))) (throw (ex-info "return" {:v {:indexmap change_indexmap :numbermap change_numbermap}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find [find_cont find_num]
  (binding [find_arr nil find_numbermap nil] (try (do (set! find_numbermap (:numbermap find_cont)) (when (in find_num find_numbermap) (do (set! find_arr (get find_numbermap find_num)) (when (> (count find_arr) 0) (throw (ex-info "return" {:v (get find_arr 0)}))))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_nm nil)

(def ^:dynamic main_im nil)

(def ^:dynamic main_cont nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_nm) (constantly {}))
      (alter-var-root (var main_im) (constantly {}))
      (alter-var-root (var main_cont) (constantly {:indexmap main_im :numbermap main_nm}))
      (println (find main_cont 10))
      (alter-var-root (var main_cont) (constantly (change main_cont 0 10)))
      (println (find main_cont 10))
      (alter-var-root (var main_cont) (constantly (change main_cont 0 20)))
      (println (find main_cont 10))
      (println (find main_cont 20))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
