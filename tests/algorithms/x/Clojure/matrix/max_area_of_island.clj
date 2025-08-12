(ns main (:refer-clojure :exclude [encode is_safe has depth_first_search find_max_area]))

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

(declare encode is_safe has depth_first_search find_max_area)

(def ^:dynamic depth_first_search_cols nil)

(def ^:dynamic depth_first_search_key nil)

(def ^:dynamic depth_first_search_rows nil)

(def ^:dynamic depth_first_search_seen nil)

(def ^:dynamic find_max_area_area nil)

(def ^:dynamic find_max_area_c nil)

(def ^:dynamic find_max_area_cols nil)

(def ^:dynamic find_max_area_key nil)

(def ^:dynamic find_max_area_line nil)

(def ^:dynamic find_max_area_max_area nil)

(def ^:dynamic find_max_area_r nil)

(def ^:dynamic find_max_area_rows nil)

(def ^:dynamic find_max_area_seen nil)

(defn encode [encode_row encode_col]
  (try (throw (ex-info "return" {:v (str (str (str encode_row) ",") (str encode_col))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn is_safe [is_safe_row is_safe_col is_safe_rows is_safe_cols]
  (try (throw (ex-info "return" {:v (and (and (and (>= is_safe_row 0) (< is_safe_row is_safe_rows)) (>= is_safe_col 0)) (< is_safe_col is_safe_cols))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn has [has_seen has_key]
  (try (throw (ex-info "return" {:v (in has_key has_seen)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn depth_first_search [depth_first_search_row depth_first_search_col depth_first_search_seen_p depth_first_search_mat]
  (binding [depth_first_search_cols nil depth_first_search_key nil depth_first_search_rows nil depth_first_search_seen nil] (try (do (set! depth_first_search_seen depth_first_search_seen_p) (set! depth_first_search_rows (count depth_first_search_mat)) (set! depth_first_search_cols (count (nth depth_first_search_mat 0))) (set! depth_first_search_key (encode depth_first_search_row depth_first_search_col)) (if (and (and (is_safe depth_first_search_row depth_first_search_col depth_first_search_rows depth_first_search_cols) (not (has depth_first_search_seen depth_first_search_key))) (= (nth (nth depth_first_search_mat depth_first_search_row) depth_first_search_col) 1)) (do (set! depth_first_search_seen (assoc depth_first_search_seen depth_first_search_key true)) (throw (ex-info "return" {:v (+ (+ (+ (+ 1 (depth_first_search (+ depth_first_search_row 1) depth_first_search_col depth_first_search_seen depth_first_search_mat)) (depth_first_search (- depth_first_search_row 1) depth_first_search_col depth_first_search_seen depth_first_search_mat)) (depth_first_search depth_first_search_row (+ depth_first_search_col 1) depth_first_search_seen depth_first_search_mat)) (depth_first_search depth_first_search_row (- depth_first_search_col 1) depth_first_search_seen depth_first_search_mat))}))) (throw (ex-info "return" {:v 0})))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_max_area [find_max_area_mat]
  (binding [find_max_area_area nil find_max_area_c nil find_max_area_cols nil find_max_area_key nil find_max_area_line nil find_max_area_max_area nil find_max_area_r nil find_max_area_rows nil find_max_area_seen nil] (try (do (set! find_max_area_seen {}) (set! find_max_area_rows (count find_max_area_mat)) (set! find_max_area_max_area 0) (set! find_max_area_r 0) (while (< find_max_area_r find_max_area_rows) (do (set! find_max_area_line (nth find_max_area_mat find_max_area_r)) (set! find_max_area_cols (count find_max_area_line)) (set! find_max_area_c 0) (while (< find_max_area_c find_max_area_cols) (do (when (= (nth find_max_area_line find_max_area_c) 1) (do (set! find_max_area_key (encode find_max_area_r find_max_area_c)) (when (not (in find_max_area_key find_max_area_seen)) (do (set! find_max_area_area (depth_first_search find_max_area_r find_max_area_c find_max_area_seen find_max_area_mat)) (when (> find_max_area_area find_max_area_max_area) (set! find_max_area_max_area find_max_area_area)))))) (set! find_max_area_c (+ find_max_area_c 1)))) (set! find_max_area_r (+ find_max_area_r 1)))) (throw (ex-info "return" {:v find_max_area_max_area}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_matrix [[0 0 1 0 0 0 0 1 0 0 0 0 0] [0 0 0 0 0 0 0 1 1 1 0 0 0] [0 1 1 0 1 0 0 0 0 0 0 0 0] [0 1 0 0 1 1 0 0 1 0 1 0 0] [0 1 0 0 1 1 0 0 1 1 1 0 0] [0 0 0 0 0 0 0 0 0 0 1 0 0] [0 0 0 0 0 0 0 1 1 1 0 0 0] [0 0 0 0 0 0 0 1 1 0 0 0 0]])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (find_max_area main_matrix))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
