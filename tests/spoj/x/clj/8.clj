(ns main (:refer-clojure :exclude [split parse_ints main]))

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

(declare split parse_ints main)

(declare _read_file)

(def ^:dynamic main_arr nil)

(def ^:dynamic main_arrBelow nil)

(def ^:dynamic main_bottom nil)

(def ^:dynamic main_c nil)

(def ^:dynamic main_caseIdx nil)

(def ^:dynamic main_current nil)

(def ^:dynamic main_depth nil)

(def ^:dynamic main_header nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_i2 nil)

(def ^:dynamic main_level nil)

(def ^:dynamic main_levels nil)

(def ^:dynamic main_nextVal nil)

(def ^:dynamic main_out nil)

(def ^:dynamic main_res nil)

(def ^:dynamic main_s nil)

(def ^:dynamic main_seq nil)

(def ^:dynamic main_step nil)

(def ^:dynamic main_t nil)

(def ^:dynamic main_tLine nil)

(def ^:dynamic next_v nil)

(def ^:dynamic parse_ints_nums nil)

(def ^:dynamic parse_ints_p nil)

(def ^:dynamic parse_ints_pieces nil)

(def ^:dynamic split_cur nil)

(def ^:dynamic split_i nil)

(def ^:dynamic split_parts nil)

(defn split [split_s split_sep]
  (binding [split_cur nil split_i nil split_parts nil] (try (do (set! split_parts []) (set! split_cur "") (set! split_i 0) (while (< split_i (count split_s)) (if (and (and (> (count split_sep) 0) (<= (+' split_i (count split_sep)) (count split_s))) (= (subs split_s split_i (min (+' split_i (count split_sep)) (count split_s))) split_sep)) (do (set! split_parts (conj split_parts split_cur)) (set! split_cur "") (set! split_i (+' split_i (count split_sep)))) (do (set! split_cur (str split_cur (subs split_s split_i (min (+' split_i 1) (count split_s))))) (set! split_i (+' split_i 1))))) (set! split_parts (conj split_parts split_cur)) (throw (ex-info "return" {:v split_parts}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn parse_ints [parse_ints_line]
  (binding [parse_ints_nums nil parse_ints_p nil parse_ints_pieces nil] (try (do (set! parse_ints_pieces (split parse_ints_line " ")) (set! parse_ints_nums []) (doseq [parse_ints_p parse_ints_pieces] (when (> (count parse_ints_p) 0) (set! parse_ints_nums (conj parse_ints_nums (toi parse_ints_p))))) (throw (ex-info "return" {:v parse_ints_nums}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_arr nil main_arrBelow nil main_bottom nil main_c nil main_caseIdx nil main_current nil main_depth nil main_header nil main_i nil main_i2 nil main_level nil main_levels nil main_nextVal nil main_out nil main_res nil main_s nil main_seq nil main_step nil main_t nil main_tLine nil next_v nil] (try (do (set! main_tLine (read-line)) (when (= main_tLine "") (throw (ex-info "return" {:v nil}))) (set! main_t (toi main_tLine)) (set! main_caseIdx 0) (while (< main_caseIdx main_t) (do (set! main_header (parse_ints (read-line))) (set! main_s (nth main_header 0)) (set! main_c (nth main_header 1)) (set! main_seq (parse_ints (read-line))) (set! main_levels []) (set! main_levels (conj main_levels main_seq)) (set! main_current main_seq) (while (> (count main_current) 1) (do (set! next_v []) (set! main_i 0) (while (< (+' main_i 1) (count main_current)) (do (set! next_v (conj next_v (- (nth main_current (+' main_i 1)) (nth main_current main_i)))) (set! main_i (+' main_i 1)))) (set! main_levels (conj main_levels next_v)) (set! main_current next_v))) (set! main_depth (- (count main_levels) 1)) (set! main_step 0) (set! main_res []) (while (< (compare main_step main_c) 0) (do (set! main_bottom (nth main_levels main_depth)) (set! main_bottom (conj main_bottom (subs main_bottom (- (count main_bottom) 1) (+ (- (count main_bottom) 1) 1)))) (set! main_levels (assoc main_levels main_depth main_bottom)) (set! main_level (- main_depth 1)) (while (>= main_level 0) (do (set! main_arr (subs main_levels main_level (+ main_level 1))) (set! main_arrBelow (subs main_levels (+' main_level 1) (+ (+' main_level 1) 1))) (set! main_nextVal (str (subs main_arr (- (count main_arr) 1) (+ (- (count main_arr) 1) 1)) (subs main_arrBelow (- (count main_arrBelow) 1) (+ (- (count main_arrBelow) 1) 1)))) (set! main_arr (conj main_arr main_nextVal)) (set! main_levels (assoc main_levels main_level main_arr)) (set! main_level (- main_level 1)))) (set! main_res (conj main_res (subs (subs main_levels 0 (+ 0 1)) (- (count (subs main_levels 0 (+ 0 1))) 1) (+ (- (count (subs main_levels 0 (+ 0 1))) 1) 1)))) (set! main_step (+' main_step 1)))) (set! main_out "") (set! main_i2 0) (while (< main_i2 (count main_res)) (do (when (> main_i2 0) (set! main_out (str main_out " "))) (set! main_out (str main_out (mochi_str (nth main_res main_i2)))) (set! main_i2 (+' main_i2 1)))) (println main_out) (set! main_caseIdx (+' main_caseIdx 1))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

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
