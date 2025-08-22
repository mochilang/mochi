(ns main (:refer-clojure :exclude [range_desc range_asc concat_lists swap generate_gon_ring min_outer is_magic_gon permute_search solution]))

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

(declare range_desc range_asc concat_lists swap generate_gon_ring min_outer is_magic_gon permute_search solution)

(declare _read_file)

(def ^:dynamic concat_lists_i nil)

(def ^:dynamic concat_lists_res nil)

(def ^:dynamic generate_gon_ring_extended nil)

(def ^:dynamic generate_gon_ring_i nil)

(def ^:dynamic generate_gon_ring_magic_number nil)

(def ^:dynamic generate_gon_ring_result nil)

(def ^:dynamic is_magic_gon_i nil)

(def ^:dynamic is_magic_gon_total nil)

(def ^:dynamic min_outer_i nil)

(def ^:dynamic min_outer_min_val nil)

(def ^:dynamic permute_search_candidate nil)

(def ^:dynamic permute_search_i nil)

(def ^:dynamic permute_search_k nil)

(def ^:dynamic permute_search_res nil)

(def ^:dynamic permute_search_ring nil)

(def ^:dynamic permute_search_s nil)

(def ^:dynamic permute_search_swapped nil)

(def ^:dynamic range_asc_i nil)

(def ^:dynamic range_asc_res nil)

(def ^:dynamic range_desc_i nil)

(def ^:dynamic range_desc_res nil)

(def ^:dynamic solution_big nil)

(def ^:dynamic solution_max_str nil)

(def ^:dynamic solution_numbers nil)

(def ^:dynamic solution_small nil)

(def ^:dynamic swap_k nil)

(def ^:dynamic swap_res nil)

(defn range_desc [range_desc_start range_desc_end]
  (binding [range_desc_i nil range_desc_res nil] (try (do (set! range_desc_res []) (set! range_desc_i range_desc_start) (while (>= range_desc_i range_desc_end) (do (set! range_desc_res (conj range_desc_res range_desc_i)) (set! range_desc_i (- range_desc_i 1)))) (throw (ex-info "return" {:v range_desc_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn range_asc [range_asc_start range_asc_end]
  (binding [range_asc_i nil range_asc_res nil] (try (do (set! range_asc_res []) (set! range_asc_i range_asc_start) (while (<= range_asc_i range_asc_end) (do (set! range_asc_res (conj range_asc_res range_asc_i)) (set! range_asc_i (+ range_asc_i 1)))) (throw (ex-info "return" {:v range_asc_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn concat_lists [concat_lists_a concat_lists_b]
  (binding [concat_lists_i nil concat_lists_res nil] (try (do (set! concat_lists_res concat_lists_a) (set! concat_lists_i 0) (while (< concat_lists_i (count concat_lists_b)) (do (set! concat_lists_res (conj concat_lists_res (nth concat_lists_b concat_lists_i))) (set! concat_lists_i (+ concat_lists_i 1)))) (throw (ex-info "return" {:v concat_lists_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn swap [swap_xs swap_i swap_j]
  (binding [swap_k nil swap_res nil] (try (do (set! swap_res []) (set! swap_k 0) (while (< swap_k (count swap_xs)) (do (if (= swap_k swap_i) (set! swap_res (conj swap_res (nth swap_xs swap_j))) (if (= swap_k swap_j) (set! swap_res (conj swap_res (nth swap_xs swap_i))) (set! swap_res (conj swap_res (nth swap_xs swap_k))))) (set! swap_k (+ swap_k 1)))) (throw (ex-info "return" {:v swap_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn generate_gon_ring [generate_gon_ring_gon_side generate_gon_ring_perm]
  (binding [generate_gon_ring_extended nil generate_gon_ring_i nil generate_gon_ring_magic_number nil generate_gon_ring_result nil] (try (do (set! generate_gon_ring_result []) (set! generate_gon_ring_result (conj generate_gon_ring_result (nth generate_gon_ring_perm 0))) (set! generate_gon_ring_result (conj generate_gon_ring_result (nth generate_gon_ring_perm 1))) (set! generate_gon_ring_result (conj generate_gon_ring_result (nth generate_gon_ring_perm 2))) (set! generate_gon_ring_extended (conj generate_gon_ring_perm (nth generate_gon_ring_perm 1))) (set! generate_gon_ring_magic_number (if (< generate_gon_ring_gon_side 5) 1 2)) (set! generate_gon_ring_i 1) (while (< generate_gon_ring_i (+ (/ (count generate_gon_ring_extended) 3) generate_gon_ring_magic_number)) (do (set! generate_gon_ring_result (conj generate_gon_ring_result (nth generate_gon_ring_extended (+ (* 2 generate_gon_ring_i) 1)))) (set! generate_gon_ring_result (conj generate_gon_ring_result (nth generate_gon_ring_result (- (* 3 generate_gon_ring_i) 1)))) (set! generate_gon_ring_result (conj generate_gon_ring_result (nth generate_gon_ring_extended (+ (* 2 generate_gon_ring_i) 2)))) (set! generate_gon_ring_i (+ generate_gon_ring_i 1)))) (throw (ex-info "return" {:v generate_gon_ring_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn min_outer [min_outer_numbers]
  (binding [min_outer_i nil min_outer_min_val nil] (try (do (set! min_outer_min_val (nth min_outer_numbers 0)) (set! min_outer_i 3) (while (< min_outer_i (count min_outer_numbers)) (do (when (< (nth min_outer_numbers min_outer_i) min_outer_min_val) (set! min_outer_min_val (nth min_outer_numbers min_outer_i))) (set! min_outer_i (+ min_outer_i 3)))) (throw (ex-info "return" {:v min_outer_min_val}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_magic_gon [is_magic_gon_numbers]
  (binding [is_magic_gon_i nil is_magic_gon_total nil] (try (do (when (not= (mod (count is_magic_gon_numbers) 3) 0) (throw (ex-info "return" {:v false}))) (when (not= (min_outer is_magic_gon_numbers) (nth is_magic_gon_numbers 0)) (throw (ex-info "return" {:v false}))) (set! is_magic_gon_total (+ (+ (nth is_magic_gon_numbers 0) (nth is_magic_gon_numbers 1)) (nth is_magic_gon_numbers 2))) (set! is_magic_gon_i 3) (while (< is_magic_gon_i (count is_magic_gon_numbers)) (do (when (not= (+ (+ (nth is_magic_gon_numbers is_magic_gon_i) (nth is_magic_gon_numbers (+ is_magic_gon_i 1))) (nth is_magic_gon_numbers (+ is_magic_gon_i 2))) is_magic_gon_total) (throw (ex-info "return" {:v false}))) (set! is_magic_gon_i (+ is_magic_gon_i 3)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn permute_search [permute_search_nums permute_search_start permute_search_gon_side permute_search_current_max]
  (binding [permute_search_candidate nil permute_search_i nil permute_search_k nil permute_search_res nil permute_search_ring nil permute_search_s nil permute_search_swapped nil] (try (do (when (= permute_search_start (count permute_search_nums)) (do (set! permute_search_ring (generate_gon_ring permute_search_gon_side permute_search_nums)) (when (is_magic_gon permute_search_ring) (do (set! permute_search_s "") (set! permute_search_k 0) (while (< permute_search_k (count permute_search_ring)) (do (set! permute_search_s (str permute_search_s (mochi_str (nth permute_search_ring permute_search_k)))) (set! permute_search_k (+ permute_search_k 1)))) (when (> (compare permute_search_s permute_search_current_max) 0) (throw (ex-info "return" {:v permute_search_s}))))) (throw (ex-info "return" {:v permute_search_current_max})))) (set! permute_search_res permute_search_current_max) (set! permute_search_i permute_search_start) (while (< permute_search_i (count permute_search_nums)) (do (set! permute_search_swapped (swap permute_search_nums permute_search_start permute_search_i)) (set! permute_search_candidate (permute_search permute_search_swapped (+ permute_search_start 1) permute_search_gon_side permute_search_res)) (when (> (compare permute_search_candidate permute_search_res) 0) (set! permute_search_res permute_search_candidate)) (set! permute_search_i (+ permute_search_i 1)))) (throw (ex-info "return" {:v permute_search_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_gon_side]
  (binding [solution_big nil solution_max_str nil solution_numbers nil solution_small nil] (try (do (when (or (< solution_gon_side 3) (> solution_gon_side 5)) (throw (ex-info "return" {:v ""}))) (set! solution_small (range_desc (+ solution_gon_side 1) 1)) (set! solution_big (range_asc (+ solution_gon_side 2) (* solution_gon_side 2))) (set! solution_numbers (concat_lists solution_small solution_big)) (set! solution_max_str (permute_search solution_numbers 0 solution_gon_side "")) (throw (ex-info "return" {:v solution_max_str}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (solution 5))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
