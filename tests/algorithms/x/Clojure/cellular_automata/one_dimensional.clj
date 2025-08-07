(ns main (:refer-clojure :exclude [format_ruleset new_generation cells_to_string]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare format_ruleset new_generation cells_to_string)

(def ^:dynamic cells_to_string_i nil)

(def ^:dynamic cells_to_string_result nil)

(def ^:dynamic format_ruleset_bits nil)

(def ^:dynamic format_ruleset_bits_rev nil)

(def ^:dynamic format_ruleset_i nil)

(def ^:dynamic format_ruleset_j nil)

(def ^:dynamic format_ruleset_rs nil)

(def ^:dynamic main_cells nil)

(def ^:dynamic main_t nil)

(def ^:dynamic main_time nil)

(def ^:dynamic new_generation_center nil)

(def ^:dynamic new_generation_i nil)

(def ^:dynamic new_generation_idx nil)

(def ^:dynamic new_generation_left_neighbor nil)

(def ^:dynamic new_generation_next_generation nil)

(def ^:dynamic new_generation_population nil)

(def ^:dynamic new_generation_right_neighbor nil)

(defn format_ruleset [format_ruleset_ruleset]
  (binding [format_ruleset_bits nil format_ruleset_bits_rev nil format_ruleset_i nil format_ruleset_j nil format_ruleset_rs nil] (try (do (set! format_ruleset_rs format_ruleset_ruleset) (set! format_ruleset_bits_rev []) (set! format_ruleset_i 0) (while (< format_ruleset_i 8) (do (set! format_ruleset_bits_rev (conj format_ruleset_bits_rev (mod format_ruleset_rs 2))) (set! format_ruleset_rs (quot format_ruleset_rs 2)) (set! format_ruleset_i (+ format_ruleset_i 1)))) (set! format_ruleset_bits []) (set! format_ruleset_j (- (count format_ruleset_bits_rev) 1)) (while (>= format_ruleset_j 0) (do (set! format_ruleset_bits (conj format_ruleset_bits (nth format_ruleset_bits_rev format_ruleset_j))) (set! format_ruleset_j (- format_ruleset_j 1)))) (throw (ex-info "return" {:v format_ruleset_bits}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn new_generation [new_generation_cells new_generation_rules new_generation_time]
  (binding [new_generation_center nil new_generation_i nil new_generation_idx nil new_generation_left_neighbor nil new_generation_next_generation nil new_generation_population nil new_generation_right_neighbor nil] (try (do (set! new_generation_population (count (nth new_generation_cells 0))) (set! new_generation_next_generation []) (set! new_generation_i 0) (while (< new_generation_i new_generation_population) (do (set! new_generation_left_neighbor (if (= new_generation_i 0) 0 (nth (nth new_generation_cells new_generation_time) (- new_generation_i 1)))) (set! new_generation_right_neighbor (if (= new_generation_i (- new_generation_population 1)) 0 (nth (nth new_generation_cells new_generation_time) (+ new_generation_i 1)))) (set! new_generation_center (nth (nth new_generation_cells new_generation_time) new_generation_i)) (set! new_generation_idx (- 7 (+ (+ (* new_generation_left_neighbor 4) (* new_generation_center 2)) new_generation_right_neighbor))) (set! new_generation_next_generation (conj new_generation_next_generation (nth new_generation_rules new_generation_idx))) (set! new_generation_i (+ new_generation_i 1)))) (throw (ex-info "return" {:v new_generation_next_generation}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn cells_to_string [cells_to_string_row]
  (binding [cells_to_string_i nil cells_to_string_result nil] (try (do (set! cells_to_string_result "") (set! cells_to_string_i 0) (while (< cells_to_string_i (count cells_to_string_row)) (do (if (= (nth cells_to_string_row cells_to_string_i) 1) (set! cells_to_string_result (str cells_to_string_result "#")) (set! cells_to_string_result (str cells_to_string_result "."))) (set! cells_to_string_i (+ cells_to_string_i 1)))) (throw (ex-info "return" {:v cells_to_string_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_initial [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])

(def ^:dynamic main_cells [main_initial])

(def ^:dynamic main_rules (format_ruleset 30))

(def ^:dynamic main_time 0)

(def ^:dynamic main_t 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< main_time 16) (do (def ^:dynamic next_v (new_generation main_cells main_rules main_time)) (def main_cells (conj main_cells next_v)) (def main_time (+ main_time 1))))
      (while (< main_t (count main_cells)) (do (println (cells_to_string (nth main_cells main_t))) (def main_t (+ main_t 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
