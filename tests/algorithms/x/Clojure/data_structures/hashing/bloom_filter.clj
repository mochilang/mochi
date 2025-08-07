(ns main (:refer-clojure :exclude [ord new_bloom hash1 hash2 hash_positions bloom_add bloom_exists bitstring format_hash estimated_error_rate any_in main]))

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

(declare ord new_bloom hash1 hash2 hash_positions bloom_add bloom_exists bitstring format_hash estimated_error_rate any_in main)

(def ^:dynamic any_in_i nil)

(def ^:dynamic bitstring_i nil)

(def ^:dynamic bitstring_res nil)

(def ^:dynamic bloom_add_bits nil)

(def ^:dynamic bloom_add_i nil)

(def ^:dynamic bloom_add_idx nil)

(def ^:dynamic bloom_add_pos nil)

(def ^:dynamic bloom_exists_i nil)

(def ^:dynamic bloom_exists_idx nil)

(def ^:dynamic bloom_exists_pos nil)

(def ^:dynamic estimated_error_rate_frac nil)

(def ^:dynamic estimated_error_rate_i nil)

(def ^:dynamic estimated_error_rate_ones nil)

(def ^:dynamic format_hash_bits nil)

(def ^:dynamic format_hash_i nil)

(def ^:dynamic format_hash_idx nil)

(def ^:dynamic format_hash_pos nil)

(def ^:dynamic format_hash_res nil)

(def ^:dynamic hash1_h nil)

(def ^:dynamic hash1_i nil)

(def ^:dynamic hash2_h nil)

(def ^:dynamic hash2_i nil)

(def ^:dynamic hash_positions_h1 nil)

(def ^:dynamic hash_positions_h2 nil)

(def ^:dynamic hash_positions_res nil)

(def ^:dynamic main_bloom nil)

(def ^:dynamic main_film nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_not_present nil)

(def ^:dynamic new_bloom_bits nil)

(def ^:dynamic new_bloom_i nil)

(def ^:dynamic ord_i nil)

(def ^:dynamic main_ascii " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

(defn ord [ord_ch]
  (binding [ord_i nil] (try (do (set! ord_i 0) (while (< ord_i (count main_ascii)) (do (when (= (subs main_ascii ord_i (min (+ ord_i 1) (count main_ascii))) ord_ch) (throw (ex-info "return" {:v (+ 32 ord_i)}))) (set! ord_i (+ ord_i 1)))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn new_bloom [new_bloom_size]
  (binding [new_bloom_bits nil new_bloom_i nil] (try (do (set! new_bloom_bits []) (set! new_bloom_i 0) (while (< new_bloom_i new_bloom_size) (do (set! new_bloom_bits (conj new_bloom_bits 0)) (set! new_bloom_i (+ new_bloom_i 1)))) (throw (ex-info "return" {:v {:size new_bloom_size :bits new_bloom_bits}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn hash1 [hash1_value hash1_size]
  (binding [hash1_h nil hash1_i nil] (try (do (set! hash1_h 0) (set! hash1_i 0) (while (< hash1_i (count hash1_value)) (do (set! hash1_h (mod (+ (* hash1_h 31) (ord (subs hash1_value hash1_i (min (+ hash1_i 1) (count hash1_value))))) hash1_size)) (set! hash1_i (+ hash1_i 1)))) (throw (ex-info "return" {:v hash1_h}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn hash2 [hash2_value hash2_size]
  (binding [hash2_h nil hash2_i nil] (try (do (set! hash2_h 0) (set! hash2_i 0) (while (< hash2_i (count hash2_value)) (do (set! hash2_h (mod (+ (* hash2_h 131) (ord (subs hash2_value hash2_i (min (+ hash2_i 1) (count hash2_value))))) hash2_size)) (set! hash2_i (+ hash2_i 1)))) (throw (ex-info "return" {:v hash2_h}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn hash_positions [hash_positions_value hash_positions_size]
  (binding [hash_positions_h1 nil hash_positions_h2 nil hash_positions_res nil] (try (do (set! hash_positions_h1 (hash1 hash_positions_value hash_positions_size)) (set! hash_positions_h2 (hash2 hash_positions_value hash_positions_size)) (set! hash_positions_res []) (set! hash_positions_res (conj hash_positions_res hash_positions_h1)) (set! hash_positions_res (conj hash_positions_res hash_positions_h2)) (throw (ex-info "return" {:v hash_positions_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bloom_add [bloom_add_b bloom_add_value]
  (binding [bloom_add_bits nil bloom_add_i nil bloom_add_idx nil bloom_add_pos nil] (try (do (set! bloom_add_pos (hash_positions bloom_add_value (:size bloom_add_b))) (set! bloom_add_bits (:bits bloom_add_b)) (set! bloom_add_i 0) (while (< bloom_add_i (count bloom_add_pos)) (do (set! bloom_add_idx (- (- (:size bloom_add_b) 1) (nth bloom_add_pos bloom_add_i))) (set! bloom_add_bits (assoc bloom_add_bits bloom_add_idx 1)) (set! bloom_add_i (+ bloom_add_i 1)))) (throw (ex-info "return" {:v {:size (:size bloom_add_b) :bits bloom_add_bits}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bloom_exists [bloom_exists_b bloom_exists_value]
  (binding [bloom_exists_i nil bloom_exists_idx nil bloom_exists_pos nil] (try (do (set! bloom_exists_pos (hash_positions bloom_exists_value (:size bloom_exists_b))) (set! bloom_exists_i 0) (while (< bloom_exists_i (count bloom_exists_pos)) (do (set! bloom_exists_idx (- (- (:size bloom_exists_b) 1) (nth bloom_exists_pos bloom_exists_i))) (when (not= (get (:bits bloom_exists_b) bloom_exists_idx) 1) (throw (ex-info "return" {:v false}))) (set! bloom_exists_i (+ bloom_exists_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bitstring [bitstring_b]
  (binding [bitstring_i nil bitstring_res nil] (try (do (set! bitstring_res "") (set! bitstring_i 0) (while (< bitstring_i (:size bitstring_b)) (do (set! bitstring_res (str bitstring_res (str (get (:bits bitstring_b) bitstring_i)))) (set! bitstring_i (+ bitstring_i 1)))) (throw (ex-info "return" {:v bitstring_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn format_hash [format_hash_b format_hash_value]
  (binding [format_hash_bits nil format_hash_i nil format_hash_idx nil format_hash_pos nil format_hash_res nil] (try (do (set! format_hash_pos (hash_positions format_hash_value (:size format_hash_b))) (set! format_hash_bits []) (set! format_hash_i 0) (while (< format_hash_i (:size format_hash_b)) (do (set! format_hash_bits (conj format_hash_bits 0)) (set! format_hash_i (+ format_hash_i 1)))) (set! format_hash_i 0) (while (< format_hash_i (count format_hash_pos)) (do (set! format_hash_idx (- (- (:size format_hash_b) 1) (nth format_hash_pos format_hash_i))) (set! format_hash_bits (assoc format_hash_bits format_hash_idx 1)) (set! format_hash_i (+ format_hash_i 1)))) (set! format_hash_res "") (set! format_hash_i 0) (while (< format_hash_i (:size format_hash_b)) (do (set! format_hash_res (str format_hash_res (str (nth format_hash_bits format_hash_i)))) (set! format_hash_i (+ format_hash_i 1)))) (throw (ex-info "return" {:v format_hash_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn estimated_error_rate [estimated_error_rate_b]
  (binding [estimated_error_rate_frac nil estimated_error_rate_i nil estimated_error_rate_ones nil] (try (do (set! estimated_error_rate_ones 0) (set! estimated_error_rate_i 0) (while (< estimated_error_rate_i (:size estimated_error_rate_b)) (do (when (= (get (:bits estimated_error_rate_b) estimated_error_rate_i) 1) (set! estimated_error_rate_ones (+ estimated_error_rate_ones 1))) (set! estimated_error_rate_i (+ estimated_error_rate_i 1)))) (set! estimated_error_rate_frac (/ (double estimated_error_rate_ones) (double (:size estimated_error_rate_b)))) (throw (ex-info "return" {:v (* estimated_error_rate_frac estimated_error_rate_frac)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn any_in [any_in_b any_in_items]
  (binding [any_in_i nil] (try (do (set! any_in_i 0) (while (< any_in_i (count any_in_items)) (do (when (bloom_exists any_in_b (nth any_in_items any_in_i)) (throw (ex-info "return" {:v true}))) (set! any_in_i (+ any_in_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_bloom nil main_film nil main_i nil main_not_present nil] (do (set! main_bloom (new_bloom 8)) (println (bitstring main_bloom)) (println (bloom_exists main_bloom "Titanic")) (set! main_bloom (bloom_add main_bloom "Titanic")) (println (bitstring main_bloom)) (println (bloom_exists main_bloom "Titanic")) (set! main_bloom (bloom_add main_bloom "Avatar")) (println (bloom_exists main_bloom "Avatar")) (println (format_hash main_bloom "Avatar")) (println (bitstring main_bloom)) (set! main_not_present ["The Godfather" "Interstellar" "Parasite" "Pulp Fiction"]) (set! main_i 0) (while (< main_i (count main_not_present)) (do (set! main_film (nth main_not_present main_i)) (println (str (str main_film ":") (format_hash main_bloom main_film))) (set! main_i (+ main_i 1)))) (println (any_in main_bloom main_not_present)) (println (bloom_exists main_bloom "Ratatouille")) (println (format_hash main_bloom "Ratatouille")) (println (str (estimated_error_rate main_bloom))) (set! main_bloom (bloom_add main_bloom "The Godfather")) (println (str (estimated_error_rate main_bloom))) (println (bitstring main_bloom)))))

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
