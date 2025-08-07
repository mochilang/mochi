(ns main (:refer-clojure :exclude [index_of encrypt decrypt brute_force main]))

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

(declare index_of encrypt decrypt brute_force main)

(def ^:dynamic brute_force_key nil)

(def ^:dynamic brute_force_message nil)

(def ^:dynamic brute_force_n nil)

(def ^:dynamic brute_force_results nil)

(def ^:dynamic decrypt_ch nil)

(def ^:dynamic decrypt_i nil)

(def ^:dynamic decrypt_idx nil)

(def ^:dynamic decrypt_n nil)

(def ^:dynamic decrypt_new_key nil)

(def ^:dynamic decrypt_result nil)

(def ^:dynamic encrypt_ch nil)

(def ^:dynamic encrypt_i nil)

(def ^:dynamic encrypt_idx nil)

(def ^:dynamic encrypt_n nil)

(def ^:dynamic encrypt_new_key nil)

(def ^:dynamic encrypt_result nil)

(def ^:dynamic index_of_i nil)

(def ^:dynamic main_alpha nil)

(def ^:dynamic main_brute nil)

(def ^:dynamic main_dec nil)

(def ^:dynamic main_enc nil)

(def ^:dynamic main_default_alphabet "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn index_of [index_of_s index_of_ch]
  (binding [index_of_i nil] (try (do (set! index_of_i 0) (while (< index_of_i (count index_of_s)) (do (when (= (subs index_of_s index_of_i (min (+ index_of_i 1) (count index_of_s))) index_of_ch) (throw (ex-info "return" {:v index_of_i}))) (set! index_of_i (+ index_of_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn encrypt [encrypt_input_string encrypt_key encrypt_alphabet]
  (binding [encrypt_ch nil encrypt_i nil encrypt_idx nil encrypt_n nil encrypt_new_key nil encrypt_result nil] (try (do (set! encrypt_result "") (set! encrypt_i 0) (set! encrypt_n (count encrypt_alphabet)) (while (< encrypt_i (count encrypt_input_string)) (do (set! encrypt_ch (subs encrypt_input_string encrypt_i (min (+ encrypt_i 1) (count encrypt_input_string)))) (set! encrypt_idx (index_of encrypt_alphabet encrypt_ch)) (if (< encrypt_idx 0) (set! encrypt_result (str encrypt_result encrypt_ch)) (do (set! encrypt_new_key (mod (+ encrypt_idx encrypt_key) encrypt_n)) (when (< encrypt_new_key 0) (set! encrypt_new_key (+ encrypt_new_key encrypt_n))) (set! encrypt_result (str encrypt_result (subs encrypt_alphabet encrypt_new_key (min (+ encrypt_new_key 1) (count encrypt_alphabet))))))) (set! encrypt_i (+ encrypt_i 1)))) (throw (ex-info "return" {:v encrypt_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decrypt [decrypt_input_string decrypt_key decrypt_alphabet]
  (binding [decrypt_ch nil decrypt_i nil decrypt_idx nil decrypt_n nil decrypt_new_key nil decrypt_result nil] (try (do (set! decrypt_result "") (set! decrypt_i 0) (set! decrypt_n (count decrypt_alphabet)) (while (< decrypt_i (count decrypt_input_string)) (do (set! decrypt_ch (subs decrypt_input_string decrypt_i (min (+ decrypt_i 1) (count decrypt_input_string)))) (set! decrypt_idx (index_of decrypt_alphabet decrypt_ch)) (if (< decrypt_idx 0) (set! decrypt_result (str decrypt_result decrypt_ch)) (do (set! decrypt_new_key (mod (- decrypt_idx decrypt_key) decrypt_n)) (when (< decrypt_new_key 0) (set! decrypt_new_key (+ decrypt_new_key decrypt_n))) (set! decrypt_result (str decrypt_result (subs decrypt_alphabet decrypt_new_key (min (+ decrypt_new_key 1) (count decrypt_alphabet))))))) (set! decrypt_i (+ decrypt_i 1)))) (throw (ex-info "return" {:v decrypt_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn brute_force [brute_force_input_string brute_force_alphabet]
  (binding [brute_force_key nil brute_force_message nil brute_force_n nil brute_force_results nil] (try (do (set! brute_force_results []) (set! brute_force_key 1) (set! brute_force_n (count brute_force_alphabet)) (while (<= brute_force_key brute_force_n) (do (set! brute_force_message (decrypt brute_force_input_string brute_force_key brute_force_alphabet)) (set! brute_force_results (conj brute_force_results brute_force_message)) (set! brute_force_key (+ brute_force_key 1)))) (throw (ex-info "return" {:v brute_force_results}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_alpha nil main_brute nil main_dec nil main_enc nil] (do (set! main_alpha main_default_alphabet) (set! main_enc (encrypt "The quick brown fox jumps over the lazy dog" 8 main_alpha)) (println main_enc) (set! main_dec (decrypt main_enc 8 main_alpha)) (println main_dec) (set! main_brute (brute_force "jFyuMy xIH'N vLONy zILwy Gy!" main_alpha)) (println (nth main_brute 19)))))

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
