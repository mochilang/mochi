(ns main (:refer-clojure :exclude [contains substring strip_chars strip test_strip main]))

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

(declare contains substring strip_chars strip test_strip main)

(def ^:dynamic contains_i nil)

(def ^:dynamic strip_chars_end nil)

(def ^:dynamic strip_chars_start nil)

(def ^:dynamic substring_i nil)

(def ^:dynamic substring_res nil)

(defn contains [contains_chars contains_ch]
  (binding [contains_i nil] (try (do (set! contains_i 0) (while (< contains_i (count contains_chars)) (do (when (= (subs contains_chars contains_i (+ contains_i 1)) contains_ch) (throw (ex-info "return" {:v true}))) (set! contains_i (+ contains_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn substring [substring_s substring_start substring_end]
  (binding [substring_i nil substring_res nil] (try (do (set! substring_res "") (set! substring_i substring_start) (while (< substring_i substring_end) (do (set! substring_res (str substring_res (subs substring_s substring_i (+ substring_i 1)))) (set! substring_i (+ substring_i 1)))) (throw (ex-info "return" {:v substring_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn strip_chars [strip_chars_user_string strip_chars_characters]
  (binding [strip_chars_end nil strip_chars_start nil] (try (do (set! strip_chars_start 0) (set! strip_chars_end (count strip_chars_user_string)) (while (and (< strip_chars_start strip_chars_end) (contains strip_chars_characters (subs strip_chars_user_string strip_chars_start (+ strip_chars_start 1)))) (set! strip_chars_start (+ strip_chars_start 1))) (while (and (> strip_chars_end strip_chars_start) (contains strip_chars_characters (subs strip_chars_user_string (- strip_chars_end 1) (+ (- strip_chars_end 1) 1)))) (set! strip_chars_end (- strip_chars_end 1))) (throw (ex-info "return" {:v (substring strip_chars_user_string strip_chars_start strip_chars_end)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn strip [strip_user_string]
  (try (throw (ex-info "return" {:v (strip_chars strip_user_string " \t\n\r")})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn test_strip []
  (do (when (not= (strip "   hello   ") "hello") (throw (Exception. "test1 failed"))) (when (not= (strip_chars "...world..." ".") "world") (throw (Exception. "test2 failed"))) (when (not= (strip_chars "123hello123" "123") "hello") (throw (Exception. "test3 failed"))) (when (not= (strip "") "") (throw (Exception. "test4 failed")))))

(defn main []
  (do (test_strip) (println (strip "   hello   ")) (println (strip_chars "...world..." ".")) (println (strip_chars "123hello123" "123")) (println (strip ""))))

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
