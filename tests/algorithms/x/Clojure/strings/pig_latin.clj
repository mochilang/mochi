(ns main (:refer-clojure :exclude [strip is_vowel pig_latin]))

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

(declare strip is_vowel pig_latin)

(def ^:dynamic first_v nil)

(def ^:dynamic is_vowel_i nil)

(def ^:dynamic pig_latin_ch nil)

(def ^:dynamic pig_latin_i nil)

(def ^:dynamic pig_latin_trimmed nil)

(def ^:dynamic pig_latin_w nil)

(def ^:dynamic strip_end nil)

(def ^:dynamic strip_start nil)

(def ^:dynamic main_VOWELS "aeiou")

(defn strip [strip_s]
  (binding [strip_end nil strip_start nil] (try (do (set! strip_start 0) (set! strip_end (count strip_s)) (while (and (< strip_start strip_end) (= (subs strip_s strip_start (min (+ strip_start 1) (count strip_s))) " ")) (set! strip_start (+ strip_start 1))) (while (and (> strip_end strip_start) (= (subs strip_s (- strip_end 1) (min strip_end (count strip_s))) " ")) (set! strip_end (- strip_end 1))) (throw (ex-info "return" {:v (subs strip_s strip_start (min strip_end (count strip_s)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_vowel [is_vowel_c]
  (binding [is_vowel_i nil] (try (do (set! is_vowel_i 0) (while (< is_vowel_i (count main_VOWELS)) (do (when (= is_vowel_c (subs main_VOWELS is_vowel_i (min (+ is_vowel_i 1) (count main_VOWELS)))) (throw (ex-info "return" {:v true}))) (set! is_vowel_i (+ is_vowel_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pig_latin [pig_latin_word]
  (binding [first_v nil pig_latin_ch nil pig_latin_i nil pig_latin_trimmed nil pig_latin_w nil] (try (do (set! pig_latin_trimmed (strip pig_latin_word)) (when (= (count pig_latin_trimmed) 0) (throw (ex-info "return" {:v ""}))) (set! pig_latin_w (clojure.string/lower-case pig_latin_trimmed)) (set! first_v (subs pig_latin_w 0 (min 1 (count pig_latin_w)))) (when (is_vowel first_v) (throw (ex-info "return" {:v (str pig_latin_w "way")}))) (set! pig_latin_i 0) (loop [while_flag_1 true] (when (and while_flag_1 (< pig_latin_i (count pig_latin_w))) (do (set! pig_latin_ch (subs pig_latin_w pig_latin_i (min (+ pig_latin_i 1) (count pig_latin_w)))) (cond (is_vowel pig_latin_ch) (recur false) :else (do (set! pig_latin_i (+ pig_latin_i 1)) (recur while_flag_1)))))) (throw (ex-info "return" {:v (str (str (subs pig_latin_w pig_latin_i (min (count pig_latin_w) (count pig_latin_w))) (subs pig_latin_w 0 (min pig_latin_i (count pig_latin_w)))) "ay")}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "pig_latin('friends') = " (pig_latin "friends")))
      (println (str "pig_latin('smile') = " (pig_latin "smile")))
      (println (str "pig_latin('eat') = " (pig_latin "eat")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
