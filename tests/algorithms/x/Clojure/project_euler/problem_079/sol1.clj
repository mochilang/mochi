(ns main (:refer-clojure :exclude [parse_int join contains index_of remove_at unique_chars satisfies search find_secret_passcode]))

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

(declare parse_int join contains index_of remove_at unique_chars satisfies search find_secret_passcode)

(def ^:dynamic contains_i nil)

(def ^:dynamic find_secret_passcode_chars nil)

(def ^:dynamic find_secret_passcode_s nil)

(def ^:dynamic index_of_i nil)

(def ^:dynamic join_i nil)

(def ^:dynamic join_s nil)

(def ^:dynamic next_v nil)

(def ^:dynamic parse_int_c nil)

(def ^:dynamic parse_int_i nil)

(def ^:dynamic parse_int_value nil)

(def ^:dynamic remove_at_i nil)

(def ^:dynamic remove_at_res nil)

(def ^:dynamic rest_v nil)

(def ^:dynamic satisfies_i nil)

(def ^:dynamic satisfies_i0 nil)

(def ^:dynamic satisfies_i1 nil)

(def ^:dynamic satisfies_i2 nil)

(def ^:dynamic satisfies_login nil)

(def ^:dynamic search_c nil)

(def ^:dynamic search_i nil)

(def ^:dynamic search_res nil)

(def ^:dynamic unique_chars_c nil)

(def ^:dynamic unique_chars_chars nil)

(def ^:dynamic unique_chars_i nil)

(def ^:dynamic unique_chars_j nil)

(def ^:dynamic unique_chars_login nil)

(defn parse_int [parse_int_s]
  (binding [parse_int_c nil parse_int_i nil parse_int_value nil] (try (do (set! parse_int_value 0) (set! parse_int_i 0) (while (< parse_int_i (count parse_int_s)) (do (set! parse_int_c (subs parse_int_s parse_int_i (+ parse_int_i 1))) (set! parse_int_value (+ (* parse_int_value 10) (Long/parseLong parse_int_c))) (set! parse_int_i (+ parse_int_i 1)))) (throw (ex-info "return" {:v parse_int_value}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn join [join_xs]
  (binding [join_i nil join_s nil] (try (do (set! join_s "") (set! join_i 0) (while (< join_i (count join_xs)) (do (set! join_s (str join_s (nth join_xs join_i))) (set! join_i (+ join_i 1)))) (throw (ex-info "return" {:v join_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains [contains_xs contains_c]
  (binding [contains_i nil] (try (do (set! contains_i 0) (while (< contains_i (count contains_xs)) (do (when (= (nth contains_xs contains_i) contains_c) (throw (ex-info "return" {:v true}))) (set! contains_i (+ contains_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn index_of [index_of_xs index_of_c]
  (binding [index_of_i nil] (try (do (set! index_of_i 0) (while (< index_of_i (count index_of_xs)) (do (when (= (nth index_of_xs index_of_i) index_of_c) (throw (ex-info "return" {:v index_of_i}))) (set! index_of_i (+ index_of_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_at [remove_at_xs remove_at_idx]
  (binding [remove_at_i nil remove_at_res nil] (try (do (set! remove_at_res []) (set! remove_at_i 0) (while (< remove_at_i (count remove_at_xs)) (do (when (not= remove_at_i remove_at_idx) (set! remove_at_res (conj remove_at_res (nth remove_at_xs remove_at_i)))) (set! remove_at_i (+ remove_at_i 1)))) (throw (ex-info "return" {:v remove_at_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn unique_chars [unique_chars_logins]
  (binding [unique_chars_c nil unique_chars_chars nil unique_chars_i nil unique_chars_j nil unique_chars_login nil] (try (do (set! unique_chars_chars []) (set! unique_chars_i 0) (while (< unique_chars_i (count unique_chars_logins)) (do (set! unique_chars_login (nth unique_chars_logins unique_chars_i)) (set! unique_chars_j 0) (while (< unique_chars_j (count unique_chars_login)) (do (set! unique_chars_c (subs unique_chars_login unique_chars_j (+ unique_chars_j 1))) (when (not (contains unique_chars_chars unique_chars_c)) (set! unique_chars_chars (conj unique_chars_chars unique_chars_c))) (set! unique_chars_j (+ unique_chars_j 1)))) (set! unique_chars_i (+ unique_chars_i 1)))) (throw (ex-info "return" {:v unique_chars_chars}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn satisfies [satisfies_permutation satisfies_logins]
  (binding [satisfies_i nil satisfies_i0 nil satisfies_i1 nil satisfies_i2 nil satisfies_login nil] (try (do (set! satisfies_i 0) (while (< satisfies_i (count satisfies_logins)) (do (set! satisfies_login (nth satisfies_logins satisfies_i)) (set! satisfies_i0 (index_of satisfies_permutation (subs satisfies_login 0 (+ 0 1)))) (set! satisfies_i1 (index_of satisfies_permutation (subs satisfies_login 1 (+ 1 1)))) (set! satisfies_i2 (index_of satisfies_permutation (subs satisfies_login 2 (+ 2 1)))) (when (not (and (< satisfies_i0 satisfies_i1) (< satisfies_i1 satisfies_i2))) (throw (ex-info "return" {:v false}))) (set! satisfies_i (+ satisfies_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn search [search_chars search_current search_logins]
  (binding [next_v nil rest_v nil search_c nil search_i nil search_res nil] (try (do (when (= (count search_chars) 0) (do (when (satisfies search_current search_logins) (throw (ex-info "return" {:v (join search_current)}))) (throw (ex-info "return" {:v ""})))) (set! search_i 0) (while (< search_i (count search_chars)) (do (set! search_c (nth search_chars search_i)) (set! rest_v (remove_at search_chars search_i)) (set! next_v (conj search_current search_c)) (set! search_res (search rest_v next_v search_logins)) (when (not= search_res "") (throw (ex-info "return" {:v search_res}))) (set! search_i (+ search_i 1)))) (throw (ex-info "return" {:v ""}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_secret_passcode [find_secret_passcode_logins]
  (binding [find_secret_passcode_chars nil find_secret_passcode_s nil] (try (do (set! find_secret_passcode_chars (unique_chars find_secret_passcode_logins)) (set! find_secret_passcode_s (search find_secret_passcode_chars [] find_secret_passcode_logins)) (if (= find_secret_passcode_s "") (- 1) (parse_int find_secret_passcode_s))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_logins1 ["135" "259" "235" "189" "690" "168" "120" "136" "289" "589" "160" "165" "580" "369" "250" "280"])

(def ^:dynamic main_logins2 ["426" "281" "061" "819" "268" "406" "420" "428" "209" "689" "019" "421" "469" "261" "681" "201"])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (find_secret_passcode main_logins1)))
      (println (str (find_secret_passcode main_logins2)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
