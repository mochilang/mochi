(ns main (:refer-clojure :exclude [is_digit find_substring extract_citation get_citation]))

(require 'clojure.set)

(defrecord Params [title journal volume pages year hl])

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

(def ^:dynamic extract_citation_ch nil)

(def ^:dynamic extract_citation_idx nil)

(def ^:dynamic extract_citation_marker nil)

(def ^:dynamic extract_citation_pos nil)

(def ^:dynamic extract_citation_result nil)

(def ^:dynamic find_substring_i nil)

(def ^:dynamic find_substring_j nil)

(def ^:dynamic get_citation_html nil)

(def ^:dynamic is_digit_i nil)

(declare is_digit find_substring extract_citation get_citation)

(def ^:dynamic main_DIGITS "0123456789")

(defn is_digit [is_digit_ch]
  (binding [is_digit_i nil] (try (do (set! is_digit_i 0) (while (< is_digit_i (count main_DIGITS)) (do (when (= (subs main_DIGITS is_digit_i (+ is_digit_i 1)) is_digit_ch) (throw (ex-info "return" {:v true}))) (set! is_digit_i (+ is_digit_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_substring [find_substring_haystack find_substring_needle]
  (binding [find_substring_i nil find_substring_j nil] (try (do (set! find_substring_i 0) (while (<= find_substring_i (- (count find_substring_haystack) (count find_substring_needle))) (do (set! find_substring_j 0) (loop [while_flag_1 true] (when (and while_flag_1 (< find_substring_j (count find_substring_needle))) (cond (not= (subs find_substring_haystack (+ find_substring_i find_substring_j) (+ (+ find_substring_i find_substring_j) 1)) (subs find_substring_needle find_substring_j (+ find_substring_j 1))) (recur false) :else (do (set! find_substring_j (+ find_substring_j 1)) (recur while_flag_1))))) (when (= find_substring_j (count find_substring_needle)) (throw (ex-info "return" {:v find_substring_i}))) (set! find_substring_i (+ find_substring_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn extract_citation [extract_citation_html]
  (binding [extract_citation_ch nil extract_citation_idx nil extract_citation_marker nil extract_citation_pos nil extract_citation_result nil] (try (do (set! extract_citation_marker "Cited by ") (set! extract_citation_idx (find_substring extract_citation_html extract_citation_marker)) (when (< extract_citation_idx 0) (throw (ex-info "return" {:v ""}))) (set! extract_citation_pos (+ extract_citation_idx (count extract_citation_marker))) (set! extract_citation_result "") (loop [while_flag_2 true] (when (and while_flag_2 (< extract_citation_pos (count extract_citation_html))) (do (set! extract_citation_ch (subs extract_citation_html extract_citation_pos (+ extract_citation_pos 1))) (cond (not (is_digit extract_citation_ch)) (recur false) :else (do (set! extract_citation_result (str extract_citation_result extract_citation_ch)) (set! extract_citation_pos (+ extract_citation_pos 1)) (recur while_flag_2)))))) (throw (ex-info "return" {:v extract_citation_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_citation [get_citation_base_url get_citation_params]
  (binding [get_citation_html nil] (try (do (set! get_citation_html "<div class=\"gs_ri\"><div class=\"gs_fl\"><a>Cited by 123</a></div></div>") (throw (ex-info "return" {:v (extract_citation get_citation_html)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (when (= __name__ "__main__") (do (def ^:dynamic main_params {"hl" "en" "journal" "Chem. Mater." "pages" "3979-3990" "title" "Precisely geometry controlled microsupercapacitors for ultrahigh areal capacitance, volumetric capacitance, and energy density" "volume" "30" "year" "2018"}) (println (get_citation "https://scholar.google.com/scholar_lookup" main_params))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
