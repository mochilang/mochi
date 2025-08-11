(ns main (:refer-clojure :exclude [index_of_from extract_links main]))

(require 'clojure.set)

(defrecord Link [href text])

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

(def ^:dynamic extract_links_href nil)

(def ^:dynamic extract_links_href_end nil)

(def ^:dynamic extract_links_href_start nil)

(def ^:dynamic extract_links_i nil)

(def ^:dynamic extract_links_link nil)

(def ^:dynamic extract_links_res nil)

(def ^:dynamic extract_links_tag_start nil)

(def ^:dynamic extract_links_text nil)

(def ^:dynamic extract_links_text_end nil)

(def ^:dynamic extract_links_text_start nil)

(def ^:dynamic index_of_from_i nil)

(def ^:dynamic index_of_from_max nil)

(def ^:dynamic main_href nil)

(def ^:dynamic main_html nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_link nil)

(def ^:dynamic main_links nil)

(def ^:dynamic main_text nil)

(declare index_of_from extract_links main)

(defn index_of_from [index_of_from_s index_of_from_sub index_of_from_start]
  (binding [index_of_from_i nil index_of_from_max nil] (try (do (set! index_of_from_i index_of_from_start) (set! index_of_from_max (- (count index_of_from_s) (count index_of_from_sub))) (while (<= index_of_from_i index_of_from_max) (do (when (= (subs index_of_from_s index_of_from_i (min (+ index_of_from_i (count index_of_from_sub)) (count index_of_from_s))) index_of_from_sub) (throw (ex-info "return" {:v index_of_from_i}))) (set! index_of_from_i (+ index_of_from_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn extract_links [extract_links_html]
  (binding [extract_links_href nil extract_links_href_end nil extract_links_href_start nil extract_links_i nil extract_links_link nil extract_links_res nil extract_links_tag_start nil extract_links_text nil extract_links_text_end nil extract_links_text_start nil] (try (do (set! extract_links_res []) (set! extract_links_i 0) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (set! extract_links_tag_start (index_of_from extract_links_html "<a class=\"eZt8xd\"" extract_links_i)) (cond (= extract_links_tag_start (- 1)) (recur false) (= extract_links_href_start (- 1)) (recur false) (= extract_links_href_end (- 1)) (recur false) (= extract_links_text_end (- 1)) (recur false) :else (do (set! extract_links_href_start (index_of_from extract_links_html "href=\"" extract_links_tag_start)) (set! extract_links_href_start (+ extract_links_href_start (count "href=\""))) (set! extract_links_href_end (index_of_from extract_links_html "\"" extract_links_href_start)) (set! extract_links_href (subs extract_links_html extract_links_href_start (min extract_links_href_end (count extract_links_html)))) (set! extract_links_text_start (+ (index_of_from extract_links_html ">" extract_links_href_end) 1)) (set! extract_links_text_end (index_of_from extract_links_html "</a>" extract_links_text_start)) (set! extract_links_text (subs extract_links_html extract_links_text_start (min extract_links_text_end (count extract_links_html)))) (set! extract_links_link {"href" extract_links_href "text" extract_links_text}) (set! extract_links_res (conj extract_links_res extract_links_link)) (set! extract_links_i (+ extract_links_text_end (count "</a>"))) (recur while_flag_1)))))) (throw (ex-info "return" {:v extract_links_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_href nil main_html nil main_i nil main_link nil main_links nil main_text nil] (do (set! main_html (str (str "<div><a class=\"eZt8xd\" href=\"/url?q=http://example1.com\">Example1</a>" "<a class=\"eZt8xd\" href=\"/maps\">Maps</a>") "<a class=\"eZt8xd\" href=\"/url?q=http://example2.com\">Example2</a></div>")) (set! main_links (extract_links main_html)) (println (str (count main_links))) (set! main_i 0) (while (and (< main_i (count main_links)) (< main_i 5)) (do (set! main_link (nth main_links main_i)) (set! main_href (get main_link "href")) (set! main_text (get main_link "text")) (if (= main_text "Maps") (println main_href) (println (str "https://google.com" main_href))) (set! main_i (+ main_i 1)))))))

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
