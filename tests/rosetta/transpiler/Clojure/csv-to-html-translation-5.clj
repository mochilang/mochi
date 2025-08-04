(ns main (:refer-clojure :exclude [split htmlEscape]))

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

(declare split htmlEscape)

(def ^:dynamic htmlEscape_ch nil)

(def ^:dynamic htmlEscape_i nil)

(def ^:dynamic htmlEscape_out nil)

(def ^:dynamic main_cells nil)

(def ^:dynamic main_rows nil)

(def ^:dynamic split_i nil)

(def ^:dynamic split_n nil)

(def ^:dynamic split_out nil)

(def ^:dynamic split_start nil)

(defn split [split_s split_sep]
  (binding [split_i nil split_n nil split_out nil split_start nil] (try (do (set! split_out []) (set! split_start 0) (set! split_i 0) (set! split_n (count split_sep)) (while (<= split_i (- (count split_s) split_n)) (if (= (subs split_s split_i (+ split_i split_n)) split_sep) (do (set! split_out (conj split_out (subs split_s split_start split_i))) (set! split_i (+ split_i split_n)) (set! split_start split_i)) (set! split_i (+ split_i 1)))) (set! split_out (conj split_out (subs split_s split_start (count split_s)))) (throw (ex-info "return" {:v split_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn htmlEscape [htmlEscape_s]
  (binding [htmlEscape_ch nil htmlEscape_i nil htmlEscape_out nil] (try (do (set! htmlEscape_out "") (set! htmlEscape_i 0) (while (< htmlEscape_i (count htmlEscape_s)) (do (set! htmlEscape_ch (subs htmlEscape_s htmlEscape_i (+ htmlEscape_i 1))) (if (= htmlEscape_ch "&") (set! htmlEscape_out (str htmlEscape_out "&amp;")) (if (= htmlEscape_ch "<") (set! htmlEscape_out (str htmlEscape_out "&lt;")) (if (= htmlEscape_ch ">") (set! htmlEscape_out (str htmlEscape_out "&gt;")) (set! htmlEscape_out (str htmlEscape_out htmlEscape_ch))))) (set! htmlEscape_i (+ htmlEscape_i 1)))) (throw (ex-info "return" {:v htmlEscape_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_c (str (str (str (str (str "Character,Speech\n" "The multitude,The messiah! Show us the messiah!\n") "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n") "The multitude,Who are you?\n") "Brians mother,I'm his mother; that's who!\n") "The multitude,Behold his mother! Behold his mother!"))

(def ^:dynamic main_rows [])

(defn -main []
  (doseq [line (split main_c "\n")] (def main_rows (conj main_rows (split line ","))))
  (println "<table>")
  (doseq [row main_rows] (do (def ^:dynamic main_cells "") (doseq [cell row] (def main_cells (str (str (str main_cells "<td>") (htmlEscape cell)) "</td>"))) (println (str (str "    <tr>" main_cells) "</tr>"))))
  (println "</table>"))

(-main)
