(ns main (:refer-clojure :exclude [fields padRight join validate main]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(defn fields [s]
  (try (do (def words []) (def cur "") (def i 0) (while (< i (count s)) (do (def ch (subs s i (+ i 1))) (if (or (or (= ch " ") (= ch "\n")) (= ch "\t")) (when (> (count cur) 0) (do (def words (conj words cur)) (def cur ""))) (def cur (str cur ch))) (def i (+ i 1)))) (when (> (count cur) 0) (def words (conj words cur))) (throw (ex-info "return" {:v words}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn padRight [s width]
  (try (do (def out s) (def i (count s)) (while (< i width) (do (def out (str out " ")) (def i (+ i 1)))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn join [xs sep]
  (try (do (def res "") (def i 0) (while (< i (count xs)) (do (when (> i 0) (def res (str res sep))) (def res (str res (nth xs i))) (def i (+ i 1)))) (throw (ex-info "return" {:v res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn validate [commands words mins]
  (try (do (def results []) (when (= (count words) 0) (throw (ex-info "return" {:v results}))) (def wi 0) (while (< wi (count words)) (do (def w (nth words wi)) (def found false) (def wlen (count w)) (def ci 0) (loop [while_flag_1 true] (when (and while_flag_1 (< ci (count commands))) (do (def cmd (nth commands ci)) (cond (and (and (and (not= (nth mins ci) 0) (>= wlen (nth mins ci))) (<= wlen (count cmd))) (= (subs c 0 wlen) ww)) (do (def c (upper cmd)) (def ww (upper w)) (do (def results (conj results c)) (def found true) (recur false))) :else (do (def ci (+ ci 1)) (recur while_flag_1)))))) (when (not found) (def results (conj results "*error*"))) (def wi (+ wi 1)))) (throw (ex-info "return" {:v results}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def table (str (str (str (str (str (str "Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress Copy " "COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find ") "NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput ") " Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO ") "MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT ") "READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT ") "RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer TypeUp ")) (def commands (fields table)) (def mins []) (def i 0) (while (< i (count commands)) (do (def count 0) (def j 0) (def cmd (nth commands i)) (while (< j (count cmd)) (do (def ch (subs cmd j (+ j 1))) (when (and (>= ch "A") (<= ch "Z")) (def count (+ count 1))) (def j (+ j 1)))) (def mins (conj mins count)) (def i (+ i 1)))) (def sentence "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin") (def words (fields sentence)) (def results (validate commands words mins)) (def out1 "user words:  ") (def k 0) (while (< k (count words)) (do (def out1 (str (str out1 (padRight (nth words k) (count (nth results k)))) " ")) (def k (+ k 1)))) (println out1) (println (str "full words:  " (join results " ")))))

(defn -main []
  (main))

(-main)
