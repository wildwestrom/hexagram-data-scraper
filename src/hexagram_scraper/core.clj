(ns hexagram-scraper.core
  (:require
   [net.cgrand.enlive-html :as html]
   [hexagram-scraper.hard-coded-values :refer [fixed-values]]
   [hexagram-scraper.helpers :refer [list-1-64
                                     int-to-keyword
                                     indices
                                     index-to-hexagram-unicode-character]]
   [clojure.string :as string]
   [clojure.pprint :as pp]
   [clojure.edn :as edn]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start data intake functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def all-GBCh-resources
  (edn/read-string (slurp "resources/all-GBCh-data.edn")))

(defn GBCh-resource-entry
  [idx]
  (map #(:content %)
       (filter #(not (string? %))
               (:content (first (html/select
                                 ((keyword (str idx)) all-GBCh-resources)
                                 [:div.psymain (keyword (str "div.hex" idx))]))))))

(defn filter-out-empty-lines
  "Get rid of those nasty nbsp and \n characters."
  [str]
  (let [check-for-matches
        (fn [re s] (not (nil? (re-find re s))))]
    (filter #(if (and (string? (first %))
                      (or (check-for-matches #"\u00A0" (first %))
                          (check-for-matches #"\n" (first %))))
               false %) str)))

(defn all-elements
  [idx]
  (into [] (filter-out-empty-lines
            (GBCh-resource-entry idx))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End data intake functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn index-of-section
  [elements-list re]
  (if (= re "last")
    (apply max (indices #(identity %) elements-list))
    (apply min (indices #(or (if (-> % first :content first string?)
                               (re-find re (-> % first :content first)) nil)
                             (if (-> % first string?)
                               (re-find re (first %))
                               nil) nil) elements-list))))

(def section-key-regex-map
  (let [regexes [#"Judgment" #"The Image"
                 #"COMMENTARY" #"NOTES AND PARAPHRASES"
                 #"(?i)Line.1" #"(?i)Line.2" #"(?i)Line.3"
                 #"(?i)Line.4" #"(?i)Line.5" #"(?i)Line.6"
                 "last"]
        kwds    [:judgment :the-image :commentary :notes-&-para
                 :line-1 :line-2 :line-3 :line-4 :line-5 :line-6]]
    (zipmap kwds
            (for [n (range (dec (count regexes)))]
              [(regexes n) (regexes (inc n))]))))

(defn extract-strings-from-coll
  [coll]
  (flatten (map #(filter string? (tree-seq associative? :content %)) coll)))

(defn get-section
  [elements-list [re-start re-end]]
  (let [beginning     (index-of-section elements-list re-start)
        ending        (index-of-section elements-list re-end)
        section-range (range (inc beginning) ending)]
    (for [idx section-range]
      (extract-strings-from-coll (get (into [] elements-list) idx)))))

(defn get-all-sections
  [elements-list]
  (zipmap (keys section-key-regex-map)
          (map #(get-section elements-list (val %)) section-key-regex-map)))

(defn get-sub-section-names
  [elements-list section-key]
  (let [replace-colons     #(string/replace % #":" "")
        replace-slashes    #(string/replace % #"/" "-")
        replace-parens     #(string/replace % #"\((\d)\)" "-$1")
        replace-whitespace #(string/replace % #"\s" "-")
        replace-doubledash #(string/replace % #"--" "-")
        replace-fn         #(-> % replace-colons replace-slashes replace-parens
                                replace-whitespace replace-doubledash)]
    (map #(keyword (string/lower-case (replace-fn (string/trim %))))
         (filter #(->> % nil? not) (map first (get-section elements-list (section-key section-key-regex-map)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start "other titles" functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn extract
  [x]
  (-> x first :content first))

(defn get-stuff
  [elements-list re]
  (filter #(->> % nil? not)
          (rest (map #(if (string? %) % (first (:content %)))
                     (first (filter #(if (string? (extract %))
                                       (re-find re (extract %)) nil) elements-list))))))

(defn filter-quotes
  [stuff]
  (filter #(not (empty? %))
          (split-at 2 (filter #(string? %) (rest stuff)))))

(defn filter-fn
  [s re sub]
  (if (string? s) (string/replace s re sub) nil))

(defn get-titles-of-other-titles
  [stuff]
  (map #(filter-fn (string/trim %) #"\.$" "") (string/split (first stuff) #",\s")))

(defn get-other-titles
  [elements-list]
  (let [stuff         (get-stuff elements-list #"(?i)Other titles")
        titles        (get-titles-of-other-titles stuff)
        quotes        (filter-quotes stuff)]
    (if (>= 1 (count stuff))
      {:titles titles}
      {:titles             titles
       :quotes (for [pair quotes] {:author (filter-fn (string/trim (second pair)) #"[-–]+\s?|\.$" "")
                                   :quote (string/replace  (string/trim (first pair))
                                                           #"\"|--|\s--"
                                                           "")})})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End "other titles" functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-name
  [elements-list]
  (let [finder #(re-find #"-- ([^\d]+) --" %)]
    (second (finder (first (filter
                            #(not (nil? (if (string? %) (finder %) nil)))
                            (map first elements-list)))))))

(defn get-sections-map
  [elements-list]
  (reduce-kv (fn [m k v] (assoc m k
                                (zipmap (get-sub-section-names elements-list k)
                                        (into [] (map #(string/trim (string/replace (apply str %)
                                                                                    #"\""
                                                                                    ""))
                                                      (map rest v))))))
             {}
             (get-all-sections elements-list)))

#_(def idx 1)

#_(def elements-list (all-elements idx))

#_(get-sections-map elements-list)

#_(get-all-sections elements-list)

(defn get-hexagram-map
  [idx]
  (let [binary        (:binary ((int-to-keyword idx) fixed-values))
        elements-list (all-elements idx)
        all-sections  (get-sections-map elements-list)
        other-titles  (get-other-titles elements-list)
        name          (get-name elements-list)]
    {:number       idx
     :binary       binary
     :char         (index-to-hexagram-unicode-character idx)
     :name         name
     :other-titles other-titles
     :judgment (:judgment all-sections)
     :the-image (:the-image all-sections)
     :commentary (:commentary all-sections)
     :notes-&-para (:notes-&-para all-sections)
     :line-1 (:line-1 all-sections)
     :line-2 (:line-2 all-sections)
     :line-3 (:line-3 all-sections)
     :line-4 (:line-4 all-sections)
     :line-5 (:line-5 all-sections)
     :line-6 (:line-6 all-sections)}))

(def all-hexagrams
  (for [idx list-1-64]
    (get-hexagram-map idx)))

(spit "resources/hexagrams.edn"
      (with-out-str (pp/pprint all-hexagrams)))

(for [idx list-1-64]
  (let [filename (str "resources/hexagrams.d/hexagram-" idx ".edn")]
    (spit filename
          (with-out-str (pp/pprint (get-hexagram-map idx))))))

#_(edn/read-string (slurp "resources/hexagrams.edn"))
