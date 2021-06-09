(ns hexagram-scraper.rescrape
  (:require [hexagram-scraper.helpers :refer [list-1-64]]
            [net.cgrand.enlive-html :as html]
            [clojure.pprint :as pp]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start web scraper functions
;; ---------------------------
;; * There should already be a base file to read from that
;; already has minor edits for easier scraping.
;; Do not use unless you can account for more edge cases. *
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (defn GBCh-url [idx]
   (java.net.URL. (str "http://www.jamesdekorne.com/GBCh/hex" idx ".htm")))

 (def all-GBCh-resources
   (zipmap (map #(keyword (str %)) list-1-64)
           (for [idx list-1-64] (html/html-resource (GBCh-url idx)))))

 (spit "resources/all-GBCh-data-rescrape.edn"
       (with-out-str (pp/pprint all-GBCh-resources)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End web scraper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
