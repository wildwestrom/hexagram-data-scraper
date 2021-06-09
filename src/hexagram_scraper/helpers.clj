(ns hexagram-scraper.helpers)

(def list-1-64
  (range 1 65))

(defn int-to-keyword
  [^long x]
  (keyword (str x)))

(defn indices [pred coll]
  (keep-indexed #(when (pred %2) %1) coll))

(defn index-to-hexagram-unicode-character
  "Takes a hexagram number starting at `1`
  and returns the unicode character for that hexagram.
  This assumes the King Wen sequence."
  [idx]
  (char (+ 19903 idx)))
