(ns json-parser.core
  (:gen-class))

(use '[clojure.string :exclude (replace reverse)]
     'clojure.pprint
     '[indent.indent :only (indent-dispatch)])

(def json-str "")
(def error-msg "ERROR : Invalid input JSON")
(def error false)

(defn print-json-str
  []
  (println (str "json-str : " json-str)))

(defn exit
  []
  (def error true))

(defn triml-json-str
  []
  (def json-str (triml json-str)))

(defn -main
  "Prints result map if input is ok, error message otherwise."
  [& args]
  (let [input-filename (first args)]
    (def json-str (slurp input-filename))
    (declare parse-value)
    (let [json (parse-value)]
      (if (and json (blank? json-str) (not error))
        (with-pprint-dispatch indent.indent/indent-dispatch (pprint (first json)))
        (println error-msg)))))

(defn parse-object
  "Returns a hash-map equivalent to JSON-STR if JSON-STR is properly formed, NIL otherwise."
  []
  (triml-json-str)
  (when (starts-with? json-str "{")
    (def json-str (subs json-str 1))
    (loop [json-map {}]
      (triml-json-str)
      (if (starts-with? json-str "}")
        (do (def json-str (subs json-str 1))
            json-map)
        (do (declare parse-key)
            (let [key (parse-key)]
              (triml-json-str)
              (if (and key (starts-with? json-str ":"))
                (do (def json-str (subs json-str 1))
                    (let [value (parse-value)]
                      (triml-json-str)
                      (let [comma? (starts-with? json-str ",")]
                        (if (and value (or comma? (starts-with? json-str "}")))
                          (do (when comma?
                                (def json-str (subs json-str 1)))
                              (recur (into json-map {key (first value)})))
                          (exit)))))
                (exit))))))))

(defn parse-array
  []
  (triml-json-str)
  (when (starts-with? json-str "[")
    (def json-str (subs json-str 1))
    (loop [vec []]
      (triml-json-str)
      (if (starts-with? json-str "]")
        (do (def json-str (subs json-str 1))
            vec)
        (do (let [value (parse-value)]
              (triml-json-str)
              (let [comma? (starts-with? json-str ",")]
                (if (and value (or comma? (starts-with? json-str "]")))
                  (do (when comma?
                        (def json-str (subs json-str 1)))
                      (recur (into vec value)))
                  (exit)))))))))

(defn parse-key
  []
  (declare parse-string)
  (parse-string))

(defn parse-string
  []
  (triml-json-str)
  (declare index-of-closing-quotes)
  (when (starts-with? json-str "\"")
    (let [end (index-of-closing-quotes)]
      (if end
        (let [string (subs json-str 1 end)]
          (def json-str (subs json-str (inc end)))
          string)
        (exit)))))

(defn parse-value
  "Returns a value wrapped in a vector if it can parse one, nil otherwise."
  []
  (declare parse-null-or-bool parse-number parse-object parse-array)
  (let [null-or-bool (parse-null-or-bool)]
    (if null-or-bool
      null-or-bool
      (let [rest (or (parse-string) (parse-number) (parse-object) (parse-array))]
        (when rest
          [rest])))))

(defn parse-null-or-bool
  []
  (declare check)
  (or (check "true") (check "false") (check "null")))

(defn check
  "checks whether JSON-STR starts with TOKEN. If yes, removes it from JSON-STR and returns corresponding clojure value in a vector. Otherwise returns nil"
  [token]
  (triml-json-str)
  (when (starts-with? json-str token)
    (let [json-str-temp (triml (subs json-str (count token)))]
      (when (or (blank? json-str-temp) (starts-with? json-str-temp ",") (starts-with? json-str-temp "}") (starts-with? json-str-temp "]"))
        (def json-str json-str-temp)
        (case token
          "true" [true]
          "false" [false]
          "null" [nil])))))

(defn parse-number
  []
  (triml-json-str)
  (let [start-char (get json-str 0)]
    (declare digit?)
    (when (or (digit? start-char) (= start-char \-))
      (declare non-null-index-of)
      (let [end (min (non-null-index-of json-str \,) (non-null-index-of json-str \t) (non-null-index-of json-str \n) (non-null-index-of json-str \space) (non-null-index-of json-str \]) (non-null-index-of json-str \}))
            numstr (subs json-str 0 end)
            num (read-string numstr)]
        (if (number? num)
          (do (def json-str (subs json-str end))
              num)
          (exit))))))

(defn digit?
  [char]
  (<= (int \0) (int char) (int \9)))

(defn non-null-index-of
  "Gives the index of first occurence of CHAR in STR if its there, otherwise length of STR"
  [str char]
  (let [index (index-of str char)]
    (or index (count str))))

(defn index-of-closing-quotes
  []
  (loop [index 0]
    (let [temp-index (index-of json-str \" (inc index))]
      (when temp-index
        (if (= (get json-str (dec temp-index)) \\)
          (recur temp-index)
          temp-index)))))
