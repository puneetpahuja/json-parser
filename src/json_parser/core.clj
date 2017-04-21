(ns json-parser.core
  (:gen-class))

(use '[clojure.string :exclude (replace reverse)]
     'clojure.pprint
     '[indent.indent :only (indent-dispatch)])

(defn remove-from-start
  [str substr]
  (subs str (count substr)))

(defn parse-space
  [str]
  (let [space (re-find #"^\s+" str)]
    (if space
      [space (remove-from-start str space)]
      [nil str])))

(defn parse-number
  [str]
  (let [str (second (parse-space str))
        numstr (re-find #"^\-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?" str)]
    (when numstr
      [(read-string numstr) (remove-from-start str numstr)])))

(defn parse-char
  [str char]
  (let [str (second (parse-space str))
        first-char (first str)
        parseable? (= char first-char)]
    (when parseable?
      [char (subs str 1)])))

(defn parse-word
  [str word parsed-object]
  (let [str (second (parse-space str))
        token (re-find (re-pattern (clojure.core/str "^" word  "($|[,\\s\\]\\}])")) str)]
    (when token
      [parsed-object (remove-from-start str word)])))

(defn closing-quotes-index
  [str]
  (loop [index 1]
    (when (< index (count str))
      (let [temp (index-of str \" index)]
        (when temp
          (if (= (get str (dec temp)) \\)
            (recur (inc temp))
            temp))))))


(defn parse-string
  [str]
  (let [str (second (parse-space str))
        string (parse-char str \")]
    (when string
      (let [end-index (closing-quotes-index str)]
        (when end-index
          [(subs str 1 end-index) (subs str (inc end-index))])))))

(defn parse-bool
  [str]
  (or (parse-word str "true" true) (parse-word str "false" false)))

(defn parse-null
  [str]
  (parse-word str "null" nil))

(def parse-key parse-string)

(defn parse-object
  "Returns a hash-map equivalent to JSON-STR if JSON-STR is properly formed, NIL otherwise."
  [str]
  (let [object (parse-char str \{)]
    (when object
      (loop [json-map {}
             str (second object)
             object-end (parse-char str \})]
        (if object-end
          [json-map (second object-end)]
          (let [key (parse-key str)]
            (when key
              (let [colon (parse-char (second key) \:)]
                (when colon
                  (declare parse-value)
                  (let [value (parse-value (second colon))]
                    ;(println (clojure.core/str (first key) " : " (first value)))
                    (when value
                      (let [comma (parse-char (second value) \,)
                            end (parse-char (second value) \})]
                        (when (or comma end)
                          (if comma
                            (recur (into json-map {(first key) (first value)}) (second comma) end)
                            (recur (into json-map {(first key) (first value)}) (second end) end)))))))))))))))

(defn parse-array
  [str]
  (let [array (parse-char str \[)]
    (when array
      (loop [vec []
             str (second array)
             array-end (parse-char str \])]
        (if array-end
          [vec (second array-end)]
          (let [value (parse-value str)]
            (when value
              (let [comma (parse-char (second value) \,)
                    end (parse-char (second value) \])]
                (when (or comma end)
                  (if comma
                    (recur (into vec [(first value)]) (second comma) end)
                    (recur (into vec [(first value)]) (second end) end))))))))))) 

(defn parse-value
  [str]
  (def value (or (parse-null str) (parse-bool str) (parse-string str) (parse-number str) (parse-object str) (parse-array str)))
  ;(println (clojure.core/str "value : " (first value)))
  value)


(defn -main
  "Pretty prints result map if input string is ok, prints error message otherwise."
  [& args]
  (let [input-filename (first args)
        json-str (slurp input-filename)
        error-msg "ERROR : Invalid input JSON"
        json (parse-value json-str)]
    (if (and json (blank? (second json)))
      (with-pprint-dispatch indent.indent/indent-dispatch (pprint (first json)))
      (println error-msg))))
