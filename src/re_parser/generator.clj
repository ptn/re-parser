(ns re-parser.generator)

;; Test with:
;;
;; (defparser parse (regex (or-regex)
;;                         (conc-repeatable)
;;                         (repeatable))
;;   (or-regex ((repeatable) \| (regex)))
;;   (conc-repeatable ((repeatable) (repeatable)))
;;   (repeatable (star)
;;               (groupable))
;;   (star ((groupable) \*))
;;   (groupable (group)
;;              (conc)
;;              (alphanum))
;;   (group (\( (regex) \)))
;;   (conc ((alphanum) (repeatable))))

(defn declare-rules [rules]
  `(declare ~@(map first rules)))

(defn or-rule [rule]
  (let [string (gensym)
        body `(or ~@(map (fn [subrule]
                           `(~@subrule ~string))
                         (rest rule)))]
    `(defn ~(first rule) [~string] ~body)))

(defn simple-body [param name body & [parsed unparsed]]
  (cond
   (empty? body)
   `[[~(keyword name) ~@(reverse (remove nil? parsed))] ~unparsed]

   (list? (first body))
   (let [new-parsed (gensym)
         new-unparsed (gensym)]
     `(when-let [[~new-parsed ~new-unparsed] (~(ffirst body)
                                              ~(if (nil? unparsed)
                                                 param
                                                 unparsed))]
        ~(simple-body param name (rest body) (conj parsed new-parsed) new-unparsed)))

   (char? (first body))
   (if (nil? unparsed)
     `(when (= (first ~param) ~(first body))
        ~(simple-body param name (rest body) parsed `(.substring ~param 1)))
     `(when (= (first ~unparsed) ~(first body))
        ~(simple-body param name (rest body) parsed `(.substring ~unparsed 1))))))

(defn simple-rule [rule]
  (let [string (gensym)]
    `(defn ~(first rule) [~string]
       ~(simple-body string (first rule) (fnext rule)))))

(defn define-rules [rules]
  (map (fn [rule]
         (if (> (count rule) 2)
           (or-rule rule)
           (simple-rule rule)))
       rules))

(defmacro defparser
  [parser-name & rules]
  (let [string (gensym)]
    `(do
       ~(declare-rules rules)

       (defn alphanum [string#]
         (when (#{\1\2\3\4\5\6\7\8\9\0\a\b\c\d\e\f\g\h\i\j\k\l\m\n\o\p\q\r\s\t\u\v\w\x\y\z}
                (first string#))
           [(first string#) (.substring string# 1)]))

       ~@(define-rules rules)

       (defn ~parser-name [~string]
         (let [[parsed# unparsed#] (~(ffirst rules) ~string)]
           parsed#)))))
