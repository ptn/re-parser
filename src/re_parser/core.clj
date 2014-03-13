(ns re-parser.core)

;; Grammar:

;; regex := or-regex
;;       |  conc-repeatable
;;       |  repeatable

;; or-regex := repeatable '|' regex

;; conc-repeatable := repeatable repeatable

;; repeatable := groupable
;;            |  star

;; star := groupable '*'

;; groupable := group
;;           |  concat

;; group := '(' regex ')'

;; concat := alphanum repeatable
;;        |  alphanum


(declare regex repeatable)

(defn group [string]
  (when (= (first string) \()
    (when-let [[reg-parsed reg-unparsed] (regex (.substring string 1))]
      (when (= (first reg-unparsed) \))
        [[:group reg-parsed] (.substring reg-unparsed 1)]))))

(defn conc [string]
  (when (#{\1\2\3\4\5\6\7\8\9\0\a\b\c\d\e\f\g\h\i\j\k\l\m\n\o\p\q\r\s\t\u\v\w\x\y\z}
         (first string))
    (if-let [[rep-parsed rep-unparsed] (repeatable (.substring string 1))]
      [[:conc (first string) rep-parsed] rep-unparsed]
      [(first string) (.substring string 1)])))

(defn groupable [string]
  (or (group string)
      (conc string)))

(defn star [string]
  (when-let [[gr-parsed gr-unparsed] (groupable string)]
    (when (= (first gr-unparsed) \*)
      [[:star gr-parsed] (.substring gr-unparsed 1)])))

(defn repeatable [string]
  (or (star string)
      (groupable string)))

(defn or-regex [string]
  (when-let [[rep-parsed rep-unparsed] (repeatable string)]
    (when (= (first rep-unparsed) \|)
      (when-let [[reg-parsed reg-unparsed] (regex (.substring rep-unparsed 1))]
        [[:or rep-parsed reg-parsed] reg-unparsed]))))

(defn conc-repeatable [string]
  (when-let [[rep1-parsed rep1-unparsed] (repeatable string)]
    (when-let [[rep2-parsed rep2-unparsed] (repeatable rep1-unparsed)]
      [[:conc rep1-parsed rep2-parsed] rep2-unparsed])))

(defn regex [string]
  (or (or-regex string)
      (conc-repeatable string)
      (repeatable string)))

(defn parse [string]
  (let [[parsed unparsed] (regex string)]
    parsed))
