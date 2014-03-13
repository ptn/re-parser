(ns re-parser.core)

(declare regex)

(defn group [string]
  (when (and (= (first string) \()
             (= (last string) \)))
    (when-let [reg-result (regex (.substring string 1 (dec (count string))))]
      [:group reg-result ])))

(defn conc [string]
  (when (#{\1\2\3\4\5\6\7\8\9\0\a\b\c\d\e\f\g\h\i\j\k\l\m\n\o\p\q\r\s\t\u\v\w\x\y\z}
         (first string))
    (if (= 1 (count string))
      (first string)
      (when-let [rep-result (repeatable (.substring string 1))]
        [:conc (first string) rep-result]))))

(defn groupable [string]
  (or (group string)
      (conc string)))

(defn star [string]
  (when (= (last string) \*)
    (when-let [gr-result (groupable (.substring string 0 (dec (count string))))]
      [:star gr-result])))

(defn repeatable [string]
  (or (groupable string)
      (star string)))

(defn or-regex [string]
  (let [pipe-pos (.indexOf string "|")]
    (when-not (= pipe-pos -1)
      (let [repeatable-str (.substring string 0 pipe-pos)
            regex-str (.substring string (inc pipe-pos))]
        (let [rep-result (repeatable repeatable-str)
              reg-result (regex regex-str)]
          (when (and rep-result reg-result)
            [:or rep-result reg-result]))))))

(defn regex [string]
  (or (or-regex string)
      (repeatable string)))
