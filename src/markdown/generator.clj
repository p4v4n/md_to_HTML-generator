;;Generator

(ns markdown.generator)

(defn converter [input]
	(let [t (:tag input)]
		(if (string? (:content input))
		(cond
			(= t "link") (str "<a href = \"" (:url input) "\">" (:content input) "</a>")
			(= t "image") (str "<img src = \"" (:path input) "\" alt = \"" (:content input) "\" >" )
			(= t "code_block") (str "<pre><code>" (:content input) "</code></pre>")
			(or (= t "text") (= t "linebreak") (= t "hr")) (str (:content input))
			:else (str "<" t ">" (:content input) "</" t ">"))
		(str "<" t ">" (reduce str (map converter (:content input))) "</" t ">"))))


(defn generator [input]
	(loop [x input y []]
		(if (empty? x)
			y
			(recur (rest x) (conj y (converter (first x)))))))


(defn write-to-html [input]
	(loop [x input]
		(when (not (empty? x))
			(spit "result.html" (str (first x) "\n") :append true)
            (recur (rest x)))))