;; Markdown to HTML parser.

(ns markdown.parser)
(use '[clojure.string])

(defn any_one_parser_factory [& args]
	(fn [data] ((reduce #(if (%1 data) %1 %2) args) data)))


(defn text_parser [data]
   (let [x (re-find #"^(([a-zA-Z0-9\?\-\.\,\:])+|(( (?! +\n)))+)+" data)]
      (if x (list (hash-map :tag "text" :content (subs data 0 (count (first x)))) (subs data (count (first x)))))))	


(defn link_parser [data]
	(if (starts-with? data "[")
		(let [[a b] (split data #"\)" 2)
			  [c d] (split a #"]" 2)
			]
			(if (ends-with? d "\"")
				(let [[e f g] (split d #"\"" 3)]
					(list (hash-map :tag "link" :content (subs c 1) :url (subs e 1 (- (count e) 1)) :title f) b)
					)
			(list (hash-map :tag "link" :content (subs c 1) :url (subs d 1)) b)))))


(defn image_parser [data]
	(if (starts-with? data "![")
		(let [[a b] (split data #"\)" 2)
			  [c d] (split a #"]" 2)
			]
			(if (ends-with? d "\"")
				(let [[e f g] (split d #"\"" 3)]
					(list (hash-map :tag "image" :content (subs c 2) :path (subs e 1 (- (count e) 1)) :title f) b)
					)
			(list (hash-map :tag "image" :content (subs c 2) :path (subs d 1)) b)))))


(defn emphasis_parser [data]
	(cond 
		(starts-with? data "**") (let [[a b c] (split data #"\*\*" 3)] (list (hash-map :tag "strong" :content b) c))
		(starts-with? data "__") (let [[a b c] (split data #"__" 3)] (list (hash-map :tag "strong" :content b) c))
		(starts-with? data "*") (let [[a b c] (split data #"\*" 3)] (list (hash-map :tag "em" :content b) c))
		(starts-with? data "_") (let [[a b c] (split data #"_" 3)] (list (hash-map :tag "em" :content b) c))))


(defn line_break_parser [data]
	(if (starts-with? data "\n")
		(let [[a b] (split data #"\n" 2)]
		(list (hash-map :tag "linebreak" :content a) b))))


(defn para_line_break_parser [data]
	(if (re-find #"^ {2,}\n" data)
		(let [[a b] (split data #"\n" 2)]
			(list (hash-map :tag "linebreak",:content "<br />") b))))


(defn code_parser [data]
	(if (starts-with? data "`")
		(let [[a b c] (split data #"`" 3)]
			(list (hash-map :tag "code",:content b) c))))



(def span_parser (any_one_parser_factory text_parser link_parser image_parser emphasis_parser line_break_parser para_line_break_parser code_parser))


(defn tag_parser [data]
	(loop [a [] b data]
		(if (empty? b)
			a
			(recur (conj a (first (span_parser b))) (last (span_parser b))))))


(defn heading_parser [data]
	(if (starts-with? data "#")
		(let [[a b] (split data #"\n" 2)
			  [c d] (split a #" " 2)]
			   (list (hash-map :tag (str "h" (str (count c))),:content (tag_parser (trim (replace (subs a (+ 1 (count c))) "#" "")))) b))
		(let [[a b c] (split data #"\n" 3)]
			(cond (= (set (trim b)) #{\=}) (list (hash-map :tag "h1",:content (tag_parser (trim a))) c)
				(= (set (trim b)) #{\-}) (list (hash-map :tag "h2",:content (tag_parser (trim a))) c)
				))))


(defn paragraph_parser [data]
	(if (re-find #"^ {0,3}\w" data)
		(let [[a b] (split data #"\n\n" 2)]
			(list (hash-map :tag "p",:content (tag_parser (trim a))) b))))


(defn unordered_list_parser [data]
	(if (re-find #"^\s*\*\s+\w" data)
		(let [[a b] (split data #"\n\n" 2)
			 li_el (map trim (map #(subs % 1)(map trim (split a #"\n"))))]
			 (list (hash-map :tag "ul",:content (map #(hash-map :tag "li",:content %)(map tag_parser li_el))) b))))


(defn ordered_list_parser [data]
	(if (re-find #"^\s*\d.\s+\w" data)
		(let [[a b] (split data #"\n\n" 2)
			 li_el (map trim (map #(subs % 2)(map trim (split a #"\n"))))]
			 (list (hash-map :tag "ol",:content (map #(hash-map :tag "li",:content %)(map tag_parser li_el))) b))))

(defn horizontal_line_parser [data]
	(if (or (re-find #"^ *\* *\* *\*[ \*]*" data) (re-find #"^ *\- *\- *\-[ \-]*" data))
		(let [[a b] (split data #"\n" 2)]
			(list (hash-map :tag "hr",:content "<hr />") b))))


(defn code_block_parser [data]
	(if (re-find #"^ {4,}" data)
		(let [[a b] (split data #"\n(?! {4})" 2)]
			(list (hash-map :tag "code_block",:content a) b))))


(defn block_quote_parser [data]
	(if (starts-with? data ">")
		(let [[a b] (split data #"\n\n" 2)]
			(list (hash-map :tag "blockquote",:content (tag_parser (subs a 1))) b))))


(def block_parser (any_one_parser_factory heading_parser link_parser image_parser block_quote_parser line_break_parser unordered_list_parser ordered_list_parser paragraph_parser horizontal_line_parser code_block_parser))


(defn markdown_parser [data]
	(loop [x data y []]
	(if (empty? x)
		y
	(let [[a b] (block_parser x)]
		(recur b (conj y a))))))