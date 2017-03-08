(ns markdown.core
  (:gen-class))

(require '[clojure.tools.cli :refer [cli]])

(use 'markdown.parser)
(use 'markdown.generator)

(defn -main
  "markdown to html"
  [input]
  (write-to-html ["<body>"])
  (write-to-html (generator (markdown_parser (slurp input))))
  (write-to-html ["</body>"]))