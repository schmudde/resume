(defproject resume "0.1.0-SNAPSHOT"
  :description "This generates a HTML resume. `lein run > test.html` creates the file. Use Prince to turn the HTML file into a PDF: `prince test.html -o test.pdf`"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [clj-template "1.0.1"]]
  :main ^:skip-aot resume.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
