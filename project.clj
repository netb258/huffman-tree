(defproject huffman-tree "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  ;; :plugins [[cider/cider-nrepl "0.49.0"]]
  :dependencies [[org.clojure/clojure "1.11.3"]
                 [org.clojure/tools.cli "1.4.256"]
                 [metosin/malli "0.20.1"]]
  :main ^:skip-aot huffman-tree.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
