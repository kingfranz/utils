(defproject org.clojars.kingfranz/utils "0.2.5"
	:description "my own utils"
	:url "http://soahojen.se"
	:license {:name "Eclipse Public License"
	:url "http://www.eclipse.org/legal/epl-v10.html"}
	:dependencies [[org.clojure/clojure "1.9.0"]
                   [expound "0.5.0"]]
	:deploy-repositories [["releases" :clojars]]
	:repositories [["releases" {:url "https://org.clojars.kingfranz/utils"
								:creds :gpg}]]
	)
