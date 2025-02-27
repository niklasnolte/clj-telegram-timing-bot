(defproject cn_timer_bot "0.1.0-SNAPSHOT"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/data.json "2.4.0"]
                 [clj-http "3.12.3"]
                 [clojure.java-time "0.3.3"]]

  ;:repl-options {:init-ns cn-timer-bot.core}
  :main cn_timer_bot.core
  :aot :all)
