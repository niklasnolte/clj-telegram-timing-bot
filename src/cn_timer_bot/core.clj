(ns cn-timer-bot.core (:require [clj-http.client :as client]
                                [clojure.data.json :as json]
                                [clojure.string :as string]
                                [java-time :as date]))

(def bot-state {:target (ref (date/local-date-time))
                :latest-message-id (ref 0)
                :datetime-format-str "dd.MM.yyyy HH:mm"
                :bot-token (string/join "" (drop-last (slurp "bot-token.txt")))})

(defn getURL [key]
  (let [base-url  "https://api.telegram.org/bot"
        url (str base-url (bot-state :bot-token))]
    (case key
      :updates (str url "/GetUpdates")
      :getme (str url "/GetMe")
      :send (str url "/SendMessage"))))

(defn extractBody [update]
  (if (= (update :status) 200)
    (let [body (json/read-str (update :body))]
      (if (body "ok")
        (body "result")
        nil))
    nil))

(defn get-updates
  ([id-offset]
   (let [url (getURL :updates)
         return (client/post url
                             {:form-params
                              {:timeout 100,
                               :offset id-offset,
                               :allowed_updates ["message"]}})]
     (extractBody return)))
  ([] (get-updates nil)))


(defn id-of [update]
  (update "update_id"))

(defn find-latest-message [updates]
  (apply max-key id-of updates))

(defn mark-read [updates]
  (dosync (->> updates
               find-latest-message
               id-of
               (ref-set (bot-state :latest-message-id)))))

(defn sendMessage [chat-id text]
  (let [url (getURL :send)
        return (client/post url {:form-params
                                 {:chat_id chat-id,
                                  :text text}})]
    (= 200 (return :status))))

(defn get-command-from [text]
  (let [cmd (first (string/split text #" "))]
    (case cmd
      "/set" :set
      "/info" :info
      "/until" :until
      :not-a-command)))

(defn parse-time [time-str]
  (date/local-date-time (bot-state :datetime-format-str) time-str))

(defn set-time [text]
  (let [args (drop 1 (string/split text #" "))]
    (try ; "DD:MM:YYYY HH:MM"
      (dosync
       (ref-set
        (bot-state :target)
        (parse-time
         (string/join " " args))))
      "Time set."
      ; for some reason i cannot catch DateTimeParseExceptions..
      (catch java.lang.RuntimeException _
        "Usage: \"/set DD.MM.YYYY HH:MM\""))))

(defn print-duration-human-readable [duration]
  (let [days (.toDays duration)
        hours (mod (.toHours duration) 24)
        minutes (mod (.toMinutes duration) 60)
        seconds (mod (.toSeconds duration) 60)]
    (string/join " "
               (list days "days"
                     hours "hours"
                     minutes "minutes"
                     seconds "seconds"
                     "❤️"))))


(defn print-time-until-target []
  (print-duration-human-readable (date/duration (date/local-date-time) @(bot-state :target))))

(print-time-until-target)

(defn print-target-time []
  (str @(bot-state :target)))

(defn respond-to [text]
  (case (get-command-from text)
    :set (set-time text)
    :until (print-time-until-target)
    :info (print-target-time)
    :not-a-command "please provide one of the supported commands"))

(defn message-or-edited [message]
  (or (message "message") (message "edited_message")))

(defn respond [message]
  (let [message (message-or-edited message)
        chat-id ((message "chat") "id")
        text (respond-to (message "text"))]
    (sendMessage chat-id text)))

(defn wait-and-respond []
  (while true
    (let [new-messages (get-updates (+ 1 @(bot-state :latest-message-id)))] ;this is waiting
      (if (or (nil? new-messages) (= new-messages []))
        (println "waiting")
        (do (doall (map respond new-messages))
            (mark-read new-messages))))))

(wait-and-respond)
