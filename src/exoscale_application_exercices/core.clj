(ns exoscale-application-exercices.core
  (:gen-class))

(def events
  [{:usage/event     :usage.event/create
    :usage/resource  :usage.resource/object
    :usage/uuid      #uuid "d8377d93-db71-488a-b894-54a962760bea"
    :usage/account   #uuid "ee12577c-983f-4729-a0e9-c5789a906c04"
    :usage/timestamp #inst "2017-03-10T00:00:00.000-00:00"}
   {:usage/event     :usage.event/destroy
    :usage/resource  :usage.resource/object
    :usage/uuid      #uuid "d8377d93-db71-488a-b894-54a962760bea"
    :usage/account   #uuid "ee12577c-983f-4729-a0e9-c5789a906c04"
    :usage/timestamp #inst "2017-03-10T01:00:00.000-00:00"}])

(defn process-usage [usage-records]
  "This function processes a collection of usage records and produces
   a collection of billing statements (one per account)."
  (let [grouped-by-account-usage-records (group-by #(:usage/account %) usage-records)]
    (vec (map (fn [[_ account-usage-records]]
                (let [{[start end] :usage/duration :as billing-record}
                      (reduce (fn [acc usage-record]
                                (merge acc (let [duration-init (first (:usage/duration acc))
                                                 {resource :usage/resource
                                                  duration-last :usage/timestamp} usage-record]
                                             {:usage/duration [duration-init duration-last]
                                              :usage/resource resource})))
                              (let [{resource :usage/resource
                                     account :usage/account
                                     first-inst :usage/timestamp
                                     uuid :usage/uuid} (first account-usage-records)]
                                {:usage/resource resource
                                 :usage/uuid uuid
                                 :usage/account account
                                 :usage/duration [first-inst]})
                              account-usage-records)]
                  (merge
                   billing-record
                   {:usage/duration (/ (- (.getTime end) (.getTime start)) 60000)})))
              grouped-by-account-usage-records))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (assert (= (process-usage events)
             [{:usage/resource  :usage.resource/object
               :usage/uuid      #uuid "d8377d93-db71-488a-b894-54a962760bea"
               :usage/account   #uuid "ee12577c-983f-4729-a0e9-c5789a906c04"
               :usage/duration  60}])))
