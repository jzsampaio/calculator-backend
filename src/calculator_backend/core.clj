(ns calculator-backend.core
  (:require [cats.monad.either :as either]
            [cats.core :refer [mlet]]
            [cheshire.core :as json]
            [io.pedestal.http :as http]
            [io.pedestal.http.body-params :as body-params]
            [io.pedestal.http.content-negotiation :as conneg]
            [io.pedestal.http.route :as route]
            [malli.core :as m]
            [malli.error]
            [medley.core :as medley]

            [calculator-backend.app-errors :as app-errors])

  (:import java.lang.Math)
  (:gen-class))

(defn extract-params [request]
  (first (remove empty? [(:params request)
                         (:json-params request)
                         (:query-params request)])))

(defn validate [request expected-type]
  (let [o (extract-params request)]
    (if (m/validate expected-type o)
      (either/right o)
      (either/left (app-errors/new-error ::app-errors/request-body-error
                                         (malli.error/humanize (m/explain expected-type o)))))))

(defn response-with-status
  ([status-code] (response-with-status status-code nil nil))
  ([status-code body result-keyword]
   (let [response-body (if (some? result-keyword) {result-keyword body} body )]
     (-> {:status status-code}
         (medley/assoc-some :body response-body)))))

(defn ok [body]
  (response-with-status 200 body :result))

(defn error-response [{e :error details :details}]
  (let [status-code (case e
                      ::app-errors/not-found          404
                      ::app-errors/request-body-error 400
                      ::app-errors/internal-error     500
                      :default                        500)]
    (response-with-status status-code details :error)))

;; TODO could this be moved to an interceptor? The interceptor would
;; turn Either responses into coplete http responses
(defn either->http-response [m]
  (either/branch
   m
   error-response
   ok))

(def numeric-binary-schema
  (m/schema [:map
             [:x0 :int]
             [:x1 :int]]))

(def numeric-unnary-schema
  (m/schema [:map
             [:val :int]]))

(defn api-sum [request {:keys [x0 x1]}]
  (either/right (+ x0 x1)))

(defn api-subtract [request {:keys [x0 x1]}]
  (either/right (- x0 x1)))

(defn api-multiply [request {:keys [x0 x1]}]
  (either/right (* x0 x1)))

(defn api-divide [request {:keys [x0 x1]}]
  (either/right (/ x0 x1)))

(defn api-square-root [request {:keys [val]}]
  (either/right (Math/sqrt val)))

(def supported-types ["text/html" "application/edn" "application/json" "text/plain"])

(def content-neg-intc (conneg/negotiate-content supported-types))

(defn accepted-type
  [context]
  (get-in context [:request :accept :field] "application/json"))

(defn transform-content
  [body content-type]
  (case content-type
    "text/html"        body
    "text/plain"       body
    "application/edn"  (pr-str body)
    "application/json" (json/generate-string body)))

(defn coerce-to
  [response content-type]
  (-> response
      (update :body transform-content content-type)
      (assoc-in [:headers "Content-Type"] content-type)))

(def coerce-body
  {:name ::coerce-body
   :leave
   (fn [context]
     (cond-> context
       (nil? (get-in context [:response :headers "Content-Type"])) ;; <1>
       (update :response coerce-to (accepted-type context))))})

(def either->http-intc
  {:name ::coerce-body
   :leave
   (fn [context]
     (cond-> context
       (either/either? (:response context))
       (update :response either->http-response)))})

(defn handler-with-validation [handler-fn request-schema]
  ;; TODO missing try/catch
  (fn [request]
    (mlet [handler-args (validate request request-schema)]
          (handler-fn request handler-args))))

(defn api-interceptors [fn request-schema]
  (let [validated-handler (handler-with-validation fn request-schema)]
    [(body-params/body-params) coerce-body content-neg-intc either->http-intc validated-handler]))

;; TODO needs an erro handling for problems when parging json
(defn build-routes []
  (route/expand-routes
   #{["/api/v1/sum" :post (api-interceptors api-sum numeric-binary-schema) :route-name :sum]
     ["/api/v1/subtract" :post (api-interceptors api-subtract numeric-binary-schema) :route-name :subtract]
     ["/api/v1/multiply" :post (api-interceptors api-multiply numeric-binary-schema) :route-name :multiply]
     ["/api/v1/divide" :post (api-interceptors api-divide numeric-binary-schema) :route-name :divide]
     ["/api/v1/sqrt" :post (api-interceptors api-square-root numeric-unnary-schema) :route-name :sqrt]}))

(defn build-pedestal-service-map []
  {::http/routes (build-routes)
   ::http/type   :jetty
   ::http/port   8890})

(defn start []
  (let [jetty-server (http/create-server (build-pedestal-service-map))]
    (http/start jetty-server)))

(defonce dev-server (atom nil))

(defn start-dev-server []
  (let [jetty-server   (http/create-server (assoc (build-pedestal-service-map) ::http/join? false))
        started-server (http/start jetty-server)]
    (reset! dev-server started-server)))

(defn stop-dev-server []
  (http/stop @dev-server))

(defn restart-dev-server []
  (when @dev-server
    (stop-dev-server))
  (start-dev-server))

(restart-dev-server)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  nil)
