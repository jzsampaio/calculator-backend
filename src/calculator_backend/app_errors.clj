(ns calculator-backend.app-errors)

;; All erros that can be thrown by this service are listed below

(def app-errors '(::not-found ;; when the request is well formed, but data was not located on the storage
                  ::request-body-error ;; when the request is ill formed
                  ::internal-error ;; for unexpected and non recoverable scenarios
                  ))

(defn new-error
  "Creates instance of error to be used with all occurrences of `either/left` on this project.

  The value err is expected to be one of the keywords defined inside
  `app-errors`."
  ([err] (new-error err nil))
  ([err details] {:error err :details details}))
