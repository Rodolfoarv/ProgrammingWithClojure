(defstruct account :id :tag :balance)
(def test-savings (struct account 1 ::Savings 100M))
(def test-checking (struct account 2 ::Checking 250M))

(defmulti interest-rate :tag)
(defmethod interest-rate ::Checking [_] 0M)
(defmethod interest-rate ::Savings [_] 0.05M)

(defmulti account-level :tag)
(defmethod account-level ::Checking [acct]
  (if (>= (:balance acct) 5000) ::Premium ::Basic))
(defmethod account-level ::Savings [acct]
  (if (>= (:balance acct) 1000) ::Premium ::Basic))

; (defmulti service-charge (fn [acct] [(account-level acct) (:tag acct)]))
; (defmethod service-charge [::Basic ::Checking] [_] 25)
; (defmethod service-charge [::Basic ::Savings] [_] 10)
; (defmethod service-charge [::Premium ::Checking] [_] 0)
; (defmethod service-charge [::Premium ::Savings] [_] 0)

(derive ::Savings ::Account)
(derive ::Checking ::Account)

(defmulti service-charge (fn [acct] [(account-level acct) (:tag acct)]))
(defmethod service-charge [::Basic ::Checking] [_] 25)
(defmethod service-charge [::Basic ::Savings] [_] 10)
(defmethod service-charge [::Premium ::Account] [_] 0)
