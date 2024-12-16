(define-constant ERR_INVALID_RISK_TYPE (err u100))
(define-constant ERR_INVALID_COVERAGE (err u101))
(define-constant ERR_UNAUTHORIZED (err u102))

;; Risk types and their corresponding base premiums (per 1000 STX of coverage)
(define-map risk-base-premiums
  (string-ascii 50)
  uint
)

;; Initialize risk base premiums
(map-set risk-base-premiums "low" u5)    ;; 0.5%
(map-set risk-base-premiums "medium" u10) ;; 1%
(map-set risk-base-premiums "high" u20)   ;; 2%

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; Calculate premium based on coverage amount and risk type
(define-read-only (calculate-premium (coverage-amount uint) (risk-type (string-ascii 50)))
  (let
    (
      (base-premium (unwrap! (map-get? risk-base-premiums risk-type) ERR_INVALID_RISK_TYPE))
    )
    (ok (/ (* coverage-amount base-premium) u1000))
  )
)

;; Assess risk and determine risk type
(define-public (assess-risk (coverage-amount uint) (applicant-data (string-ascii 1000)))
  (begin
    (asserts! (> coverage-amount u0) ERR_INVALID_COVERAGE)
    (let
      (
        (risk-score (calculate-risk-score coverage-amount applicant-data))
      )
      (if (< risk-score u30)
        (ok "low")
        (if (< risk-score u70)
          (ok "medium")
          (ok "high")
        )
      )
    )
  )
)

;; Calculate risk score based on coverage amount and applicant data
(define-private (calculate-risk-score (coverage-amount uint) (applicant-data (string-ascii 1000)))
  (let
    (
      (coverage-factor (/ coverage-amount u1000000))  ;; Higher coverage increases risk
      (data-factor (len applicant-data))   ;; More data might indicate higher risk
    )
    (+ (* coverage-factor u10) data-factor)
  )
)

;; Update base premium for a risk type
(define-public (update-base-premium (risk-type (string-ascii 50)) (new-premium uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (ok (map-set risk-base-premiums risk-type new-premium))
  )
)

;; Get base premium for a risk type
(define-read-only (get-base-premium (risk-type (string-ascii 50)))
  (map-get? risk-base-premiums risk-type)
)

;; Set new contract owner
(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (ok (var-set contract-owner new-owner))
  )
)