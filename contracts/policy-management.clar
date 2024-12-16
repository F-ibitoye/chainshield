(use-trait token-trait .token-trait.token-trait)

;; Define constants for error codes
(define-constant ERR_POLICY_NOT_FOUND (err u100))
(define-constant ERR_POLICY_EXPIRED (err u101))
(define-constant ERR_POLICY_ALREADY_CLAIMED (err u102))
(define-constant ERR_UNAUTHORIZED (err u103))
(define-constant ERR_INVALID_AMOUNT (err u104))

;; Define the insurance-policies map
(define-map insurance-policies
  { policy-id: uint }
  {
    holder: principal,
    coverage-amount: uint,
    premium: uint,
    expiration: uint,
    is-claimed: bool
  }
)

;; Policy renewal
(define-public (renew-policy (policy-id uint) (new-duration uint))
  (let
    (
      (policy (unwrap! (map-get? insurance-policies { policy-id: policy-id }) ERR_POLICY_NOT_FOUND))
      (sender tx-sender)
    )
    (asserts! (is-eq sender (get holder policy)) ERR_UNAUTHORIZED)
    (asserts! (< block-height (get expiration policy)) ERR_POLICY_EXPIRED)
    
    ;; Calculate and charge renewal premium (simplified)
    (let
      (
        (renewal-premium (/ (get premium policy) u2))  ;; 50% of original premium for renewal
      )
      (try! (stx-transfer? renewal-premium sender (as-contract tx-sender)))
      
      ;; Update policy
      (map-set insurance-policies
        { policy-id: policy-id }
        (merge policy 
          { 
            expiration: (+ block-height new-duration),
            premium: (+ (get premium policy) renewal-premium)
          }
        )
      )
      
      (ok true)
    )
  )
)

;; Partial claim
(define-public (partial-claim (policy-id uint) (claim-amount uint))
  (let
    (
      (policy (unwrap! (map-get? insurance-policies { policy-id: policy-id }) ERR_POLICY_NOT_FOUND))
      (sender tx-sender)
    )
    (asserts! (is-eq sender (get holder policy)) ERR_UNAUTHORIZED)
    (asserts! (< block-height (get expiration policy)) ERR_POLICY_EXPIRED)
    (asserts! (not (get is-claimed policy)) ERR_POLICY_ALREADY_CLAIMED)
    (asserts! (<= claim-amount (get coverage-amount policy)) ERR_INVALID_AMOUNT)
    
    ;; Process partial claim
    (try! (as-contract (stx-transfer? claim-amount tx-sender sender)))
    
    ;; Update policy
    (map-set insurance-policies
      { policy-id: policy-id }
      (merge policy 
        { 
          coverage-amount: (- (get coverage-amount policy) claim-amount),
          is-claimed: (is-eq claim-amount (get coverage-amount policy))
        }
      )
    )
    
    (ok true)
  )
)

;; Get policy details
(define-read-only (get-policy-details (policy-id uint))
  (map-get? insurance-policies { policy-id: policy-id })
)