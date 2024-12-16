(use-trait token-trait .token-trait.token-trait)

(define-constant ERR_INSUFFICIENT_FUNDS (err u100))
(define-constant ERR_NOT_OWNER (err u101))
(define-constant ERR_POLICY_NOT_FOUND (err u102))
(define-constant ERR_INVALID_COVERAGE (err u103))
(define-constant ERR_UNAUTHORIZED (err u104))
(define-constant ERR_INVALID_PREMIUM (err u105))
(define-constant ERR_POLICY_EXPIRED (err u106))
(define-constant ERR_POLICY_ALREADY_CLAIMED (err u107))

;; Insurance pool storage
(define-map insurance-policies 
  { policy-id: uint }
  {
    holder: principal,
    coverage-amount: uint,
    premium: uint,
    expiration: uint,
    is-claimed: bool,
    risk-type: (string-ascii 50)
  }
)

;; Liquidity providers map
(define-map liquidity-providers 
  principal 
  {
    total-contribution: uint,
    reward-tokens: uint
  }
)

;; Total liquidity in the pool
(define-data-var total-liquidity uint u0)
(define-data-var total-policies uint u0)

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; Create a new insurance policy
(define-public (create-policy 
  (coverage-amount uint) 
  (premium uint) 
  (duration uint)
  (risk-type (string-ascii 50))
)
  (let 
    (
      (policy-id (+ (var-get total-policies) u1))
      (sender tx-sender)
    )
    (asserts! (> coverage-amount u0) ERR_INVALID_COVERAGE)
    (asserts! (> premium u0) ERR_INVALID_PREMIUM)
    (try! (stx-transfer? premium sender (as-contract tx-sender)))
    
    (map-set insurance-policies 
      { policy-id: policy-id }
      {
        holder: sender,
        coverage-amount: coverage-amount,
        premium: premium,
        expiration: (+ block-height duration),
        is-claimed: false,
        risk-type: risk-type
      }
    )
    
    (var-set total-policies policy-id)
    (ok policy-id)
  )
)

;; Claim insurance if conditions are met
(define-public (claim-policy (policy-id uint))
  (let 
    (
      (policy (unwrap! (map-get? insurance-policies { policy-id: policy-id }) ERR_POLICY_NOT_FOUND))
      (sender tx-sender)
    )
    (asserts! (is-eq sender (get holder policy)) ERR_NOT_OWNER)
    (asserts! (< block-height (get expiration policy)) ERR_POLICY_EXPIRED)
    (asserts! (not (get is-claimed policy)) ERR_POLICY_ALREADY_CLAIMED)
    
    (try! (as-contract (stx-transfer? (get coverage-amount policy) tx-sender sender)))
    
    (map-set insurance-policies 
      { policy-id: policy-id }
      (merge policy { is-claimed: true })
    )
    
    (ok true)
  )
)

;; Add liquidity to the pool
(define-public (add-liquidity (amount uint))
  (let 
    (
      (sender tx-sender)
      (current-provider-info (default-to 
        { total-contribution: u0, reward-tokens: u0 }
        (map-get? liquidity-providers sender)
      ))
    )
    (asserts! (> amount u0) ERR_INVALID_PREMIUM)
    (try! (stx-transfer? amount sender (as-contract tx-sender)))
    
    ;; Update total liquidity
    (var-set total-liquidity (+ (var-get total-liquidity) amount))
    
    ;; Update liquidity provider info
    (map-set liquidity-providers 
      sender 
      {
        total-contribution: (+ (get total-contribution current-provider-info) amount),
        reward-tokens: (get reward-tokens current-provider-info)
      }
    )
    
    (ok true)
  )
)

;; Withdraw liquidity from the pool
(define-public (withdraw-liquidity (amount uint))
  (let 
    (
      (sender tx-sender)
      (provider-info (unwrap! 
        (map-get? liquidity-providers sender) 
        ERR_UNAUTHORIZED
      ))
    )
    ;; Ensure sufficient liquidity and user contribution
    (asserts! (> amount u0) ERR_INVALID_PREMIUM)
    (asserts! (>= (var-get total-liquidity) amount) ERR_INSUFFICIENT_FUNDS)
    (asserts! (>= (get total-contribution provider-info) amount) ERR_INSUFFICIENT_FUNDS)
    
    ;; Transfer liquidity back to provider
    (try! (as-contract (stx-transfer? amount tx-sender sender)))
    
    ;; Update liquidity tracking
    (var-set total-liquidity (- (var-get total-liquidity) amount))
    (map-set liquidity-providers 
      sender 
      {
        total-contribution: (- (get total-contribution provider-info) amount),
        reward-tokens: (get reward-tokens provider-info)
      }
    )
    
    (ok true)
  )
)

;; Get total liquidity
(define-read-only (get-total-liquidity)
  (ok (var-get total-liquidity))
)

;; Get liquidity provider info
(define-read-only (get-liquidity-provider-info (provider principal))
  (ok (map-get? liquidity-providers provider))
)

;; Get policy details
(define-read-only (get-policy-details (policy-id uint))
  (ok (map-get? insurance-policies { policy-id: policy-id }))
)

;; Set contract owner
(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_OWNER)
    (var-set contract-owner new-owner)
    (ok true)
  )
)

