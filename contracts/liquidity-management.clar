;; liquidity-management.clar

;; Define constants for error codes
(define-constant ERR_INSUFFICIENT_FUNDS (err u100))
(define-constant ERR_UNAUTHORIZED (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))

;; Define the map for liquidity providers
(define-map liquidity-providers 
  principal 
  { 
    total-contribution: uint, 
    reward-tokens: uint 
  }
)

;; Define the total liquidity variable
(define-data-var total-liquidity uint u0)

;; Define the contract owner variable
(define-data-var contract-owner principal tx-sender)

;; Add liquidity with reward calculation
(define-public (add-liquidity-with-reward (amount uint))
  (let 
    (
      (sender tx-sender)
      (current-provider-info (default-to 
        { total-contribution: u0, reward-tokens: u0 }
        (map-get? liquidity-providers sender)
      ))
      (reward-amount (calculate-reward amount))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount sender (as-contract tx-sender)))
    
    ;; Update total liquidity
    (var-set total-liquidity (+ (var-get total-liquidity) amount))
    
    ;; Update liquidity provider info
    (map-set liquidity-providers 
      sender 
      {
        total-contribution: (+ (get total-contribution current-provider-info) amount),
        reward-tokens: (+ (get reward-tokens current-provider-info) reward-amount)
      }
    )
    
    ;; Mint reward tokens
    (try! (contract-call? .insurance-pool-token mint reward-amount sender))
    
    (ok true)
  )
)

;; Calculate reward for liquidity provision
(define-private (calculate-reward (amount uint))
  ;; Simplified reward calculation (1% of provided liquidity)
  (/ amount u100)
)

;; Withdraw liquidity with penalty for early withdrawal
(define-public (withdraw-liquidity-with-penalty (amount uint))
  (let 
    (
      (sender tx-sender)
      (provider-info (unwrap! 
        (map-get? liquidity-providers sender) 
        ERR_UNAUTHORIZED
      ))
      (penalty-amount (calculate-withdrawal-penalty amount))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    ;; Ensure sufficient liquidity and user contribution
    (asserts! (>= (var-get total-liquidity) amount) ERR_INSUFFICIENT_FUNDS)
    (asserts! (>= (get total-contribution provider-info) amount) ERR_INSUFFICIENT_FUNDS)
    
    ;; Transfer liquidity back to provider minus penalty
    (try! (as-contract (stx-transfer? (- amount penalty-amount) tx-sender sender)))
    
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

;; Calculate withdrawal penalty
(define-private (calculate-withdrawal-penalty (amount uint))
  ;; Simplified penalty calculation (2% of withdrawn amount)
  (/ amount u50)
)

;; Get liquidity provider detailed info
(define-read-only (get-liquidity-provider-detailed-info (provider principal))
  (match (map-get? liquidity-providers provider)
    provider-info (ok {
      total-contribution: (get total-contribution provider-info),
      reward-tokens: (get reward-tokens provider-info),
      share-of-pool: (if (is-eq (var-get total-liquidity) u0)
                        u0
                        (/ (* (get total-contribution provider-info) u10000) (var-get total-liquidity)))
    })
    (err ERR_UNAUTHORIZED)
  )
)