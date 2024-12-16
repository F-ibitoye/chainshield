;; Implement the token trait from the separate contract
(impl-trait .token-trait.token-trait)

;; Define the token
(define-fungible-token insurance-pool-token)

;; Token information
(define-constant token-name "Insurance Pool Token")
(define-constant token-symbol "IPT")
(define-constant token-decimals u6)

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; Error constants
(define-constant ERR_UNAUTHORIZED (err u1))
(define-constant ERR_INVALID_AMOUNT (err u2))

;; Mint new tokens
(define-public (mint (amount uint) (recipient principal))
  (begin
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (ft-mint? insurance-pool-token amount recipient)
  )
)

;; Transfer tokens
(define-public (transfer (amount uint) (sender principal) (recipient principal))
  (begin
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (is-eq tx-sender sender) ERR_UNAUTHORIZED)
    (ft-transfer? insurance-pool-token amount sender recipient)
  )
)

;; Get token name
(define-read-only (get-name)
  (ok token-name)
)

;; Get token symbol
(define-read-only (get-symbol)
  (ok token-symbol)
)

;; Get token decimals
(define-read-only (get-decimals)
  (ok token-decimals)
)

;; Get token balance
(define-read-only (get-balance (account principal))
  (ok (ft-get-balance insurance-pool-token account))
)

;; Get total token supply
(define-read-only (get-total-supply)
  (ok (ft-get-supply insurance-pool-token))
)

;; Reward liquidity provider
(define-public (reward-liquidity-provider (amount uint) (provider principal))
  (begin
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (ft-mint? insurance-pool-token amount provider)
  )
)

;; Set contract owner
(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)

