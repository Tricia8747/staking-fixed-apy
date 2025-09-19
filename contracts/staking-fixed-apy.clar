;; staking-fixed-apy
;; Fixed APY staking contract where users can stake STX tokens
;; Interest accrues per-block at a fixed APY (in basis points)
;; Contract must be pre-funded to pay rewards

(define-constant BPS u10000)
;; Approx blocks per year (adjust to your network/block-time)
;; Default ~10s blocktime -> 3,153,600 blocks/year
(define-constant BLOCKS_PER_YEAR u3153600)

;; Errors
(define-constant ERR_UNAUTHORIZED u100)
(define-constant ERR_ZERO_AMOUNT u101)
(define-constant ERR_INSUFFICIENT_BALANCE u102)
(define-constant ERR_NO_REWARDS_FUNDS u103)
(define-constant ERR_INVALID_PARAM u104)
(define-constant ERR_NO_FUNDS u105)

;; Admin
(define-data-var admin principal tx-sender)

;; APY in basis points (e.g., 500 = 5.00% APY)
(define-data-var apy-bps uint u500)

;; Storage per user:
;; - stake: how much STX the user has deposited (microSTX)
;; - accrued: rewards accumulated but not yet claimed (microSTX)
;; - last-update: block-height when we last updated accrual for the user
(define-map stakes
  { user: principal }
  { stake: uint, accrued: uint, last-update: uint })

(define-data-var total-staked uint u0)

;; Events will be printed as tuples

;; -------------------------
;; Read-only helpers
;; -------------------------
(define-read-only (is-admin (p principal)) (is-eq p (var-get admin)))

(define-read-only (get-apy-bps) (var-get apy-bps))

(define-read-only (get-stake (who principal))
  (match (map-get? stakes { user: who })
    some-stake (ok (get stake some-stake))
    (ok u0)))

(define-read-only (get-accrued (who principal))
  ;; returns accrued + pending since last-update (does not modify state)
  (match (map-get? stakes { user: who })
    some-stake
    (let ((current-block burn-block-height)
          (staked (get stake some-stake))
          (accrued (get accrued some-stake))
          (last (get last-update some-stake)))
      (if (or (is-eq staked u0) (<= current-block last))
          (ok accrued)
          (let ((delta (- current-block last))
                (apy (var-get apy-bps))
                (pending (/ (* staked apy delta) (* BPS BLOCKS_PER_YEAR))))
            (ok (+ accrued pending)))))
    (ok u0)))

(define-read-only (get-total-staked) (var-get total-staked))

;; -------------------------
;; Internal: accrue interest for a user (mutates state)
;; -------------------------
(define-private (accrue-for (who principal))
  (match (map-get? stakes { user: who })
    value
    (let ((current-block burn-block-height)
            (staked (get stake value))
            (accrued (get accrued value))
            (last (get last-update value)))
        (if (or (is-eq staked u0) (<= current-block last))
            ;; nothing to do: just update timestamp
            (begin
              (map-set stakes { user: who }
                      { stake: staked, accrued: accrued, last-update: current-block })
              (ok true))
            (let ((delta (- current-block last))
                  (apy (var-get apy-bps))
                  (pending (/ (* staked apy delta) (* BPS BLOCKS_PER_YEAR))))
              ;; update accrued rewards
              (map-set stakes { user: who }
                      { stake: staked, accrued: (+ accrued pending), last-update: current-block })
              (ok true))))
    (ok true)))

;; -------------------------
;; User flows
;; -------------------------

;; Deposit STX to stake (must transfer STX into contract)
(define-public (deposit (amount uint))
  (begin
    (asserts! (> amount u0) (err ERR_ZERO_AMOUNT))
    ;; transfer STX from user to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    ;; update accrual before changing stake
    (unwrap-panic (accrue-for tx-sender))
    (match (map-get? stakes { user: tx-sender })
      some-stake
      (let ((current-stake (get stake some-stake)))
        (let ((new-stake (+ current-stake amount)))
          (map-set stakes { user: tx-sender }
                  { stake: new-stake, 
                    accrued: (get accrued some-stake), 
                    last-update: burn-block-height })
          (var-set total-staked (+ (var-get total-staked) amount))
          (print { type: "deposit", user: tx-sender, amount: amount, stake-after: new-stake })
          (ok new-stake)))
      (begin
        (map-set stakes { user: tx-sender }
                { stake: amount, accrued: u0, last-update: burn-block-height })
        (var-set total-staked (+ (var-get total-staked) amount))
        (print { type: "deposit", user: tx-sender, amount: amount, stake-after: amount })
        (ok amount)))))

;; Withdraw principal (does NOT auto-claim rewards - it will tally accrued and leave them until claim)
;; For convenience this withdraw will update accrual and then transfer requested principal.
(define-public (withdraw (amount uint))
  (begin
    (asserts! (> amount u0) (err ERR_ZERO_AMOUNT))
    ;; update accrual
    (unwrap-panic (accrue-for tx-sender))
    (match (map-get? stakes { user: tx-sender })
      some-stake
      (let ((current-stake (get stake some-stake)))
        (asserts! (>= current-stake amount) (err ERR_INSUFFICIENT_BALANCE))
        (let ((new-stake (- current-stake amount)))
          (map-set stakes { user: tx-sender }
                  { stake: new-stake,
                    accrued: (get accrued some-stake),
                    last-update: burn-block-height })
          (var-set total-staked (- (var-get total-staked) amount))
          ;; transfer STX to user
          (asserts! (>= (stx-get-balance (as-contract tx-sender)) amount) (err ERR_NO_FUNDS))
          (try! (stx-transfer? amount (as-contract tx-sender) tx-sender))
          (print { type: "withdraw", user: tx-sender, amount: amount, stake-after: new-stake })
          (ok new-stake)))
      (err ERR_INSUFFICIENT_BALANCE))))

;; Claim rewards (transfer accrued rewards in STX)
(define-public (claim)
  (begin 
    (unwrap-panic (accrue-for tx-sender))
    (match (map-get? stakes { user: tx-sender })
      some-stake
      (let ((a (get accrued some-stake)))
        (asserts! (> a u0) (err ERR_ZERO_AMOUNT))
        (asserts! (>= (stx-get-balance (as-contract tx-sender)) a) (err ERR_NO_REWARDS_FUNDS))
        (map-set stakes { user: tx-sender }
                { stake: (get stake some-stake), accrued: u0, last-update: burn-block-height })
        (try! (stx-transfer? a (as-contract tx-sender) tx-sender))
        (print { type: "claim", user: tx-sender, amount: a })
        (ok a))
      (err ERR_NO_FUNDS))))

;; Exit: withdraw all stake and claim rewards (atomic)
(define-public (exit)
  (begin
    (unwrap-panic (accrue-for tx-sender))
    (match (map-get? stakes { user: tx-sender })
      some-stake
      (let ((stake (get stake some-stake)))
        (let ((accrued (get accrued some-stake)))
          ;; ensure contract has enough funds to send (principal + rewards)
          (asserts! (>= (stx-get-balance (as-contract tx-sender)) (+ stake accrued)) (err ERR_NO_FUNDS))
          ;; zero out
          (map-delete stakes { user: tx-sender })
          (var-set total-staked (- (var-get total-staked) stake))
          ;; transfer principal and rewards
          (try! (stx-transfer? stake (as-contract tx-sender) tx-sender))
          (if (> accrued u0)
            (try! (stx-transfer? accrued (as-contract tx-sender) tx-sender))
            true)
          (print { type: "exit", user: tx-sender, stake: stake, reward: accrued })
          (ok { stake: stake, reward: accrued })))
      (err ERR_NO_FUNDS))))

;; -------------------------
;; Admin flows
;; -------------------------

(define-public (set-apy (apy uint))
  (begin
    (asserts! (is-admin tx-sender) (err ERR_UNAUTHORIZED))
    (asserts! (<= apy u1000000) (err ERR_INVALID_PARAM)) ;; avoid absurd APYs, e.g., >10000%
    (var-set apy-bps apy)
    (print { type: "set-apy", apy-bps: apy })
    (ok true)))

;; Fund contract with STX so it can pay rewards
(define-public (fund-rewards (amount uint))
  (begin
    (asserts! (is-admin tx-sender) (err ERR_UNAUTHORIZED))
    (asserts! (> amount u0) (err ERR_ZERO_AMOUNT))
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (print { type: "fund", from: tx-sender, amount: amount })
    (ok true)))

;; Admin withdraw surplus STX (not used as staked funds should be tracked)
;; Be careful: admin must not withdraw funds needed for outstanding stakes + accrued rewards
;; Admin withdraw surplus STX (not used as staked funds should be tracked)
;; Be careful: admin must not withdraw funds needed for outstanding stakes + accrued rewards
(define-private (perform-withdraw (recipient principal) (amount uint))
  (begin
    (try! (stx-transfer? amount (as-contract tx-sender) recipient))
    (print { type: "admin-withdraw", to: recipient, amount: amount })
    (ok true)))

(define-public (admin-withdraw (to principal) (amount uint))
  (begin
    (asserts! (is-admin tx-sender) (err ERR_UNAUTHORIZED))
    (asserts! (> amount u0) (err ERR_ZERO_AMOUNT))
    ;; check contract has funds
    (asserts! (>= (stx-get-balance (as-contract tx-sender)) amount) (err ERR_NO_FUNDS))
    (as-contract (perform-withdraw to amount))))

;; -------------------------
;; Utility read-only view to inspect user record
;; -------------------------
(define-read-only (user-info (who principal))
  (match (map-get? stakes { user: who })
    some-stake
    { stake: (get stake some-stake), 
      accrued: (get accrued some-stake), 
      last-update: (get last-update some-stake) }
    { stake: u0, accrued: u0, last-update: u0 }))
    