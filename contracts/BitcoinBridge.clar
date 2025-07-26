;; BitcoinBridge - Cross-Chain Asset Bridge using HTLCs
;; Enables trustless atomic swaps between Stacks and other Bitcoin L2s

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INVALID_HASH (err u102))
(define-constant ERR_INVALID_TIMELOCK (err u103))
(define-constant ERR_HTLC_NOT_FOUND (err u104))
(define-constant ERR_HTLC_EXPIRED (err u105))
(define-constant ERR_HTLC_NOT_EXPIRED (err u106))
(define-constant ERR_INVALID_PREIMAGE (err u107))
(define-constant ERR_HTLC_ALREADY_CLAIMED (err u108))
(define-constant ERR_HTLC_ALREADY_REFUNDED (err u109))
(define-constant ERR_INSUFFICIENT_FUNDS (err u110))
(define-constant ERR_INVALID_RECIPIENT (err u111))
(define-constant ERR_ZERO_AMOUNT (err u112))

;; Minimum timelock duration (1 hour in blocks, ~10 blocks per hour on Stacks)
(define-constant MIN_TIMELOCK_DURATION u10)

;; HTLC Status
(define-constant STATUS_ACTIVE u1)
(define-constant STATUS_CLAIMED u2)
(define-constant STATUS_REFUNDED u3)

;; Data Variables
(define-data-var contract-active bool true)
(define-data-var htlc-counter uint u0)

;; Data Maps
(define-map htlcs
  { htlc-id: uint }
  {
    sender: principal,
    recipient: principal,
    amount: uint,
    hash-lock: (buff 32),
    timelock: uint,
    status: uint,
    created-at: uint
  }
)

(define-map claimed-htlcs
  { htlc-id: uint }
  {
    claimer: principal,
    preimage: (buff 32),
    claimed-at: uint
  }
)

;; Private Functions
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT_OWNER)
)

(define-private (validate-hash-lock (hash-lock (buff 32)))
  (> (len hash-lock) u0)
)

(define-private (validate-timelock (timelock uint))
  (and 
    (> timelock stacks-block-height)
    (>= (- timelock stacks-block-height) MIN_TIMELOCK_DURATION)
  )
)

(define-private (validate-amount (amount uint))
  (> amount u0)
)

(define-private (validate-principal (address principal))
  (not (is-eq address tx-sender))
)

(define-private (validate-htlc-id (htlc-id uint))
  (and 
    (> htlc-id u0)
    (<= htlc-id (var-get htlc-counter))
  )
)

(define-private (increment-htlc-counter)
  (let ((current-counter (var-get htlc-counter)))
    (var-set htlc-counter (+ current-counter u1))
    (+ current-counter u1)
  )
)

(define-private (verify-preimage (preimage (buff 32)) (hash-lock (buff 32)))
  (is-eq (sha256 preimage) hash-lock)
)

;; Public Functions

;; Create a new HTLC
(define-public (create-htlc 
  (recipient principal)
  (amount uint)
  (hash-lock (buff 32))
  (timelock uint)
)
  (let (
    (htlc-id (increment-htlc-counter))
    (current-block stacks-block-height)
  )
    (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
    (asserts! (validate-amount amount) ERR_ZERO_AMOUNT)
    (asserts! (validate-principal recipient) ERR_INVALID_RECIPIENT)
    (asserts! (validate-hash-lock hash-lock) ERR_INVALID_HASH)
    (asserts! (validate-timelock timelock) ERR_INVALID_TIMELOCK)
    (asserts! (>= (stx-get-balance tx-sender) amount) ERR_INSUFFICIENT_FUNDS)

    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))

    (map-set htlcs
      { htlc-id: htlc-id }
      {
        sender: tx-sender,
        recipient: recipient,
        amount: amount,
        hash-lock: hash-lock,
        timelock: timelock,
        status: STATUS_ACTIVE,
        created-at: current-block
      }
    )

    (print {
      event: "htlc-created",
      htlc-id: htlc-id,
      sender: tx-sender,
      recipient: recipient,
      amount: amount,
      timelock: timelock
    })

    (ok htlc-id)
  )
)

;; Claim HTLC with preimage
(define-public (claim-htlc (htlc-id uint) (preimage (buff 32)))
  (let (
    (htlc-data (unwrap! (map-get? htlcs { htlc-id: htlc-id }) ERR_HTLC_NOT_FOUND))
    (current-block stacks-block-height)
  )
    (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
    (asserts! (validate-htlc-id htlc-id) ERR_HTLC_NOT_FOUND)
    (asserts! (> (len preimage) u0) ERR_INVALID_PREIMAGE)
    (asserts! (is-eq (get status htlc-data) STATUS_ACTIVE) ERR_HTLC_ALREADY_CLAIMED)
    (asserts! (< current-block (get timelock htlc-data)) ERR_HTLC_EXPIRED)
    (asserts! (verify-preimage preimage (get hash-lock htlc-data)) ERR_INVALID_PREIMAGE)
    (asserts! (is-eq tx-sender (get recipient htlc-data)) ERR_UNAUTHORIZED)

    (map-set htlcs
      { htlc-id: htlc-id }
      (merge htlc-data { status: STATUS_CLAIMED })
    )

    (map-set claimed-htlcs
      { htlc-id: htlc-id }
      {
        claimer: tx-sender,
        preimage: preimage,
        claimed-at: current-block
      }
    )

    (try! (as-contract (stx-transfer? (get amount htlc-data) tx-sender (get recipient htlc-data))))

    (print {
      event: "htlc-claimed",
      htlc-id: htlc-id,
      claimer: tx-sender,
      amount: (get amount htlc-data)
    })

    (ok true)
  )
)

;; Refund expired HTLC
(define-public (refund-htlc (htlc-id uint))
  (let (
    (htlc-data (unwrap! (map-get? htlcs { htlc-id: htlc-id }) ERR_HTLC_NOT_FOUND))
    (current-block stacks-block-height)
  )
    (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
    (asserts! (validate-htlc-id htlc-id) ERR_HTLC_NOT_FOUND)
    (asserts! (is-eq (get status htlc-data) STATUS_ACTIVE) ERR_HTLC_ALREADY_REFUNDED)
    (asserts! (>= current-block (get timelock htlc-data)) ERR_HTLC_NOT_EXPIRED)
    (asserts! (is-eq tx-sender (get sender htlc-data)) ERR_UNAUTHORIZED)

    (map-set htlcs
      { htlc-id: htlc-id }
      (merge htlc-data { status: STATUS_REFUNDED })
    )

    (try! (as-contract (stx-transfer? (get amount htlc-data) tx-sender (get sender htlc-data))))

    (print {
      event: "htlc-refunded",
      htlc-id: htlc-id,
      sender: tx-sender,
      amount: (get amount htlc-data)
    })

    (ok true)
  )
)

;; Read-only Functions

;; Get HTLC details
(define-read-only (get-htlc (htlc-id uint))
  (begin
    (asserts! (validate-htlc-id htlc-id) none)
    (map-get? htlcs { htlc-id: htlc-id })
  )
)

;; Get claim details
(define-read-only (get-claim-details (htlc-id uint))
  (begin
    (asserts! (validate-htlc-id htlc-id) none)
    (map-get? claimed-htlcs { htlc-id: htlc-id })
  )
)

;; Get current HTLC counter
(define-read-only (get-htlc-counter)
  (var-get htlc-counter)
)

;; Check if HTLC is expired
(define-read-only (is-htlc-expired (htlc-id uint))
  (begin
    (asserts! (validate-htlc-id htlc-id) false)
    (match (map-get? htlcs { htlc-id: htlc-id })
      htlc-data (>= stacks-block-height (get timelock htlc-data))
      false
    )
  )
)

;; Check if HTLC is claimable
(define-read-only (is-htlc-claimable (htlc-id uint))
  (begin
    (asserts! (validate-htlc-id htlc-id) false)
    (match (map-get? htlcs { htlc-id: htlc-id })
      htlc-data (and
        (is-eq (get status htlc-data) STATUS_ACTIVE)
        (< stacks-block-height (get timelock htlc-data))
      )
      false
    )
  )
)

;; Check if HTLC is refundable
(define-read-only (is-htlc-refundable (htlc-id uint))
  (begin
    (asserts! (validate-htlc-id htlc-id) false)
    (match (map-get? htlcs { htlc-id: htlc-id })
      htlc-data (and
        (is-eq (get status htlc-data) STATUS_ACTIVE)
        (>= stacks-block-height (get timelock htlc-data))
      )
      false
    )
  )
)

;; Get contract status
(define-read-only (get-contract-status)
  (var-get contract-active)
)

;; Administrative Functions

;; Emergency pause contract (owner only)
(define-public (pause-contract)
  (begin
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    (var-set contract-active false)
    (print { event: "contract-paused", caller: tx-sender })
    (ok true)
  )
)

;; Resume contract (owner only)
(define-public (resume-contract)
  (begin
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    (var-set contract-active true)
    (print { event: "contract-resumed", caller: tx-sender })
    (ok true)
  )
)