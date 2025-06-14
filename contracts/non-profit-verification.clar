;; Non-Profit Verification Contract
;; Validates and manages non-profit organizations

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-input (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-status (err u105))

;; Data Variables
(define-data-var next-nonprofit-id uint u1)
(define-data-var total-nonprofits uint u0)
(define-data-var total-verified uint u0)

;; Data Maps
(define-map nonprofits
  { id: uint }
  {
    name: (string-ascii 100),
    registration-number: (string-ascii 50),
    verification-status: (string-ascii 20),
    verification-date: uint,
    contact-info: (string-ascii 200),
    mission-statement: (string-ascii 500),
    focus-areas: (list 10 (string-ascii 50)),
    verified-by: principal,
    created-at: uint,
    updated-at: uint
  }
)

(define-map nonprofit-by-registration
  { registration-number: (string-ascii 50) }
  { id: uint }
)

(define-map verifiers
  { verifier: principal }
  { authorized: bool, added-at: uint }
)

(define-map nonprofit-metrics
  { nonprofit-id: uint }
  {
    total-programs: uint,
    total-donations: uint,
    impact-score: uint,
    last-audit-date: uint
  }
)

;; Authorization functions
(define-private (is-contract-owner)
  (is-eq tx-sender contract-owner)
)

(define-private (is-authorized-verifier)
  (default-to false
    (get authorized (map-get? verifiers { verifier: tx-sender }))
  )
)

;; Validation functions
(define-private (validate-nonprofit-name (name (string-ascii 100)))
  (and
    (> (len name) u0)
    (<= (len name) u100)
  )
)

(define-private (validate-registration-number (reg-num (string-ascii 50)))
  (and
    (> (len reg-num) u0)
    (<= (len reg-num) u50)
  )
)

(define-private (validate-status (status (string-ascii 20)))
  (or
    (is-eq status "pending")
    (is-eq status "verified")
    (is-eq status "rejected")
    (is-eq status "suspended")
  )
)

;; Public functions

;; Register a new non-profit organization
(define-public (register-nonprofit
  (name (string-ascii 100))
  (registration-number (string-ascii 50))
  (contact-info (string-ascii 200))
  (mission-statement (string-ascii 500))
  (focus-areas (list 10 (string-ascii 50)))
  )
  (let (
    (nonprofit-id (var-get next-nonprofit-id))
    (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
  )
    ;; Validate inputs
    (asserts! (validate-nonprofit-name name) err-invalid-input)
    (asserts! (validate-registration-number registration-number) err-invalid-input)
    (asserts! (> (len contact-info) u0) err-invalid-input)
    (asserts! (> (len mission-statement) u0) err-invalid-input)

    ;; Check if registration number already exists
    (asserts! (is-none (map-get? nonprofit-by-registration { registration-number: registration-number })) err-already-exists)

    ;; Create nonprofit record
    (map-set nonprofits
      { id: nonprofit-id }
      {
        name: name,
        registration-number: registration-number,
        verification-status: "pending",
        verification-date: u0,
        contact-info: contact-info,
        mission-statement: mission-statement,
        focus-areas: focus-areas,
        verified-by: tx-sender,
        created-at: current-time,
        updated-at: current-time
      }
    )

    ;; Create registration number mapping
    (map-set nonprofit-by-registration
      { registration-number: registration-number }
      { id: nonprofit-id }
    )

    ;; Initialize metrics
    (map-set nonprofit-metrics
      { nonprofit-id: nonprofit-id }
      {
        total-programs: u0,
        total-donations: u0,
        impact-score: u0,
        last-audit-date: u0
      }
    )

    ;; Update counters
    (var-set next-nonprofit-id (+ nonprofit-id u1))
    (var-set total-nonprofits (+ (var-get total-nonprofits) u1))

    ;; Emit event
    (print {
      event: "nonprofit-registered",
      nonprofit-id: nonprofit-id,
      name: name,
      registration-number: registration-number,
      created-at: current-time
    })

    (ok nonprofit-id)
  )
)

;; Verify a non-profit organization
(define-public (verify-nonprofit (nonprofit-id uint) (status (string-ascii 20)))
  (let (
    (nonprofit (unwrap! (map-get? nonprofits { id: nonprofit-id }) err-not-found))
    (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
  )
    ;; Check authorization
    (asserts! (or (is-contract-owner) (is-authorized-verifier)) err-unauthorized)

    ;; Validate status
    (asserts! (validate-status status) err-invalid-status)

    ;; Update nonprofit record
    (map-set nonprofits
      { id: nonprofit-id }
      (merge nonprofit {
        verification-status: status,
        verification-date: current-time,
        verified-by: tx-sender,
        updated-at: current-time
      })
    )

    ;; Update verified counter
    (if (is-eq status "verified")
      (var-set total-verified (+ (var-get total-verified) u1))
      (if (and (is-eq (get verification-status nonprofit) "verified") (not (is-eq status "verified")))
        (var-set total-verified (- (var-get total-verified) u1))
        false
      )
    )

    ;; Emit event
    (print {
      event: "nonprofit-verification-updated",
      nonprofit-id: nonprofit-id,
      status: status,
      verified-by: tx-sender,
      verification-date: current-time
    })

    (ok true)
  )
)

;; Add authorized verifier
(define-public (add-verifier (verifier principal))
  (begin
    (asserts! (is-contract-owner) err-owner-only)

    (map-set verifiers
      { verifier: verifier }
      {
        authorized: true,
        added-at: (unwrap-panic (get-block-info? time (- block-height u1)))
      }
    )

    (print {
      event: "verifier-added",
      verifier: verifier,
      added-by: tx-sender
    })

    (ok true)
  )
)

;; Remove authorized verifier
(define-public (remove-verifier (verifier principal))
  (begin
    (asserts! (is-contract-owner) err-owner-only)

    (map-delete verifiers { verifier: verifier })

    (print {
      event: "verifier-removed",
      verifier: verifier,
      removed-by: tx-sender
    })

    (ok true)
  )
)

;; Update nonprofit metrics
(define-public (update-nonprofit-metrics
  (nonprofit-id uint)
  (programs uint)
  (donations uint)
  (impact-score uint)
  )
  (let (
    (nonprofit (unwrap! (map-get? nonprofits { id: nonprofit-id }) err-not-found))
    (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
  )
    ;; Check if nonprofit is verified
    (asserts! (is-eq (get verification-status nonprofit) "verified") err-unauthorized)

    ;; Update metrics
    (map-set nonprofit-metrics
      { nonprofit-id: nonprofit-id }
      {
        total-programs: programs,
        total-donations: donations,
        impact-score: impact-score,
        last-audit-date: current-time
      }
    )

    (print {
      event: "nonprofit-metrics-updated",
      nonprofit-id: nonprofit-id,
      programs: programs,
      donations: donations,
      impact-score: impact-score
    })

    (ok true)
  )
)

;; Read-only functions

;; Get nonprofit information
(define-read-only (get-nonprofit-info (nonprofit-id uint))
  (map-get? nonprofits { id: nonprofit-id })
)

;; Get nonprofit by registration number
(define-read-only (get-nonprofit-by-registration (registration-number (string-ascii 50)))
  (match (map-get? nonprofit-by-registration { registration-number: registration-number })
    id-record (map-get? nonprofits { id: (get id id-record) })
    none
  )
)

;; Get nonprofit metrics
(define-read-only (get-nonprofit-metrics (nonprofit-id uint))
  (map-get? nonprofit-metrics { nonprofit-id: nonprofit-id })
)

;; Check if verifier is authorized
(define-read-only (is-verifier-authorized (verifier principal))
  (default-to false
    (get authorized (map-get? verifiers { verifier: verifier }))
  )
)

;; Get contract stats
(define-read-only (get-contract-stats)
  {
    total-nonprofits: (var-get total-nonprofits),
    total-verified: (var-get total-verified),
    next-id: (var-get next-nonprofit-id)
  }
)

;; Check if nonprofit is verified
(define-read-only (is-nonprofit-verified (nonprofit-id uint))
  (match (map-get? nonprofits { id: nonprofit-id })
    nonprofit (is-eq (get verification-status nonprofit) "verified")
    false
  )
)

;; Get verified nonprofits count
(define-read-only (get-verified-count)
  (var-get total-verified)
)

;; Get total nonprofits count
(define-read-only (get-total-count)
  (var-get total-nonprofits)
)
