(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-verified (err u102))
(define-constant err-invalid-coordinates (err u103))
(define-constant err-insufficient-credits (err u104))
(define-constant err-invalid-price (err u105))
(define-constant err-order-not-found (err u106))
(define-constant err-cannot-buy-own-order (err u107))
(define-constant err-order-already-filled (err u108))
(define-constant err-not-order-owner (err u109))

(define-data-var next-tree-id uint u1)
(define-data-var next-order-id uint u1)

(define-map trees 
    uint 
    {
        planter: principal,
        latitude: int,
        longitude: int,
        species: (string-ascii 64),
        planted-at: uint,
        verified: bool,
        verifier: (optional principal)
    }
)

(define-map planter-stats
    principal
    {
        trees-planted: uint,
        trees-verified: uint,
        reputation-score: uint
    }
)

(define-map verifier-list
    principal
    bool
)

(define-map carbon-credits
    principal
    uint
)

(define-map sell-orders
    uint
    {
        seller: principal,
        credits-amount: uint,
        price-per-credit: uint,
        filled: bool,
        created-at: uint
    }
)

(define-map tree-credits-claimed
    uint
    bool
)

(define-read-only (get-tree-details (tree-id uint))
    (map-get? trees tree-id)
)

(define-read-only (get-planter-stats (planter principal))
    (default-to
        {
            trees-planted: u0,
            trees-verified: u0,
            reputation-score: u100
        }
        (map-get? planter-stats planter)
    )
)

(define-read-only (is-verifier (account principal))
    (default-to false (map-get? verifier-list account))
)

(define-read-only (get-carbon-credits (account principal))
    (default-to u0 (map-get? carbon-credits account))
)

(define-read-only (get-sell-order (order-id uint))
    (map-get? sell-orders order-id)
)

(define-read-only (is-tree-credits-claimed (tree-id uint))
    (default-to false (map-get? tree-credits-claimed tree-id))
)

(define-public (register-tree (latitude int) (longitude int) (species (string-ascii 64)))
    (let
        (
            (tree-id (var-get next-tree-id))
            (planter-data (get-planter-stats tx-sender))
        )
        (asserts! (and (> latitude (* -90 100000)) (< latitude (* 90 100000))) err-invalid-coordinates)
        (asserts! (and (> longitude (* -180 100000)) (< longitude (* 180 100000))) err-invalid-coordinates)
        
        (map-set trees tree-id {
            planter: tx-sender,
            latitude: latitude,
            longitude: longitude,
            species: species,
            planted-at: stacks-block-height,
            verified: false,
            verifier: none
        })
        
        (map-set planter-stats tx-sender {
            trees-planted: (+ (get trees-planted planter-data) u1),
            trees-verified: (get trees-verified planter-data),
            reputation-score: (get reputation-score planter-data)
        })
        
        (var-set next-tree-id (+ tree-id u1))
        (ok tree-id)
    )
)

(define-public (verify-tree (tree-id uint))
    (let
        (
            (tree-data (unwrap! (map-get? trees tree-id) err-not-found))
            (planter-data (get-planter-stats (get planter tree-data)))
        )
        (asserts! (is-verifier tx-sender) err-owner-only)
        (asserts! (not (get verified tree-data)) err-already-verified)
        
        (map-set trees tree-id (merge tree-data {
            verified: true,
            verifier: (some tx-sender)
        }))
        
        (map-set planter-stats (get planter tree-data) {
            trees-planted: (get trees-planted planter-data),
            trees-verified: (+ (get trees-verified planter-data) u1),
            reputation-score: (+ (get reputation-score planter-data) u10)
        })
        
        (ok true)
    )
)

(define-public (add-verifier (new-verifier principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set verifier-list new-verifier true)
        (ok true)
    )
)

(define-public (remove-verifier (verifier principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set verifier-list verifier false)
        (ok true)
    )
)

(define-read-only (get-all-trees-for-planter (planter principal))
    (let
        (
            (tree-id (- (var-get next-tree-id) u1))
        )
        (filter get-planter-trees (list tree-id))
    )
)

(define-private (get-planter-trees (tree-id uint))
    (match (map-get? trees tree-id)
        tree-data (is-eq (get planter tree-data) tx-sender)
        false
    )
)

(define-public (claim-tree-credits (tree-id uint))
    (let
        (
            (tree-data (unwrap! (map-get? trees tree-id) err-not-found))
            (current-credits (get-carbon-credits tx-sender))
            (credits-per-tree u10)
        )
        (asserts! (is-eq (get planter tree-data) tx-sender) err-owner-only)
        (asserts! (get verified tree-data) err-not-found)
        (asserts! (not (is-tree-credits-claimed tree-id)) err-already-verified)
        
        (map-set tree-credits-claimed tree-id true)
        (map-set carbon-credits tx-sender (+ current-credits credits-per-tree))
        
        (ok credits-per-tree)
    )
)

(define-public (create-sell-order (credits-amount uint) (price-per-credit uint))
    (let
        (
            (order-id (var-get next-order-id))
            (seller-credits (get-carbon-credits tx-sender))
        )
        (asserts! (> credits-amount u0) err-invalid-price)
        (asserts! (> price-per-credit u0) err-invalid-price)
        (asserts! (>= seller-credits credits-amount) err-insufficient-credits)
        
        (map-set carbon-credits tx-sender (- seller-credits credits-amount))
        
        (map-set sell-orders order-id {
            seller: tx-sender,
            credits-amount: credits-amount,
            price-per-credit: price-per-credit,
            filled: false,
            created-at: stacks-block-height
        })
        
        (var-set next-order-id (+ order-id u1))
        (ok order-id)
    )
)

(define-public (buy-credits (order-id uint))
    (let
        (
            (order-data (unwrap! (map-get? sell-orders order-id) err-order-not-found))
            (buyer-credits (get-carbon-credits tx-sender))
            (total-cost (* (get credits-amount order-data) (get price-per-credit order-data)))
        )
        (asserts! (not (is-eq tx-sender (get seller order-data))) err-cannot-buy-own-order)
        (asserts! (not (get filled order-data)) err-order-already-filled)
        
        (try! (stx-transfer? total-cost tx-sender (get seller order-data)))
        
        (map-set carbon-credits tx-sender (+ buyer-credits (get credits-amount order-data)))
        
        (map-set sell-orders order-id (merge order-data {
            filled: true
        }))
        
        (ok (get credits-amount order-data))
    )
)

(define-public (cancel-sell-order (order-id uint))
    (let
        (
            (order-data (unwrap! (map-get? sell-orders order-id) err-order-not-found))
            (seller-credits (get-carbon-credits tx-sender))
        )
        (asserts! (is-eq tx-sender (get seller order-data)) err-not-order-owner)
        (asserts! (not (get filled order-data)) err-order-already-filled)
        
        (map-set carbon-credits tx-sender (+ seller-credits (get credits-amount order-data)))
        
        (map-set sell-orders order-id (merge order-data {
            filled: true
        }))
        
        (ok (get credits-amount order-data))
    )
)

(define-public (burn-carbon-credits (credits-amount uint))
    (let
        (
            (current-credits (get-carbon-credits tx-sender))
        )
        (asserts! (>= current-credits credits-amount) err-insufficient-credits)
        (asserts! (> credits-amount u0) err-invalid-price)
        
        (map-set carbon-credits tx-sender (- current-credits credits-amount))
        
        (ok credits-amount)
    )
)

(define-public (transfer-credits (recipient principal) (credits-amount uint))
    (let
        (
            (sender-credits (get-carbon-credits tx-sender))
            (recipient-credits (get-carbon-credits recipient))
        )
        (asserts! (>= sender-credits credits-amount) err-insufficient-credits)
        (asserts! (> credits-amount u0) err-invalid-price)
        
        (map-set carbon-credits tx-sender (- sender-credits credits-amount))
        (map-set carbon-credits recipient (+ recipient-credits credits-amount))
        
        (ok credits-amount)
    )
)