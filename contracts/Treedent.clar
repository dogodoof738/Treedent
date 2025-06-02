(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-verified (err u102))
(define-constant err-invalid-coordinates (err u103))

(define-data-var next-tree-id uint u1)

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