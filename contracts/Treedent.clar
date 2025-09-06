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
(define-constant err-tree-already-adopted (err u110))
(define-constant err-not-sponsor (err u111))
(define-constant err-insufficient-payment (err u112))
(define-constant err-milestone-already-claimed (err u113))
(define-constant err-tree-not-adopted (err u114))
(define-constant err-invalid-milestone (err u115))
(define-constant err-not-authorized-monitor (err u116))
(define-constant err-invalid-health-status (err u117))
(define-constant err-monitoring-too-frequent (err u118))

(define-data-var next-tree-id uint u1)
(define-data-var next-order-id uint u1)
(define-data-var next-adoption-id uint u1)
(define-data-var next-monitoring-id uint u1)

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

(define-map tree-adoptions
    uint
    {
        sponsor: principal,
        tree-id: uint,
        total-payment: uint,
        milestone-1-claimed: bool,
        milestone-2-claimed: bool,
        milestone-3-claimed: bool,
        adoption-date: uint,
        active: bool
    }
)

(define-map sponsor-stats
    principal
    {
        trees-sponsored: uint,
        total-invested: uint,
        active-sponsorships: uint
    }
)

(define-map tree-to-adoption
    uint
    uint
)

;; Tree monitoring maps
(define-map tree-monitoring-entries
    uint
    {
        tree-id: uint,
        reporter: principal,
        health-status: uint,
        height-cm: uint,
        diameter-mm: uint,
        notes: (string-ascii 128),
        timestamp: uint,
        entry-id: uint
    }
)

(define-map tree-monitoring-count
    uint
    uint
)

(define-map tree-latest-monitoring
    uint
    {
        last-entry-id: uint,
        last-update: uint,
        current-health: uint
    }
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

(define-read-only (get-tree-adoption (adoption-id uint))
    (map-get? tree-adoptions adoption-id)
)

(define-read-only (get-sponsor-stats (sponsor principal))
    (default-to
        {
            trees-sponsored: u0,
            total-invested: u0,
            active-sponsorships: u0
        }
        (map-get? sponsor-stats sponsor)
    )
)

(define-read-only (get-adoption-by-tree (tree-id uint))
    (match (map-get? tree-to-adoption tree-id)
        adoption-id (map-get? tree-adoptions adoption-id)
        none
    )
)

(define-read-only (is-tree-adopted (tree-id uint))
    (is-some (map-get? tree-to-adoption tree-id))
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

(define-public (adopt-tree (tree-id uint) (payment uint))
    (let
        (
            (tree-data (unwrap! (map-get? trees tree-id) err-not-found))
            (adoption-id (var-get next-adoption-id))
            (sponsor-data (get-sponsor-stats tx-sender))
            (minimum-payment u1000000)
        )
        (asserts! (get verified tree-data) err-not-found)
        (asserts! (not (is-tree-adopted tree-id)) err-tree-already-adopted)
        (asserts! (>= payment minimum-payment) err-insufficient-payment)
        
        (try! (stx-transfer? payment tx-sender (as-contract tx-sender)))
        
        (map-set tree-adoptions adoption-id {
            sponsor: tx-sender,
            tree-id: tree-id,
            total-payment: payment,
            milestone-1-claimed: false,
            milestone-2-claimed: false,
            milestone-3-claimed: false,
            adoption-date: stacks-block-height,
            active: true
        })
        
        (map-set tree-to-adoption tree-id adoption-id)
        
        (map-set sponsor-stats tx-sender {
            trees-sponsored: (+ (get trees-sponsored sponsor-data) u1),
            total-invested: (+ (get total-invested sponsor-data) payment),
            active-sponsorships: (+ (get active-sponsorships sponsor-data) u1)
        })
        
        (var-set next-adoption-id (+ adoption-id u1))
        (ok adoption-id)
    )
)

(define-public (claim-milestone-payment (tree-id uint) (milestone uint))
    (let
        (
            (tree-data (unwrap! (map-get? trees tree-id) err-not-found))
            (adoption-id (unwrap! (map-get? tree-to-adoption tree-id) err-tree-not-adopted))
            (adoption-data (unwrap! (map-get? tree-adoptions adoption-id) err-tree-not-adopted))
            (planter (get planter tree-data))
            (payment-amount (/ (get total-payment adoption-data) u3))
            (blocks-since-adoption (- stacks-block-height (get adoption-date adoption-data)))
        )
        (asserts! (is-eq tx-sender planter) err-not-sponsor)
        (asserts! (get active adoption-data) err-tree-not-adopted)
        (asserts! (and (>= milestone u1) (<= milestone u3)) err-invalid-milestone)
        
        (if (is-eq milestone u1)
            (begin
                (asserts! (not (get milestone-1-claimed adoption-data)) err-milestone-already-claimed)
                (asserts! (>= blocks-since-adoption u144) err-insufficient-payment)
                (map-set tree-adoptions adoption-id (merge adoption-data {
                    milestone-1-claimed: true
                }))
            )
            (if (is-eq milestone u2)
                (begin
                    (asserts! (not (get milestone-2-claimed adoption-data)) err-milestone-already-claimed)
                    (asserts! (>= blocks-since-adoption u1008) err-insufficient-payment)
                    (map-set tree-adoptions adoption-id (merge adoption-data {
                        milestone-2-claimed: true
                    }))
                )
                (begin
                    (asserts! (not (get milestone-3-claimed adoption-data)) err-milestone-already-claimed)
                    (asserts! (>= blocks-since-adoption u5040) err-insufficient-payment)
                    (map-set tree-adoptions adoption-id (merge adoption-data {
                        milestone-3-claimed: true
                    }))
                )
            )
        )
        
        (try! (as-contract (stx-transfer? payment-amount tx-sender planter)))
        (ok payment-amount)
    )
)

(define-public (terminate-adoption (tree-id uint))
    (let
        (
            (adoption-id (unwrap! (map-get? tree-to-adoption tree-id) err-tree-not-adopted))
            (adoption-data (unwrap! (map-get? tree-adoptions adoption-id) err-tree-not-adopted))
            (sponsor-data (get-sponsor-stats tx-sender))
            (remaining-payment (calculate-remaining-payment adoption-data))
        )
        (asserts! (is-eq tx-sender (get sponsor adoption-data)) err-not-sponsor)
        (asserts! (get active adoption-data) err-tree-not-adopted)
        
        (map-set tree-adoptions adoption-id (merge adoption-data {
            active: false
        }))
        
        (map-set sponsor-stats tx-sender {
            trees-sponsored: (get trees-sponsored sponsor-data),
            total-invested: (get total-invested sponsor-data),
            active-sponsorships: (- (get active-sponsorships sponsor-data) u1)
        })
        
        (if (> remaining-payment u0)
            (try! (as-contract (stx-transfer? remaining-payment tx-sender (get sponsor adoption-data))))
            true
        )
        
        (ok remaining-payment)
    )
)

(define-private (calculate-remaining-payment (adoption-data {sponsor: principal, tree-id: uint, total-payment: uint, milestone-1-claimed: bool, milestone-2-claimed: bool, milestone-3-claimed: bool, adoption-date: uint, active: bool}))
    (let
        (
            (payment-per-milestone (/ (get total-payment adoption-data) u3))
            (claimed-count 
                (+ 
                    (if (get milestone-1-claimed adoption-data) u1 u0)
                    (+ 
                        (if (get milestone-2-claimed adoption-data) u1 u0)
                        (if (get milestone-3-claimed adoption-data) u1 u0)
                    )
                )
            )
        )
        (- (get total-payment adoption-data) (* claimed-count payment-per-milestone))
    )
)

(define-public (sponsor-tree-update (tree-id uint) (health-report (string-ascii 256)))
    (let
        (
            (adoption-id (unwrap! (map-get? tree-to-adoption tree-id) err-tree-not-adopted))
            (adoption-data (unwrap! (map-get? tree-adoptions adoption-id) err-tree-not-adopted))
        )
        (asserts! (is-eq tx-sender (get sponsor adoption-data)) err-not-sponsor)
        (asserts! (get active adoption-data) err-tree-not-adopted)
        
        (ok health-report)
    )
)

;; Tree monitoring functions
(define-public (add-monitoring-entry (tree-id uint) (health-status uint) (height-cm uint) (diameter-mm uint) (notes (string-ascii 128)))
    (let
        (
            (tree-data (unwrap! (map-get? trees tree-id) err-not-found))
            (monitoring-id (var-get next-monitoring-id))
            (current-monitoring (map-get? tree-latest-monitoring tree-id))
            (monitoring-count (default-to u0 (map-get? tree-monitoring-count tree-id)))
            (current-block stacks-block-height)
        )
        ;; Validate inputs
        (asserts! (get verified tree-data) err-not-found)
        (asserts! (and (>= health-status u1) (<= health-status u5)) err-invalid-health-status)
        
        ;; Check authorization: verifiers or sponsors can monitor
        (asserts! (or (is-verifier tx-sender) (is-authorized-sponsor tree-id tx-sender)) err-not-authorized-monitor)
        
        ;; Prevent too frequent monitoring (minimum 72 blocks / ~12 hours)
        (match current-monitoring
            latest-data
                (asserts! (>= (- current-block (get last-update latest-data)) u72) err-monitoring-too-frequent)
            true
        )
        
        ;; Create monitoring entry
        (map-set tree-monitoring-entries monitoring-id {
            tree-id: tree-id,
            reporter: tx-sender,
            health-status: health-status,
            height-cm: height-cm,
            diameter-mm: diameter-mm,
            notes: notes,
            timestamp: current-block,
            entry-id: monitoring-id
        })
        
        ;; Update monitoring counters and latest status
        (map-set tree-monitoring-count tree-id (+ monitoring-count u1))
        (map-set tree-latest-monitoring tree-id {
            last-entry-id: monitoring-id,
            last-update: current-block,
            current-health: health-status
        })
        
        (var-set next-monitoring-id (+ monitoring-id u1))
        (ok monitoring-id)
    )
)

;; Helper function to check if user is authorized sponsor for tree
(define-private (is-authorized-sponsor (tree-id uint) (user principal))
    (match (map-get? tree-to-adoption tree-id)
        adoption-id
            (match (map-get? tree-adoptions adoption-id)
                adoption-data
                    (and (is-eq (get sponsor adoption-data) user) (get active adoption-data))
                false
            )
        false
    )
)

;; Tree monitoring read-only functions
(define-read-only (get-monitoring-entry (entry-id uint))
    (map-get? tree-monitoring-entries entry-id)
)

(define-read-only (get-tree-latest-monitoring (tree-id uint))
    (map-get? tree-latest-monitoring tree-id)
)

(define-read-only (get-tree-monitoring-count (tree-id uint))
    (default-to u0 (map-get? tree-monitoring-count tree-id))
)

(define-read-only (get-tree-health-status (tree-id uint))
    (match (map-get? tree-latest-monitoring tree-id)
        monitoring-data (get current-health monitoring-data)
        u0
    )
)

;; Calculate tree growth metrics from first and latest monitoring
(define-read-only (calculate-tree-growth (tree-id uint))
    (let
        (
            (latest-monitoring (map-get? tree-latest-monitoring tree-id))
            (monitoring-count (get-tree-monitoring-count tree-id))
        )
        (if (> monitoring-count u1)
            (match latest-monitoring
                latest-data
                    (match (map-get? tree-monitoring-entries (get last-entry-id latest-data))
                        current-entry
                            {
                                has-growth-data: true,
                                current-height: (get height-cm current-entry),
                                current-diameter: (get diameter-mm current-entry),
                                monitoring-count: monitoring-count,
                                health-status: (get current-health latest-data)
                            }
                        {
                            has-growth-data: false,
                            current-height: u0,
                            current-diameter: u0,
                            monitoring-count: monitoring-count,
                            health-status: u0
                        }
                    )
                {
                    has-growth-data: false,
                    current-height: u0,
                    current-diameter: u0,
                    monitoring-count: u0,
                    health-status: u0
                }
            )
            {
                has-growth-data: false,
                current-height: u0,
                current-diameter: u0,
                monitoring-count: monitoring-count,
                health-status: u0
            }
        )
    )
)

(define-read-only (is-tree-healthy (tree-id uint))
    (let
        (
            (health-status (get-tree-health-status tree-id))
        )
        (and (> health-status u0) (>= health-status u3))
    )
)



