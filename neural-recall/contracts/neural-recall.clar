;; Neural-Recall - AI-Enhanced Blockchain Product Recall Management
;; Improved version with better organization and enhanced functionality

;; ============================================================================
;; ERROR CONSTANTS
;; ============================================================================
(define-constant ERR_UNAUTHORIZED (err u1))
(define-constant ERR_INVALID_PRODUCT (err u2))
(define-constant ERR_INVALID_NEURAL_FINGERPRINT (err u3))
(define-constant ERR_INSUFFICIENT_COLLATERAL (err u4))
(define-constant ERR_ANALYST_NOT_FOUND (err u5))
(define-constant ERR_FINGERPRINT_EXPIRED (err u6))
(define-constant ERR_INVALID_RISK_THRESHOLD (err u7))
(define-constant ERR_DUPLICATE_ANALYSIS (err u8))
(define-constant ERR_INVALID_TELEMETRY_PROOF (err u9))
(define-constant ERR_INVALID_NEURAL_COMMITMENT (err u10))
(define-constant ERR_RISK_SCORE_TOO_LOW (err u11))
(define-constant ERR_RECALL_REQUEST_NOT_FOUND (err u12))
(define-constant ERR_PRODUCT_ALREADY_EXISTS (err u13))
(define-constant ERR_DEADLINE_PASSED (err u14))
(define-constant ERR_INSUFFICIENT_BALANCE (err u15))

;; ============================================================================
;; DATA VARIABLES
;; ============================================================================
(define-data-var contract-owner principal tx-sender)
(define-data-var product-id-nonce uint u0)
(define-data-var recall-request-nonce uint u0)
(define-data-var minimum-collateral uint u1000)
(define-data-var neural-decay-rate uint u5) ;; percentage per year
(define-data-var base-risk-score uint u100)
(define-data-var emergency-pause bool false)

;; ============================================================================
;; DATA MAPS
;; ============================================================================
(define-map products
    uint
    {
        name: (string-ascii 64),
        manufacturer: principal,
        category: (string-ascii 32),
        decay-rate: uint,
        risk-threshold: uint,
        is-active: bool,
        created-at: uint,
        serial-number: (string-ascii 50)
    }
)

(define-map product-neural-fingerprints
    {manufacturer: principal, product-id: uint}
    {
        commitment-hash: (buff 32),
        telemetry-root: (buff 32),
        risk-confidence: uint,
        last-analyzed: uint,
        analyst: principal,
        collateral-amount: uint,
        fingerprint-valid-until: uint,
        verification-count: uint
    }
)

(define-map ai-analyst-profiles
    principal
    {
        prediction-accuracy: uint,
        total-analyses: uint,
        successful-predictions: uint,
        total-collateral: uint,
        is-approved: bool,
        specialization: (string-ascii 50),
        reputation-score: uint
    }
)

(define-map recall-requests
    uint
    {
        requester: principal,
        product-requirements: (list 10 uint),
        risk-threshold: uint,
        accuracy-threshold: uint,
        deadline: uint,
        is-active: bool,
        reward-amount: uint,
        request-type: (string-ascii 20)
    }
)

(define-map neural-compositions
    uint
    {
        parent-product: uint,
        required-components: (list 5 uint),
        composition-logic: (string-ascii 32) ;; "AND", "OR", "THRESHOLD"
    }
)

(define-map manufacturer-risk-profiles
    principal
    {
        base-score: uint,
        prediction-bonus: uint,
        collateral-penalties: uint,
        last-updated: uint,
        total-products: uint
    }
)

(define-map temporal-product-weights
    {product-id: uint, time-period: uint}
    {
        weight-multiplier: uint,
        decay-applied: bool
    }
)

;; Enhanced tracking for better analytics
(define-map product-recall-history
    uint
    {
        recall-count: uint,
        last-recall-date: uint,
        severity-scores: (list 10 uint)
    }
)

;; ============================================================================
;; PRIVATE FUNCTIONS
;; ============================================================================
(define-private (verify-telemetry-path (proof-element (buff 32)) (current-hash (buff 32)))
    (keccak256 (concat current-hash proof-element))
)

(define-private (calculate-risk-score (manufacturer principal))
    (match (map-get? manufacturer-risk-profiles manufacturer)
        risk-profile
        (+ (get base-score risk-profile) 
           (- (get prediction-bonus risk-profile) (get collateral-penalties risk-profile)))
        (var-get base-risk-score)
    )
)

(define-private (is-fingerprint-expired (neural-fingerprint {commitment-hash: (buff 32), telemetry-root: (buff 32), risk-confidence: uint, last-analyzed: uint, analyst: principal, collateral-amount: uint, fingerprint-valid-until: uint, verification-count: uint}))
    (> block-height (get fingerprint-valid-until neural-fingerprint))
)

(define-private (is-buffer-empty (buffer (buff 32)))
    (is-eq buffer 0x0000000000000000000000000000000000000000000000000000000000000000)
)

(define-private (is-contract-owner)
    (is-eq tx-sender (var-get contract-owner))
)

(define-private (cap-value (value uint) (max-value uint))
    (if (<= value max-value) value max-value)
)

;; ============================================================================
;; OWNER FUNCTIONS
;; ============================================================================
(define-public (set-contract-owner (new-owner principal))
    (begin
        (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
        (asserts! (not (var-get emergency-pause)) ERR_UNAUTHORIZED)
        (ok (var-set contract-owner new-owner))
    )
)

(define-public (toggle-emergency-pause)
    (begin
        (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
        (var-set emergency-pause (not (var-get emergency-pause)))
        (ok (var-get emergency-pause))
    )
)

(define-public (set-minimum-collateral (new-minimum uint))
    (begin
        (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
        (asserts! (> new-minimum u0) ERR_INVALID_RISK_THRESHOLD)
        (asserts! (not (var-get emergency-pause)) ERR_UNAUTHORIZED)
        (ok (var-set minimum-collateral new-minimum))
    )
)

(define-public (approve-analyst (analyst principal))
    (begin
        (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
        (asserts! (not (var-get emergency-pause)) ERR_UNAUTHORIZED)
        (match (map-get? ai-analyst-profiles analyst)
            existing-profile (ok (map-set ai-analyst-profiles analyst 
                (merge existing-profile {is-approved: true})))
            (ok (map-set ai-analyst-profiles analyst {
                prediction-accuracy: (var-get base-risk-score),
                total-analyses: u0,
                successful-predictions: u0,
                total-collateral: u0,
                is-approved: true,
                specialization: "",
                reputation-score: u50
            }))
        )
    )
)

(define-public (revoke-analyst (analyst principal))
    (begin
        (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
        (asserts! (not (var-get emergency-pause)) ERR_UNAUTHORIZED)
        (match (map-get? ai-analyst-profiles analyst)
            existing-profile (ok (map-set ai-analyst-profiles analyst 
                (merge existing-profile {is-approved: false})))
            ERR_ANALYST_NOT_FOUND
        )
    )
)

;; ============================================================================
;; PUBLIC FUNCTIONS
;; ============================================================================
(define-public (register-product 
    (name (string-ascii 64)) 
    (category (string-ascii 32)) 
    (threshold uint)
    (serial-number (string-ascii 50)))
    (let ((product-id (+ (var-get product-id-nonce) u1)))
        (asserts! (not (var-get emergency-pause)) ERR_UNAUTHORIZED)
        (asserts! (> threshold u0) ERR_INVALID_RISK_THRESHOLD)
        (asserts! (< threshold u101) ERR_INVALID_RISK_THRESHOLD)
        (asserts! (> (len name) u0) ERR_INVALID_PRODUCT)
        (asserts! (> (len category) u0) ERR_INVALID_PRODUCT)
        (asserts! (> (len serial-number) u0) ERR_INVALID_PRODUCT)
        
        (map-set products product-id {
            name: name,
            manufacturer: tx-sender,
            category: category,
            decay-rate: (var-get neural-decay-rate),
            risk-threshold: threshold,
            is-active: true,
            created-at: block-height,
            serial-number: serial-number
        })
        
        ;; Update manufacturer profile
        (match (map-get? manufacturer-risk-profiles tx-sender)
            existing-profile 
            (map-set manufacturer-risk-profiles tx-sender
                (merge existing-profile {
                    total-products: (+ (get total-products existing-profile) u1),
                    last-updated: block-height
                }))
            (map-set manufacturer-risk-profiles tx-sender {
                base-score: (var-get base-risk-score),
                prediction-bonus: u0,
                collateral-penalties: u0,
                last-updated: block-height,
                total-products: u1
            })
        )
        
        (var-set product-id-nonce product-id)
        (ok product-id)
    )
)

(define-public (submit-neural-fingerprint 
    (product-id uint) 
    (commitment (buff 32)) 
    (telemetry-root (buff 32))
    (risk-confidence uint)
    (collateral-amount uint))
    (let (
        (analyst-profile (unwrap! (map-get? ai-analyst-profiles tx-sender) ERR_ANALYST_NOT_FOUND))
        (product-info (unwrap! (map-get? products product-id) ERR_INVALID_PRODUCT))
    )
        (asserts! (not (var-get emergency-pause)) ERR_UNAUTHORIZED)
        (asserts! (get is-approved analyst-profile) ERR_UNAUTHORIZED)
        (asserts! (>= collateral-amount (var-get minimum-collateral)) ERR_INSUFFICIENT_COLLATERAL)
        (asserts! (and (>= risk-confidence u1) (<= risk-confidence u100)) ERR_INVALID_RISK_THRESHOLD)
        (asserts! (get is-active product-info) ERR_INVALID_PRODUCT)
        (asserts! (not (is-buffer-empty commitment)) ERR_INVALID_NEURAL_COMMITMENT)
        (asserts! (not (is-buffer-empty telemetry-root)) ERR_INVALID_NEURAL_COMMITMENT)
        
        ;; Check for duplicate analysis
        (asserts! (is-none (map-get? product-neural-fingerprints {manufacturer: tx-sender, product-id: product-id})) 
                  ERR_DUPLICATE_ANALYSIS)
        
        (try! (stx-transfer? collateral-amount tx-sender (as-contract tx-sender)))
        
        (map-set product-neural-fingerprints 
            {manufacturer: tx-sender, product-id: product-id}
            {
                commitment-hash: commitment,
                telemetry-root: telemetry-root,
                risk-confidence: risk-confidence,
                last-analyzed: block-height,
                analyst: tx-sender,
                collateral-amount: collateral-amount,
                fingerprint-valid-until: (+ block-height u52560), ;; ~1 year
                verification-count: u0
            }
        )
        
        ;; Update analyst profile
        (map-set ai-analyst-profiles tx-sender 
            (merge analyst-profile {
                total-analyses: (+ (get total-analyses analyst-profile) u1),
                total-collateral: (+ (get total-collateral analyst-profile) collateral-amount)
            })
        )
        
        (ok true)
    )
)

(define-public (create-recall-request 
    (product-requirements (list 10 uint))
    (risk-threshold uint)
    (accuracy-threshold uint)
    (deadline uint)
    (reward-amount uint)
    (request-type (string-ascii 20)))
    (let ((request-id (+ (var-get recall-request-nonce) u1)))
        (asserts! (not (var-get emergency-pause)) ERR_UNAUTHORIZED)
        (asserts! (> (len product-requirements) u0) ERR_INVALID_PRODUCT)
        (asserts! (and (>= risk-threshold u1) (<= risk-threshold u100)) ERR_INVALID_RISK_THRESHOLD)
        (asserts! (and (>= accuracy-threshold u1) (<= accuracy-threshold u100)) ERR_INVALID_RISK_THRESHOLD)
        (asserts! (> deadline block-height) ERR_DEADLINE_PASSED)
        (asserts! (> reward-amount u0) ERR_INVALID_RISK_THRESHOLD)
        (asserts! (> (len request-type) u0) ERR_INVALID_PRODUCT)
        
        (try! (stx-transfer? reward-amount tx-sender (as-contract tx-sender)))
        
        (map-set recall-requests request-id {
            requester: tx-sender,
            product-requirements: product-requirements,
            risk-threshold: risk-threshold,
            accuracy-threshold: accuracy-threshold,
            deadline: deadline,
            is-active: true,
            reward-amount: reward-amount,
            request-type: request-type
        })
        (var-set recall-request-nonce request-id)
        (ok request-id)
    )
)

(define-public (verify-neural-fingerprint 
    (manufacturer principal)
    (product-id uint)
    (telemetry-proof (list 10 (buff 32)))
    (sensor-data (buff 32)))
    (let (
        (neural-fingerprint (unwrap! (map-get? product-neural-fingerprints {manufacturer: manufacturer, product-id: product-id}) ERR_INVALID_NEURAL_FINGERPRINT))
        (analyst-profile (unwrap! (map-get? ai-analyst-profiles tx-sender) ERR_ANALYST_NOT_FOUND))
        (calculated-root (fold verify-telemetry-path telemetry-proof sensor-data))
    )
        (asserts! (not (var-get emergency-pause)) ERR_UNAUTHORIZED)
        (asserts! (get is-approved analyst-profile) ERR_UNAUTHORIZED)
        (asserts! (< block-height (get fingerprint-valid-until neural-fingerprint)) ERR_FINGERPRINT_EXPIRED)
        (asserts! (is-eq calculated-root (get telemetry-root neural-fingerprint)) ERR_INVALID_TELEMETRY_PROOF)
        
        ;; Update fingerprint verification count
        (map-set product-neural-fingerprints {manufacturer: manufacturer, product-id: product-id}
            (merge neural-fingerprint {
                verification-count: (+ (get verification-count neural-fingerprint) u1)
            }))
        
        ;; Update successful prediction count
        (map-set ai-analyst-profiles tx-sender 
            (merge analyst-profile {
                successful-predictions: (+ (get successful-predictions analyst-profile) u1),
                prediction-accuracy: (/ (* (+ (get successful-predictions analyst-profile) u1) u100) 
                                       (+ (get total-analyses analyst-profile) u1)),
                reputation-score: (cap-value (+ (get reputation-score analyst-profile) u5) u100)
            })
        )
        
        ;; Update manufacturer risk profile
        (match (map-get? manufacturer-risk-profiles manufacturer)
            existing-profile (map-set manufacturer-risk-profiles manufacturer 
                (merge existing-profile {
                    prediction-bonus: (+ (get prediction-bonus existing-profile) u10),
                    last-updated: block-height
                }))
            (map-set manufacturer-risk-profiles manufacturer {
                base-score: (var-get base-risk-score),
                prediction-bonus: u10,
                collateral-penalties: u0,
                last-updated: block-height,
                total-products: u0
            })
        )
        
        (ok true)
    )
)

(define-public (update-product-temporal-weight (product-id uint) (time-period uint) (weight uint))
    (begin
        (asserts! (not (var-get emergency-pause)) ERR_UNAUTHORIZED)
        (asserts! (is-some (map-get? products product-id)) ERR_INVALID_PRODUCT)
        (asserts! (and (>= weight u1) (<= weight u200)) ERR_INVALID_RISK_THRESHOLD)
        
        (map-set temporal-product-weights 
            {product-id: product-id, time-period: time-period}
            {weight-multiplier: weight, decay-applied: false}
        )
        (ok true)
    )
)

(define-public (compose-neural-network 
    (parent-product uint)
    (components (list 5 uint))
    (logic (string-ascii 32)))
    (begin
        (asserts! (not (var-get emergency-pause)) ERR_UNAUTHORIZED)
        (asserts! (is-some (map-get? products parent-product)) ERR_INVALID_PRODUCT)
        (asserts! (> (len components) u0) ERR_INVALID_PRODUCT)
        (asserts! (or (is-eq logic "AND") (or (is-eq logic "OR") (is-eq logic "THRESHOLD"))) ERR_INVALID_PRODUCT)
        
        (map-set neural-compositions parent-product {
            parent-product: parent-product,
            required-components: components,
            composition-logic: logic
        })
        (ok true)
    )
)

(define-public (withdraw-collateral (product-id uint))
    (let (
        (neural-fingerprint (unwrap! (map-get? product-neural-fingerprints {manufacturer: tx-sender, product-id: product-id}) ERR_INVALID_NEURAL_FINGERPRINT))
    )
        (asserts! (not (var-get emergency-pause)) ERR_UNAUTHORIZED)
        (asserts! (> block-height (get fingerprint-valid-until neural-fingerprint)) ERR_FINGERPRINT_EXPIRED)
        
        (try! (as-contract (stx-transfer? (get collateral-amount neural-fingerprint) tx-sender tx-sender)))
        
        (map-delete product-neural-fingerprints {manufacturer: tx-sender, product-id: product-id})
        (ok (get collateral-amount neural-fingerprint))
    )
)

(define-public (deactivate-product (product-id uint))
    (let (
        (product-info (unwrap! (map-get? products product-id) ERR_INVALID_PRODUCT))
    )
        (asserts! (or (is-contract-owner) (is-eq tx-sender (get manufacturer product-info))) ERR_UNAUTHORIZED)
        (asserts! (not (var-get emergency-pause)) ERR_UNAUTHORIZED)
        
        (map-set products product-id 
            (merge product-info {is-active: false}))
        (ok true)
    )
)

(define-public (process-recall-claim (request-id uint) (product-id uint))
    (let (
        (recall-request (unwrap! (map-get? recall-requests request-id) ERR_RECALL_REQUEST_NOT_FOUND))
        (neural-fingerprint (unwrap! (map-get? product-neural-fingerprints {manufacturer: tx-sender, product-id: product-id}) ERR_INVALID_NEURAL_FINGERPRINT))
        (analyst-profile (unwrap! (map-get? ai-analyst-profiles (get analyst neural-fingerprint)) ERR_ANALYST_NOT_FOUND))
    )
        (asserts! (not (var-get emergency-pause)) ERR_UNAUTHORIZED)
        (asserts! (get is-active recall-request) ERR_RECALL_REQUEST_NOT_FOUND)
        (asserts! (< block-height (get deadline recall-request)) ERR_DEADLINE_PASSED)
        (asserts! (>= (get risk-confidence neural-fingerprint) (get risk-threshold recall-request)) ERR_RISK_SCORE_TOO_LOW)
        (asserts! (>= (get prediction-accuracy analyst-profile) (get accuracy-threshold recall-request)) ERR_RISK_SCORE_TOO_LOW)
        
        ;; Transfer reward to analyst
        (try! (as-contract (stx-transfer? (get reward-amount recall-request) tx-sender (get analyst neural-fingerprint))))
        
        ;; Deactivate request
        (map-set recall-requests request-id 
            (merge recall-request {is-active: false}))
        
        ;; Update recall history
        (match (map-get? product-recall-history product-id)
            history
            (map-set product-recall-history product-id
                (merge history {
                    recall-count: (+ (get recall-count history) u1),
                    last-recall-date: block-height
                }))
            (map-set product-recall-history product-id {
                recall-count: u1,
                last-recall-date: block-height,
                severity-scores: (list)
            })
        )
        
        (ok true)
    )
)

;; ============================================================================
;; READ-ONLY FUNCTIONS
;; ============================================================================
(define-read-only (get-product-info (product-id uint))
    (map-get? products product-id)
)

(define-read-only (get-product-neural-fingerprint (manufacturer principal) (product-id uint))
    (map-get? product-neural-fingerprints {manufacturer: manufacturer, product-id: product-id})
)

(define-read-only (get-analyst-profile (analyst principal))
    (map-get? ai-analyst-profiles analyst)
)

(define-read-only (get-recall-request (request-id uint))
    (map-get? recall-requests request-id)
)

(define-read-only (get-manufacturer-risk-profile (manufacturer principal))
    (map-get? manufacturer-risk-profiles manufacturer)
)

(define-read-only (get-temporal-weight (product-id uint) (time-period uint))
    (map-get? temporal-product-weights {product-id: product-id, time-period: time-period})
)

(define-read-only (get-neural-composition (parent-product uint))
    (map-get? neural-compositions parent-product)
)

(define-read-only (get-product-recall-history (product-id uint))
    (map-get? product-recall-history product-id)
)

(define-read-only (get-contract-info)
    {
        contract-owner: (var-get contract-owner),
        minimum-collateral: (var-get minimum-collateral),
        neural-decay-rate: (var-get neural-decay-rate),
        base-risk-score: (var-get base-risk-score),
        product-id-nonce: (var-get product-id-nonce),
        recall-request-nonce: (var-get recall-request-nonce),
        emergency-pause: (var-get emergency-pause)
    }
)

(define-read-only (calculate-manufacturer-risk (manufacturer principal))
    (calculate-risk-score manufacturer)
)

(define-read-only (is-fingerprint-valid (product-id uint) (manufacturer principal))
    (match (map-get? product-neural-fingerprints {manufacturer: manufacturer, product-id: product-id})
        fingerprint
        (< block-height (get fingerprint-valid-until fingerprint))
        false
    )
)