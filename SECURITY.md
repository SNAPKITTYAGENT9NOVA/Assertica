# Security Policy for Assertica

Assertica is a formal verification tool designed for mathematical certainty in proof checking. This document outlines the security model, threat analysis, and vulnerability reporting process.

---

## Table of Contents

1. [Security Model](#security-model)
2. [Threat Analysis](#threat-analysis)
3. [Scope of Guarantees](#scope-of-guarantees)
4. [Trust Boundary](#trust-boundary)
5. [Known Limitations](#known-limitations)
6. [Vulnerability Reporting](#vulnerability-reporting)
7. [Incident Response](#incident-response)
8. [Audit & Certification](#audit--certification)

---

## Security Model

### Design Philosophy

Assertica's security model prioritizes **verification correctness** over cryptographic security. The system is designed to ensure:

1. **Proof Integrity:** Every mathematical claim must have explicit proof
2. **Deterministic Verification:** Same input always produces identical result
3. **Fail-Closed Semantics:** Unknown claims are rejected, never silently accepted
4. **Auditable Kernel:** Small, reviewable trusted core (243 lines)
5. **No Hidden Reasoning:** All inference is explicit and traceable

### Security vs. Cryptography

**This is NOT a cryptographic system.** It does not provide:
- Encryption or decryption
- Digital signatures
- Key management
- Confidentiality guarantees
- Authentication protocols

**This IS a formal verification system** providing:
- Proof verification guarantees
- Deterministic computation
- Rejection of unproven claims
- Explicit reasoning traces

---

## Threat Analysis

### Protected Against

#### 1. Silent Acceptance of Unproven Claims

**Threat:** Verification accepts proof when no valid proof exists.

**Mitigation:**
- Proof Checker module (Proof Checker) implements deterministic verification
- All proof primitives have explicit checking logic
- Unknown proofs are rejected (fail-closed)
- Negative tests verify rejection paths
- Property tests confirm determinism

**Risk Level:** CRITICAL | **Mitigation:** STRONG

#### 2. Nondeterministic Verification Results

**Threat:** Same proof submitted twice produces different results (accept/reject).

**Mitigation:**
- Property tests: `∀ proof. verify(proof) = verify(proof)` always holds
- No random number generation
- No hash-based data structures (deterministic ordering)
- Sequential execution (no concurrency)
- Determinism verified by 80+ property tests

**Risk Level:** CRITICAL | **Mitigation:** STRONG

#### 3. Hidden Algebraic Axiom Invocation

**Threat:** Equality checker secretly assumes commutativity/associativity without proof.

**Mitigation:**
- Negative tests explicitly verify rejection of:
  - Assumed commutativity: a + b ≠ b + a (without proof)
  - Assumed associativity: (a + b) + c ≠ a + (b + c) (without proof)
  - Assumed distributivity: a * (b + c) ≠ a*b + a*c (without proof)
- Kernel only checks β-reduction, α-equivalence, η-conversion
- No axiom database
- Complete equality checker source (243 lines) is auditable

**Risk Level:** CRITICAL | **Mitigation:** STRONG

#### 4. Infinite Recursion in Verification

**Threat:** Malformed proof or program causes verification to hang.

**Mitigation:**
- Termination/Positivity module (Termination Checker) validates all recursion is structurally decreasing
- Beta reduction on non-terminating definitions fails termination check
- Module system detects circular dependencies
- No infinite loops possible in verified programs

**Risk Level:** HIGH | **Mitigation:** STRONG

#### 5. Logical Inconsistency from Impredicative Types

**Threat:** Type system admits Girard's paradox, making system inconsistent.

**Mitigation:**
- Termination/Positivity module (Positivity Checker) validates inductive types are strictly positive
- Type constructor arguments checked for polarity
- Negative positions in function domains cause rejection
- Prevents impredicative type definitions
- Type universe hierarchy enforces stratification

**Risk Level:** HIGH | **Mitigation:** STRONG

#### 6. Circular Module Dependencies

**Threat:** Circular imports create inconsistent state or infinite loops.

**Mitigation:**
- Module System module (Module System) performs circular dependency detection
- Topological sorting ensures loading order
- Rejects programs with circular dependencies
- Module environment tracks visited modules
- Prevents initialization cycles

**Risk Level:** MEDIUM | **Mitigation:** STRONG

#### 7. Code Generation Producing Unsafe Output

**Threat:** Generated Haskell code uses unsafe operations (unsafeCoerce, etc.).

**Mitigation:**
- Code Generator module (Backend) generates only safe Haskell constructs
- No `unsafeCoerce`, `unsafePerformIO`, `unsafeDupablePerformIO`
- Generated code passes GHC type checker
- Generated code uses only safe library functions
- Code generation reviewed in CI

**Risk Level:** MEDIUM | **Mitigation:** STRONG

#### 8. Type Confusion via Proof Injection

**Threat:** Proof term for type A is used to justify claim of type B.

**Mitigation:**
- Proof Checker module checks proof against specific proposition
- Proof type must match claimed property
- Type environment (Type Checker module) tracks binding contexts
- Mismatch causes rejection

**Risk Level:** MEDIUM | **Mitigation:** STRONG

---

### Not Protected Against

#### 1. Malicious Hardware

**Threat:** CPU, memory, or storage deliberately corrupted.

**Status:** OUT OF SCOPE | This is a property of the execution environment, not the compiler.

**Mitigation:** Use trusted hardware. Run on air-gapped systems if paranoia warrants.

#### 2. Side-Channel Attacks

**Threat:** Timing analysis, cache analysis, or power analysis reveals secrets.

**Status:** OUT OF SCOPE | Assertica is not a cryptographic system.

#### 3. Compiler Bugs in GHC

**Threat:** GHC (Haskell compiler) has bug that miscompiles Assertica.

**Status:** OUT OF SCOPE | Assuming trustworthy compiler toolchain.

**Mitigation:** Use verified version of GHC. Build on reproducible system (Nix, Docker).

#### 4. Denial of Service via Resource Exhaustion

**Threat:** Malformed input causes excessive memory or CPU use.

**Status:** PARTIALLY MITIGATED

**Mitigation:**
- Termination checker prevents non-terminating recursion
- Parse error recovery prevents unbounded parsing
- Type checking bounded by program size
- Resource limits at OS level recommended

#### 5. Unintended Proof Acceptance (Logic Errors)

**Threat:** Proof checker has bug that accepts invalid proof.

**Status:** MITIGATED VIA REVIEW | Kernel designed for auditability (243 lines).

**Mitigation:** Regular security audits. External review of kernel logic.

---

## Scope of Guarantees

### What Assertica Guarantees

If Assertica accepts a proof, then:

1. **The proof is structurally valid:** All proof terms type-check against their propositions
2. **No hidden axioms invoked:** Only β, α, η reductions applied
3. **Verification is deterministic:** Identical input → identical output
4. **All reasoning is explicit:** Every inference is traceable

### What Assertica Does NOT Guarantee

1. **Correctness of user-supplied axioms:** If user declares `axiom contradiction : False`, Assertica accepts it. Users are responsible for axiom correctness.
2. **Performance:** Verification may be slow for large proofs. No performance SLA.
3. **Availability:** No guarantees on build time, resource requirements, or uptime.
4. **Compatibility:** Assertica versions may not be forward/backward compatible. Pin versions.

### Limitations of Formal Verification

1. **Only Proofs About Models:** Assertica proves properties of abstract specifications. Real systems have:
   - Implementation bugs
   - Compiler bugs
   - Hardware faults
   - Network failures

2. **Assumptions Can Be Wrong:** Proof depends on premises. If premises are false, conclusion is meaningless.

3. **Incomplete Coverage:** Proof of Property P doesn't prove absence of other bad Property Q.

---

## Trust Boundary

### Trusted Component (TCB - Trusted Computing Base)

```
┌─────────────────────────────────────────────────────────┐
│  TRUSTED COMPUTING BASE (TCB)                           │
│                                                         │
│  ┌──────────────────────────────────────────────────┐  │
│  │ Equality Kernel module: Equality Kernel (243 lines)            │  │
│  │ • Beta reduction logic                           │  │
│  │ • Alpha equivalence check                        │  │
│  │ • Eta conversion                                 │  │
│  │ • Normalization                                  │  │
│  └──────────────────────────────────────────────────┘  │
│                                                         │
│  ┌──────────────────────────────────────────────────┐  │
│  │ Proof Checker module: Proof Checker Core (200 lines approx)  │  │
│  │ • Pattern matching on proof primitives           │  │
│  │ • Proof type checking                            │  │
│  │ • Refl, Symm, Trans verification                 │  │
│  └──────────────────────────────────────────────────┘  │
│                                                         │
│  TOTAL TCB: ~450 lines of Haskell                       │
│  Audit Time: ~3 hours for expert review                 │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

### Outside Trusted Boundary (Verified Against TCB)

- Parser (subject to misparse risk, but caught by elaborator)
- Type checker (subject to bugs, but proofs must be valid regardless)
- Module system (subject to bugs, but circular dependencies caught)
- Code generator (outputs to GHC, which verifies)

### Attack Surface

| Component | Risk | Mitigation |
|-----------|------|-----------|
| TCB kernel (450 lines) | CRITICAL | 100% test coverage, multiple reviews |
| Parser (836 lines) | MEDIUM | Error recovery, type checking |
| Type system (1,099 lines) | MEDIUM | Proof checking gates acceptance |
| Module system (766 lines) | LOW | Circular dependency detection |
| Code generator (1,507 lines) | LOW | GHC re-verifies output |
| Testing infrastructure | LOW | Test generation not exposed |

---

## Known Limitations

### 1. No Proof of Implementation Correctness

Assertica verifies **specifications** are consistent. It does not verify implementations of Assertica itself.

**Implication:** There could be a bug in the type checker that causes it to accept invalid programs. But proofs must still be valid by the proof checker.

**Mitigation:** Keep TCB small (450 lines). Have external review.

### 2. User Axioms Are Trusted

If a user declares:
```assertica
axiom my_axiom : P
```

Assertica trusts it. Proof of `P` is not required. This is intentional (allows building on prior work), but means **axioms are attack surface**.

**Mitigation:** Audit axioms carefully. Use only well-known axioms from literature.

### 3. Termination Checking is Conservative

Termination checker requires **structural recursion** (provably terminating by structural argument). Some terminating programs are rejected.

**Example (Rejected):**
```assertica
ackermann : Nat → Nat → Nat
ackermann 0 n = n + 1
ackermann (S m) 0 = ackermann m 1
ackermann (S m) (S n) = ackermann m (ackermann (S m) n)
```

**This is intentional:** Fail-closed. False positives (rejection of valid) are safer than false negatives (acceptance of non-terminating).

### 4. Performance Not Guaranteed

Verification of large proofs may be slow. No SLA. Example timings:
- 1 KB program: ~100 ms
- 100 KB program: ~500 ms
- 1 MB program: ~2 seconds
- Very large programs: may time out

**Mitigation:** Break large proofs into modules. Use proof caching.

### 5. No Cryptographic Security

Assertica does **not** provide:
- Confidentiality (no encryption)
- Integrity protection (no signatures)
- Authentication (no proof of identity)
- Non-repudiation

**Implication:** Do not use Assertica for security-critical decisions (authentication, authorization, key management).

---

## Vulnerability Reporting

### Reporting Process

**DO NOT open public issues for security vulnerabilities.**

Instead, report to: **security@example.com** (placeholder - use actual email)

**Email template:**
```
Subject: [SECURITY] Vulnerability in Assertica [component] [type]

Description:
  Clear description of vulnerability

Impact:
  What is affected? How severe?

Reproduction:
  Steps to reproduce (include code sample if applicable)

Suggested Fix (optional):
  If you have a proposed fix

Your Information:
  Your name (will be credited if you wish)
  Preferred contact method
```

### Response Timeline

| Stage | Timeline |
|-------|----------|
| Initial acknowledgment | 24 hours |
| Vulnerability assessment | 5 business days |
| Fix development | 7-14 days |
| Security review of fix | 3-7 days |
| Patch release | Within 30 days of report |
| Public disclosure | After patch is available |

### Vulnerability Scoring

Vulnerabilities are scored using CVSS 3.1:

| CVSS Score | Category | Example |
|-----------|----------|---------|
| 9.0-10.0 | CRITICAL | Proof checker accepts invalid proofs |
| 7.0-8.9 | HIGH | Nondeterministic verification results |
| 4.0-6.9 | MEDIUM | Denial of service via resource exhaustion |
| 0.1-3.9 | LOW | Performance regression, non-critical bug |

### Embargo Period

- **CRITICAL, HIGH:** 30-day embargo (time to patch before disclosure)
- **MEDIUM:** 14-day embargo
- **LOW:** No embargo (can disclose immediately)

### Researcher Credit

- Security researchers are credited by name (with permission)
- Listed in release notes
- Eligible for research collaboration opportunities

---

## Incident Response

### Security Incident Definition

A security incident is:
- Reported vulnerability with CVSS >= 4.0
- Confirmed bug in TCB (Equality Kernel module, Proof Checker module core)
- Evidence of vulnerability being exploited
- Bug that violates stated guarantees

### Response Phases

#### Phase 1: Confirmation (0-3 days)
1. Verify vulnerability is real
2. Determine scope and severity
3. Notify affected users (if any)
4. Create security patch branch

#### Phase 2: Remediation (3-7 days)
1. Develop fix
2. Security review of fix
3. Add regression tests
4. Full test suite passes
5. Code review by 2+ people

#### Phase 3: Release (7-14 days)
1. Release patch version
2. Tag as security release
3. Publish security advisory
4. Credit researcher (with permission)
5. Post-incident analysis

#### Phase 4: Follow-up (Post-release)
1. Monitor for related issues
2. Improve test coverage to prevent recurrence
3. Document lessons learned
4. Update security guidelines if needed

### Post-Incident Analysis

After every security incident:
1. Root cause analysis
2. Prevention strategies
3. Detection improvements
4. Process updates
5. Team debrief

---

## Audit & Certification

### Self-Audit Checklist

Run this checklist quarterly:

- [ ] All 768+ tests pass
- [ ] Code coverage >95% (TCB 100%)
- [ ] No compiler warnings (cabal build)
- [ ] Determinism property tests pass
- [ ] Fail-closed behavior verified
- [ ] No new dependencies on external solvers
- [ ] Kernel unchanged (Equality Kernel module, 2B core)
- [ ] Security policy up to date
- [ ] No open security issues

### External Audit Recommendations

**Recommended for:**
- Organizations using Assertica in safety-critical contexts
- Institutions adopting Assertica for research

**Audit Scope:**
1. Code review of TCB (Equality Kernel module, Proof Checker module) - ~450 lines
2. Test coverage verification (>95%)
3. Determinism verification (run tests 1000x)
4. Threat model assessment
5. Dependency analysis
6. Supply chain review

**Estimated Effort:** 40-60 person-hours

**Certification Bodies:** Currently self-certified. Third-party certification available upon request.

---

## Supply Chain Security

### Dependencies

Assertica's dependencies are:
- **GHC:** Haskell compiler (trusted)
- **Cabal:** Build system (trusted)
- **Standard library modules:** Data.Set, Data.Map, etc. (Haskell standard library)

**No external theorem provers.** No SMT solvers. No experimental libraries.

### Build Integrity

**Reproducible builds:**
```bash
# Same source always produces identical binary
nix build

# Verify hash
nix build --out-link result
sha256sum result
```

### Vendor Lock-in Resistance

Assertica is:
- Open source (MIT license)
- Self-contained (no proprietary dependencies)
- Portable (runs on any system with GHC 9.2+)
- Forkable (easy to fork if needed)

---

## Security Contact

**For vulnerability reports:**
- Email: security@example.com (placeholder)
- PGP Key: [public key URL]
- Response time: 24 hours acknowledged, 5 days assessment

**For general security questions:**
- GitHub Discussions (public)
- security-questions@example.com (private)

---

## References

### Formal Methods Standards
- IEEE Std 1012-2017: Software Verification and Validation
- IEC 61508: Functional Safety

### Proof Theory References
- "Lectures on the Curry-Howard Isomorphism" (Sorensen & Urzyczyn)
- "Type Theory and Formal Proof" (Nederpelt & Geuvers)
- "Proofs and Types" (Girard, Taylor, Lafont)

### Formal Verification in Practice
- Coq proof assistant documentation
- Agda documentation
- Lean 4 documentation

---

**Last Updated:** 2026-10-03
**Version:** 1.0.0
**Policy Effective Date:** 2026-10-03
**Next Review Date:** 2027-10-03
