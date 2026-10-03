# Contributing to Assertica

Thank you for your interest in contributing to Assertica, a deterministic proof language compiler. This document provides guidelines for participating in the project.

---

## Table of Contents

1. [Code of Conduct](#code-of-conduct)
2. [Getting Started](#getting-started)
3. [Development Setup](#development-setup)
4. [Contribution Types](#contribution-types)
5. [Coding Standards](#coding-standards)
6. [Testing Requirements](#testing-requirements)
7. [Submission Process](#submission-process)
8. [Review Process](#review-process)
9. [DCO: Developer Certificate of Origin](#dco-developer-certificate-of-origin)
10. [Architecture Constraints](#architecture-constraints)

---

## Code of Conduct

Assertica is committed to providing a welcoming and inclusive environment. All contributors are expected to:

- Be respectful and professional
- Provide constructive feedback
- Focus on the technical merit of ideas
- Respect intellectual property
- Acknowledge contributions appropriately

Violations can be reported to project maintainers privately.

---

## Getting Started

### Prerequisites

- **Haskell GHC:** 9.2 or later
- **Cabal:** 3.6 or later
- **Git:** For version control
- **familiarity with:**
  - Haskell programming language
  - Formal verification concepts
  - Type systems and proof theory

### Initial Setup

```bash
# Fork repository on GitHub
# Clone your fork
git clone https://github.com/YOUR_USERNAME/Assertica.git
cd Assertica

# Add upstream remote for syncing
git remote add upstream https://github.com/SNAPKITTYAGENT9NOVA/Assertica.git

# Create development branch
git checkout -b feature/your-feature-name

# Build and test
cabal build
cabal test
```

---

## Development Setup

### Environment Configuration

```bash
# Recommended cabal configuration for development
mkdir -p ~/.cabal
cat >> ~/.cabal/cabal.config << 'EOF'
ghc-options: -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints -fhide-source-paths
tests: True
benchmarks: True
EOF

# Build with coverage
cabal build --enable-coverage

# Install tools for linting (optional but recommended)
cabal install hlint stylish-haskell
```

### Daily Workflow

```bash
# After pulling changes
git fetch upstream
git rebase upstream/main

# Before committing
cabal format  # Format .cabal file
cabal build
cabal test --verbose

# Submit
git push origin feature/your-feature-name
# Create pull request on GitHub
```

---

## Contribution Types

### 1. Bug Fixes

**Criteria:**
- Clear reproduction steps
- Affects functionality or correctness
- Not a known limitation
- Minimal risk to other components

**Process:**
1. Create issue describing bug with reproduction
2. Reference issue in PR (`Fixes #123`)
3. Include test case demonstrating fix
4. Update documentation if needed

**Example:**
```haskell
-- Before: incorrect behavior
buggyFunction x = x + 1

-- After: corrected
buggyFunction x = x + 2

-- Test
test_buggy = assertEqual 3 (buggyFunction 2)
```

### 2. Feature Enhancements (Non-Kernel)

**Eligible Components:**
- Standard library (Standard Library module)
- Backend code generation (Code Generator module)
- Verification reporting (Verification Pipeline module)
- CLI tools (Verification Pipeline module)
- Test infrastructure

**Ineligible Components (Kernel):**
- Core AST (Core AST module)
- Equality checker (Equality Checker module)
- Proof checker (Proof Checker module)
- Type checker (Type Checker module)

**Process:**
1. Open discussion issue first
2. Wait for maintainer feedback (24-72 hours)
3. If approved, proceed with implementation
4. Follow coding standards
5. Add comprehensive tests (min. 30 tests for new modules)
6. Submit PR with detailed description

### 3. Documentation Improvements

**Eligible:**
- README clarifications
- Architecture explanations
- Usage examples
- Inline code comments
- API documentation
- Tutorial content

**Process:**
1. Submit PR directly (no issue required)
2. Clear, technical writing
3. Example code must be tested
4. Build documentation locally to verify:
   ```bash
   cabal haddock --haddock-html --open
   ```

### 4. Test Coverage Expansion

**Requirements:**
- Tests for uncovered code paths
- Determinism verification tests
- Property tests for invariants
- Negative tests (what should fail)
- Integration tests

**Process:**
1. Identify untested code
2. Write comprehensive tests
3. Verify code coverage:
   ```bash
   cabal test --enable-coverage
   cat dist/hpc/html/index.html
   ```
4. Submit PR with test module

### 5. Refactoring (Non-Kernel Only)

**Constraints:**
- No behavior change
- No API change (externally)
- Improves readability or performance
- All tests pass unchanged

**Process:**
1. Open RFC (Request for Comments) issue
2. Wait for approval
3. Refactor with all tests passing
4. Include before/after performance metrics
5. Document rationale in PR

---

## Coding Standards

### Haskell Style Guide

**Module Structure:**
```haskell
{-|
Module      : Assertica.Component.SubComponent
Description : One-line description
Copyright   : (c) 2026 Ahmad Ali Parr
License     : BSL 1.0

Detailed description of module purpose and responsibilities.
Important invariants and guarantees.
-}

{-# LANGUAGE pragmas #-}

module Assertica.Component.SubComponent
  ( -- * Public API
    publicFunction
  , PublicType(..)
    -- * Internal helpers (for testing only)
  , internalHelper
  ) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
```

**Documentation:**
- Every public function has a Haddock comment
- Document type signatures with examples
- Explain non-obvious invariants
- Note side effects clearly

**Example:**
```haskell
-- | Check deterministic equality of two terms
--
-- Two terms are equal if they normalize to the same canonical form
-- (modulo alpha equivalence).
--
-- Examples:
--   isEqual (TApp (TAbs x (TVar x)) (TConst "5")) (TConst "5") == True
--   isEqual (TVar (Var "x")) (TVar (Var "y")) == False
--
-- Properties:
--   - Deterministic: always returns same result for same input
--   - Idempotent: isEqual t1 t2 == isEqual t1 t2
isEqual :: Term -> Term -> Bool
isEqual t1 t2 = alphaEquivalent (normalize t1) (normalize t2)
```

**Naming Conventions:**
- Types: CamelCase (`Term`, `Assertion`)
- Functions: camelCase (`isDefinitionallyEqual`)
- Constants: SCREAMING_SNAKE_CASE (rarely used)
- Type variables: single lowercase letter or descriptive (`a`, `term`)
- Predicates: prefix with `is` or `has` (`isValid`, `hasProperty`)

**Formatting:**
```bash
# Use cabal's built-in formatter
cabal format

# For Haskell code, use consistent indentation
# 2 spaces for indentation (project standard)
# Line length: prefer <100 characters (max 120)

# Example:
foo :: Term -> Term -> Bool
foo t1 t2 =
  let normalized1 = normalize t1
      normalized2 = normalize t2
  in alphaEquivalent normalized1 normalized2
```

**Error Handling:**
- Use `Either String a` for recoverable errors
- Use `Maybe a` for optional values
- Never use `error` or `undefined` in production code
- Provide meaningful error messages

```haskell
-- Good: clear error message
checkTerm :: Term -> Either String ()
checkTerm t
  | isWellFormed t = Right ()
  | otherwise = Left $ "Malformed term: " ++ show t

-- Bad: unhelpful message
checkTerm _ = Left "Error"
```

---

## Testing Requirements

### Minimum Coverage

- **New modules:** 95% code coverage minimum
- **Existing modules:** No decrease in coverage
- **Critical path (Equality Checker module, 2B):** 100% coverage required

### Test Structure

```haskell
module Test.YourModule where

import Test.HUnit
import Test.QuickCheck
import Assertica.YourModule

-- Unit tests
test_basic :: Test
test_basic = TestCase $ assertEqual "basic case"
  expected
  (yourFunction input)

-- Property tests
prop_deterministic :: Term -> Bool
prop_deterministic t = 
  normalize t === normalize t

-- Negative tests (what should fail)
test_rejectInvalid :: Test
test_rejectInvalid = TestCase $ 
  assertEqual "reject invalid"
    (Left "error")
    (validate invalidInput)

-- Comprehensive test suite
main :: IO ()
main = do
  runTestTT $ TestList
    [ test_basic
    , test_another
    ]
  quickCheckResult prop_deterministic
```

### Test Organization

```
test/Test/Component/
├── SubComponentA.hs       # Tests for Agent/Component
├── SubComponentB.hs       # One file per component
└── Integration.hs         # Cross-component tests
```

### Running Tests

```bash
# All tests with verbose output
cabal test --test-show-details=direct

# Specific test
cabal test --test-show-details=direct test:assertica-tests -- --match="Equality"

# With coverage
cabal test --enable-coverage
cabal open dist/hpc/html/index.html
```

---

## Submission Process

### Before Submitting

**Checklist:**
- [ ] Code follows style guide
- [ ] New functions documented with examples
- [ ] All tests pass (`cabal test`)
- [ ] Test coverage >95% for new code
- [ ] No compiler warnings (`-Wall`)
- [ ] Determinism verified (property tests)
- [ ] Negative tests included
- [ ] CONTRIBUTING.md DCO clause signed
- [ ] Commit message clear and descriptive

**Commit Message Format:**

```
[Component] Brief description (50 chars max)

Detailed explanation of changes. Wrap at 72 characters.
Mention related issues: Fixes #123, Related to #456

Architectural decisions and trade-offs if relevant.

Co-Authored-By: Your Name <email@example.com> (if applicable)
```

**Examples:**

```
[Standard Library module] Add Lattice.absorbptionProof function

Implements the absorption theorem proof for lattice structures:
a ⊔ (a ⊓ b) ≈ a

Adds 24 unit tests covering all absorption cases and
proof composition. Verifies proof terms are checked by
Proof Checker module proof checker.

Fixes #145
```

### Submitting a Pull Request

```bash
# Ensure you're up to date
git fetch upstream
git rebase upstream/main

# Push to your fork
git push origin feature/your-feature-name

# Create PR on GitHub with detailed description
```

**PR Template:**
```markdown
## Description
Brief summary of changes

## Motivation
Why is this change needed?

## Changes
- List of modifications
- Affected components
- New files (if any)

## Testing
- Tests added: X unit tests, Y property tests
- Coverage: Z%
- Manual testing: describe what was tested

## Checklist
- [ ] Tests pass locally
- [ ] Coverage maintained/improved
- [ ] Documentation updated
- [ ] DCO signed
- [ ] No breaking changes (if applicable)

## Related Issues
Fixes #123
```

---

## Review Process

### Code Review Policy

**Kernel Changes (Equality Checker module, 2B):**
- Requires 2+ architectural reviews
- 48-hour review period minimum
- All feedback must be addressed
- No force-merge allowed
- Maintainer approval required

**Non-Kernel Changes:**
- Requires 1 maintainer review
- 24-hour review period (unless urgent)
- Feedback incorporated or explained
- One approval to merge

### Review Criteria

Reviewers will evaluate:

1. **Correctness**
   - Does it solve the stated problem?
   - Are edge cases handled?
   - Are there any logical errors?

2. **Quality**
   - Code clarity and maintainability
   - Adherence to style guide
   - Test coverage and quality

3. **Performance**
   - No unnecessary allocations
   - Complexity appropriate to task
   - No regressions in benchmarks

4. **Architecture**
   - Fits within role pair boundaries
   - Clean interfaces
   - No hidden dependencies

5. **Documentation**
   - Clear explanations
   - Examples where helpful
   - Updated README if needed

### Addressing Feedback

- Respond to every comment
- Acknowledge valid points
- Explain disagreements respectfully
- Use "Resolve conversation" only after feedback addressed
- Push updates to same branch (don't force-push)

---

## DCO: Developer Certificate of Origin

By submitting a pull request, you certify that:

```
Developer Certificate of Origin
Version 1.1

By making a contribution to this project, I certify that:

(a) The contribution was created in whole or in part by me and I
    have the right to submit it under the open source license
    indicated in the file; or

(b) The contribution is based upon previous work that, to the best
    of my knowledge, is covered under an appropriate open source
    license and I have the right under that license to submit that
    work with modifications, whether created in whole or in part
    by me, under the same open source license (unless I am
    permitted to submit under a different license), as indicated
    in the file; or

(c) The contribution was provided directly to me by some other
    person who certified (a), (b) or (c) and I have not modified
    it.

(d) I understand and agree that this project and the contribution
    are public and that a record of the contribution (including all
    personal information I submit with it, including my sign-off) is
    maintained indefinitely and may be redistributed consistent with
    this project or the open source license(s) involved.
```

**Sign-off:** Include in commit message:
```
Signed-off-by: Your Name <your.email@example.com>
```

**Git configuration:**
```bash
git config --global user.name "Your Name"
git config --global user.email "your.email@example.com"
git commit -s  # Automatically sign-off
```

---

## Architecture Constraints

### Module Boundaries

Contributions must respect module architecture boundaries:

| Layer | Modules | Contribution Allowed | Contribution Forbidden |
|-------|---------|----------------------|------------------------|
| **Kernel** | Core AST, Equality Checker | Tests, documentation | Core AST, equality logic |
| **Verification** | Assertion System, Proof Checker | Proof primitives, tests | Proof checker core logic |
| **Syntax** | Parser/Elaborator, Type Checker | Surface syntax, type inference tests | Type system fundamentals |
| **Safety** | Pattern Compiler, Termination/Positivity | New safety checks | Pattern/termination algorithms |
| **Organization** | Module System, Standard Library | Algebraic structures, standard library | Module system core |
| **Emission** | Code Generator, Verification Pipeline | Code generation backends, CLI | Pipeline orchestration |

### Invariants That Cannot Change

These are inviolable (violations rejected in review):

1. **Determinism:** Same input must produce identical output
2. **Fail-Closed:** Unknown inputs must be rejected, never silently accepted
3. **No Algebraic Axioms:** Commutativity, associativity, distributivity forbidden in kernel
4. **Proof Requirements:** Propositional equality requires explicit proof
5. **No SMT Solvers:** Zero external theorem prover calls
6. **No Unsafe Code:** No `unsafeCoerce`, `unsafePerformIO`, etc.

### Integration Points

When adding features, ensure clean integration:

- Use existing equality checker (Equality Checker module) - don't reimplement
- All proof verification goes through Proof Checker module
- Type environment via Type Checker module's API
- Module resolution via Module System module
- Code generation through Code Generator module's interfaces

---

## Common Contribution Patterns

### Adding a New Algebraic Structure (Standard Library module)

1. Create new module: `src/Assertica/StdLib/YourStructure.hs`
2. Define structure (analogous to `Setoid`, `Lattice`)
3. Prove algebraic laws with explicit proof terms
4. Create test module: `test/Test/StdLib/YourStructure.hs`
5. Add 30+ comprehensive tests
6. Update `assertica.cabal` with new modules
7. Submit PR with architecture discussion

### Extending Parser/Elaborator (Parser/Elaborator module)

1. Extend token types in `Lexer.hs` if needed
2. Add parser rule in `Parser.hs`
3. Add elaborator case in `Elaborator.hs`
4. Update surface AST (`Surface/AST.hs`)
5. Add 20+ parser tests
6. Add 15+ elaborator tests
7. Document syntax in examples
8. Submit PR with grammar specification

### Adding Backend Code Generation (Code Generator module)

1. Extend target language support in `CodeGen.hs`
2. Implement term compilation in appropriate compiler
3. Ensure generated code is type-safe
4. Add 15+ end-to-end tests
5. Verify output compiles with target compiler
6. Benchmark code generation performance
7. Submit PR with performance analysis

---

## Frequently Asked Questions

**Q: Can I modify the kernel (Equality Checker module)?**
A: Only for critical bug fixes. Changes require 2+ architectural reviews. Bug fix must have failing test demonstrating issue, and passing test after fix.

**Q: What if I disagree with reviewer feedback?**
A: Respectfully explain your position. If disagreement persists, maintainers make final decision. Disagreement is not grounds for force-merge.

**Q: How long does review typically take?**
A: 24-72 hours for non-kernel, 48+ hours for kernel. Depends on complexity and reviewer availability.

**Q: Can I work on multiple features in parallel?**
A: Yes, use separate branches and PRs. Avoid large PRs (500+ lines) - break into logical units.

**Q: What if my contribution introduces a performance regression?**
A: Regressions must be justified and fixed before merge. Include benchmarks showing impact.

---

## Getting Help

- **Architecture questions:** Open discussion issue
- **Technical questions:** GitHub Discussions
- **Bug reports:** GitHub Issues
- **Security issues:** See SECURITY.md
- **Email:** Contact project maintainers

---

**Last Updated:** 2026-10-03
**Version:** 1.0.0
