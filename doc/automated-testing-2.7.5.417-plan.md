# Amber 2.7.5.417: CLI and web automated testing plan

## Objective

Extend Amber from a command-line automation framework into a generic automated
test framework that can validate both command-line and web behavior. Amber must
remain reusable by unrelated applications. No application name, URL, page
object, credential, database schema, translation set, or product workflow may
be required by Amber itself.

The target build is **2.7.5.417**, including the release metadata and runtime
version reported by the current Amber checkout.

## Review completed

### Amber

- Amber owns the generic test input factory: plans, suites, cases, includes,
  steps, substitutions, evidence, writers, and report output.
- The consuming application owns the report directory. Amber receives it with
  `--report-dir`; its input is `<report-dir>/factory` and its output is
  `<report-dir>/test-output`.
- CLI parsing is centralized in `Amber::CommandLineOptions`, with shared
  concepts for browser, language, writer, plan, suite, case, file, dry-run,
  simulation, logging, and output cleanup.
- `Amber::TestStep` currently builds and executes shell commands. Its evidence
  path and lifecycle are the natural extension point for another execution
  adapter.
- The current gem description and README still define Amber as command-line
  testing only.
- The current validation task invokes Amber's own plan and builds a report, but
  there is no generic browser execution path in Amber.

### Daryn

- Daryn contains a second implementation of most Amber CLI options and option
  state. The overlap includes browser, language, writer, plan, suite, case,
  file, dry-run, environment, simulation, logging, verbose, and version
  behavior.
- `Daryn::CommandLineOptions` contains a duplicated `dryrun_option` method;
  the later definition silently replaces the first one.
- Daryn's CLI also mixes framework options with application-specific options:
  `--application`, `--username`, `--password`, and `--inputfolder`.
- Daryn provides reusable web capabilities: browser startup and driver
  configuration, page objects, page factories, screenshots, downloads, PDF
  handling, browser navigation, local OCR, environment information, and
  concurrency coverage.
- Daryn's web behavior is coupled to its own configuration and output helpers,
  but several pieces already support dependency injection, including browser
  factories and configurable paths.
- Daryn's test fixtures include product-like translation and organization data.
  Those are valid Daryn fixtures but must not become Amber requirements.
- Daryn currently shells out to Amber for version/environment information and
  has a configurable `AMBER_COMMAND`; this is a useful compatibility seam for
  integration validation.

## Architectural decision proposed for approval

Amber should own the framework contract and orchestration. Web support should
be implemented as a generic execution capability, not as application logic.
Daryn is obsolete and will not be an adapter or runtime dependency. Its useful
generic ideas may be reimplemented in Amber behind injected interfaces, but
Daryn code, configuration, fixtures, and application coupling are not part of
the Amber architecture.

The boundary should look like this:

```text
Application repository
  application page objects, URLs, fixtures, credentials, workflows
          |
          v
Injected web adapter supplied by the consuming application
  browser lifecycle, navigation, screenshots, downloads, OCR hooks
          |
          v
Amber framework
  CLI/configuration, plan-suite-case model, execution lifecycle,
  evidence, writers, report paths, status and exit behavior
```

Amber must depend only on generic interfaces. A consuming application supplies
page objects, browser configuration, navigation data, and fixtures through
configuration or injected objects. The web implementation itself belongs in
Amber for this release, while application-specific adapters remain in the
consumer repository.

## Scope rules

### In scope for 2.7.5.417

- A single documented framework contract for CLI and web test execution.
- One canonical definition of shared command-line options and defaults.
- Generic web execution hooks that can run without a named application.
- Browser lifecycle, navigation, screenshot, download, and evidence capture
  contracts with injectable implementations.
- Dry-run/simulate behavior that does not start a browser or mutate an
  application.
- Deterministic local fixtures for Amber self-validation.
- Validation of Amber's CLI and web framework features in the Amber checkout.
- Version, README, gemspec, help output, and restart documentation updates.

### Out of scope for Amber

- Product URLs, product page classes, product translations, product database
  adapters, product credentials, or product-specific workflows.
- Requiring a browser binary, OCR, or a network service
  for every Amber installation or every CLI test.
- Moving all of Daryn's application helper code into Amber.
- Supporting live external websites as test prerequisites.

## Implementation phases

### Phase 1 — Freeze and define the contract

1. Confirm the target Ruby versions and supported browsers.
2. Define the canonical option model and precedence rules.
3. Decide whether web tests are represented by a new step type, an execution
   adapter selected by the case, or a separate web case class.
4. Define lifecycle callbacks for setup, action, assertion, teardown, and
   evidence capture.
5. Define failure semantics: exception handling, step status, process exit
   status, and partial evidence.
6. Define output naming and directory rules so CLI and web evidence can be
   consumed by the existing writers without browser/application assumptions.
7. Define `--report-dir` as the boundary between Amber and the consuming
   application's report factory.

### Phase 1A — Migrate the Daryn factory contract

1. Merge the Daryn plan into Amber's `factory/plan/master`.
2. Merge Daryn's generic web suites into `factory/suite/web/primitives`.
3. Rewrite eligible Daryn cases as Amber-native web cases under
   `factory/case/web/primitives`.
4. Record every Daryn plan, suite, case, and `factory/config` item in a
   migration matrix marked `migrate`, `rewrite`, or `exclude`.
5. Exclude Daryn application-specific URLs, page objects, VCORE resources,
   credentials, database scripts, teleport routes, and product workflows.

### Phase 2 — Remove CLI duplication

1. Make Amber's option schema/parser the canonical shared implementation.
2. Remove Daryn's duplicated shared parser/state from the design; there is no
   Daryn compatibility layer to preserve.
3. Correct incompatible behavior and names found during the review, including
   Daryn's duplicate
   `dryrun_option`, `--Simulate` capitalization, dump behavior, and plan/suite/
   case storage differences.
4. Add contract tests for the canonical parser and the integration entry point.

### Phase 3 — Add generic web execution to Amber

1. Introduce a web executor interface with no Watir/Selenium types in the core
   contract. The interface should support start, navigate/action, assertion,
   capture evidence, and close.
2. Add adapter injection/factory configuration for browser implementations.
3. Model browser selection as a capability/configuration value, not as a
   product name or page implementation.
4. Allow a case to declare generic web actions or reference an injected web
   runner while retaining the existing CLI step format.
5. Route web results through the existing test-step/evidence/writer pipeline.
6. Ensure dry-run and simulation validate parsing and output structure without
   opening a browser.

### Phase 4 — Add the built-in web implementation without application specifics

1. Implement generic browser startup, adapter-driven navigation,
   screenshot, download, PDF, and OCR translation-validation facilities behind
   the web executor contract.
2. Keep application configuration, credentials, translation resources, and
   page classes outside Amber.
3. Replace hard-coded output assumptions with an injected Amber output/evidence
   sink or a documented path contract.
4. Retain local fixtures for browser tests and remove live-site requirements.
5. Validate Chrome, Brave, Edge, and Firefox, with clear
   pending/skip behavior when a browser is unavailable.

### Phase 5 — Amber self-validation and release

1. Add application-neutral CLI fixtures that exercise command execution,
   failure, logging, dry-run, simulation, filtering, and report generation.
2. Add application-neutral web fixtures, preferably local HTML and generated
   files, that exercise navigation, assertion, screenshot, download, OCR
   translation comparison, and teardown behavior.
3. Run the full deterministic suite without a browser.
4. Run the browser suite separately and record prerequisites and results.
5. Verify gem contents, executable help/version output, and the 2.7.5.417
   version in all generated/report-facing locations.
6. Update README and release notes with framework usage, adapter boundaries,
   and restart instructions.

## Proposed neutral test model

The approved YAML/API shape is documented in
`doc/web-navigation-format.md`. The model distinguishes the framework action
from its adapter:

```yaml
case:
  name: local web capability
  steps:
    - type: web
      action: navigate
      target: fixture://web/page_mock.html
    - type: web
      action: assert
      target: heading
      expectation: visible
    - type: web
      action: capture
      evidence: screenshot
```

`fixture://web/page_mock.html`, `heading`, and the expected state above are
framework test fixtures, not application implementation. A consuming
application registers its own adapter and page/action vocabulary without
changing Amber.

## Acceptance criteria

- `amber --version` reports `2.7.5.417`.
- Amber's existing CLI validation remains green, including report/evidence
  generation and failure exit behavior.
- The shared option matrix has one owner; no Daryn copy is needed to parse
  Amber options.
- A clean Amber checkout can run all non-browser tests without Daryn, Watir,
  Selenium, browser drivers, credentials, or network access.
- A local browser fixture can validate a generic web case through the same
  plan/suite/case/evidence lifecycle as a CLI case.
- Web adapter failure produces deterministic step status, diagnostics, and a
  non-success result.
- OCR translation validation records recognized text, selected language,
  comparison outcome, and diagnostic evidence without embedding application
  translation resources in Amber.
- Application-specific names and resources exist only in consumer fixtures or
  adapters, never in Amber core validation.
- Chrome, Brave, Edge, and Firefox browser validation pass where installed; unavailable optional
  browsers are reported explicitly rather than causing unrelated CLI tests to
  fail.
- The gem package contains only intended framework code and documentation.

## Decisions recorded

- Daryn is obsolete and will not be used or maintained as part of Amber.
- Web tests will use YAML to describe the test case and injected adapters to
  supply application behavior.
- Amber, not the consuming application, owns adapter registration and
  selection. YAML step types are the definitive adapter-selection mechanism;
  `--adapter` and `--web` are not required for 2.7.5.417.
- Web support uses a middle ground: YAML provides portable generic actions for
  common interactions, while injected Ruby adapters/page objects handle
  complex application workflows.
- Web support includes OCR-based translation validation. Amber owns OCR
  execution, comparison, diagnostics, and evidence; consuming applications
  supply language resources and expected translations.
- Screenshots, downloads, OCR, and PDF capture are core Amber web capabilities.
- The required browser matrix is Chrome, Brave, Edge, and Firefox.
- Existing LaTeX output compatibility is required for 2.7.5.417.
- Ruby 4 is the minimum supported Ruby version.
- Arch Linux is the primary operating-system and CI target.
- Amber's current CLI is the compatibility baseline. Its parser and options
  object remain the single source of truth; corrections to inconsistent
  behavior must be documented. Legacy YAML steps remain command steps by
  default, `--browser` selects a browser rather than an adapter, and Daryn-only
  options are excluded.
- Amber will introduce an internal neutral result model beneath the existing
  writers. Existing LaTeX output and macros remain a required compatibility
  boundary while web results are added.
- Browser and driver versions will follow current Arch Linux packages rather
  than pinned versions. `bin/update-browser-drivers` updates the browsers and
  drivers through `yay`, then verifies their compatibility. When the AUR
  EdgeDriver package lags behind Edge, the script resolves and installs the
  matching Linux driver from Microsoft's official release endpoint. It tries
  the exact Edge build endpoint first and falls back to the major-release
  endpoint when Microsoft has not published the build-specific alias.

## CLI adapter-selection decision

Web execution is selected by YAML step type. There is no required `--web` or
`--adapter` switch in 2.7.5.417.

### Decision rationale

The YAML step type selects execution, for example `type: web` versus
`type: command`. The CLI stays focused on selecting plans, suites, cases,
files, and run behavior.

This keeps YAML as the source of truth and permits mixed CLI/web cases. The
tradeoff is that a user cannot force a web-only mode without selecting a
web-specific case or suite.

## Canonical option schema and precedence

Amber's command-line options remain the canonical workflow controls: files,
plan/suite/case selection, writer, run mode, dry-run/simulation, logging, and
environment reporting. A web case does not require a new CLI switch.

The YAML `web` block owns web execution configuration. `web.browser` selects
the browser capability, `web.configuration` supplies browser-factory settings,
and each step's `type` selects its adapter. A web case cannot replace a
`type: web` step with a command step.

Injected runtime components have this precedence:

1. `Options#web_adapter_factory`, when supplied, creates the case adapter.
2. An injected `:web` adapter in `Options#adapter_registry` is used next.
3. Amber's built-in browser actions are used otherwise; an injected
   `Options#ocr_engine` adds the generic OCR action to that default adapter.

The browser factory is injected through `Options#browser_factory` when
supplied; otherwise Amber creates its default factory. YAML remains
authoritative for test intent while applications can inject browser, OCR, and
complex-action implementations without application logic in Amber.

## Arch Linux browser-driver baseline

The current Arch Linux workstation reports:

| Browser/driver | Installed browser | Installed driver | Assessment |
| --- | --- | --- | --- |
| Chromium | 152.0.7977.82 | ChromeDriver 152.0.7977.82 | Matching; usable |
| Brave | 153.1.95.104 | ChromeDriver 152.0.7977.82 | Verify Brave Chromium engine; likely mismatch |
| Microsoft Edge | 153.0.4234.48 | EdgeDriver 151.0.4129.59 | Mismatch; not usable |
| Firefox | 155.0.1 | geckodriver 0.37.1 | Compatible according to Mozilla support range |

The baseline must use compatibility rules, not only minimum numeric versions:

- ChromeDriver must match the Chromium/Chrome major, minor, and build version.
- Brave must use a ChromeDriver matching Brave's embedded Chromium engine, not
  merely the Brave product version.
- Edge WebDriver's first three version components must match Edge's first
  three components.
- geckodriver 0.37.1 supports Firefox 115 ESR and newer according to Mozilla's
  compatibility table.

Therefore the Arch CI image must upgrade Edge WebDriver to the installed Edge
153.0.4234 build, verify Brave's embedded Chromium version, and validate
Firefox with geckodriver before browser
validation is considered green. Amber should add a startup diagnostic that
reports browser and driver versions and fails with an actionable mismatch
message.

The repository updater is:

```bash
bin/update-browser-drivers
```

Use `bin/update-browser-drivers --check-only` to inspect and validate the
currently installed versions without updating packages or installing a
fallback driver. The updater requires
the Arch `yay` helper because Brave, Edge, and EdgeDriver are AUR packages.

## Restart prompt for another computer

```text
Resume the Amber 2.7.5.417 CLI and web automated-testing work.

Repository:
  Amber: /home/traap/soup/amber

Read Amber/doc/automated-testing-2.7.5.417-plan.md first. Preserve existing
user changes. Do not
print, replace, or commit local credential/configuration files such as .env.
Do not add application-specific URLs, page classes, credentials, translations,
database schemas, or product workflows to Amber.

The goal is build 2.7.5.417: one reusable Amber framework validating both
command-line and web testing. Amber owns the canonical CLI/options, test
plan-suite-case lifecycle, evidence, writers, and generic web adapter
contract. Daryn is obsolete and must not be added as a dependency or opened
as a runtime source.

All architecture decisions recorded so far are approved. Continue with the
next unchecked TODO item. The next implementation point is the neutral
`fixture://` resolver, YAML navigation/teleport runtime, and safe input-file
loading. Use local deterministic fixtures, run focused tests after each
change, and run the complete CLI and browser suites before release. Update
the TODO and completed lists below as work progresses.
```

## TODO

- [x] Resolve the architecture and compatibility decisions needed before
      implementation.
- [x] Record that Daryn is obsolete and out of the Amber architecture.
- [x] Record YAML cases with injected adapters as the web test model.
- [x] Record Amber as the owner of adapter registration and selection.
- [x] Record YAML step types as the definitive adapter-selection mechanism.
- [x] Record generic YAML web actions plus injected Ruby support for complex
      web workflows.
- [x] Record OCR-based translation validation as a core web capability.
- [x] Record screenshots, downloads, OCR, and PDF capture as core capabilities.
- [x] Record Chrome, Brave, Edge, and Firefox as the required browser matrix.
- [x] Record LaTeX compatibility as a release requirement.
- [x] Record Ruby 4 as the minimum and Linux as the primary target.
- [x] Record Arch Linux as the primary operating-system and CI target.
- [x] Inspected `/usr/bin/msedgedriver`, `/usr/bin/chromedriver`, and
      `/usr/bin/geckodriver` and compared them with installed browsers.
- [x] Chose rolling Arch browser/driver updates instead of version pinning.
- [x] Added `bin/update-browser-drivers` with compatibility verification.
- [x] Analyzed the captured update error: the AUR package remained on
      EdgeDriver 151 while Edge was updated to 153, and the exact-build
      Microsoft release alias returned HTTP 404.
- [x] Added a major-release fallback for Microsoft EdgeDriver lookup and kept
      `--noconfirm` in the Arch update command.
- [x] Sanitized Microsoft EdgeDriver release responses before constructing the
      download URL, preventing CR/NUL characters from corrupting the URL.
- [x] Record Amber's current CLI as the compatibility baseline.
- [x] Record the neutral internal result model with mandatory LaTeX
      compatibility.
- [x] Define and document the canonical option schema and precedence rules.
- [x] Design the generic web executor and evidence interfaces.
- [x] Integrate the neutral result model with existing command execution and
      LaTeX/Ascii writers without changing output compatibility.
- [x] Decide and document the web case YAML/API representation.
- [x] Remove duplicated shared CLI concepts from the Amber design.
- [x] Add adapter injection and web lifecycle handling to Amber.
- [x] Add neutral local CLI and browser fixtures to Amber validation.
- [x] Implement the built-in web capabilities without application-specific
      dependencies.
- [x] Adapt the generic page mock into
      `report/factory/config/web/page_mock.html` without Daryn application
      names, identifiers, URLs, or workflows.
- [x] Implement safe `fixture://` resolution below the report factory.
- [x] Implement the documented neutral YAML navigation and teleport runtime.
- [x] Implement safe YAML input-file resolution below the configured input
      root and pass parsed data to the injected adapter.
- [x] Integrate the neutral page mock and navigation/input fixtures into the
      Amber master web validation plan.
- [x] Migrate the remaining generic Daryn cases as Amber-native cases,
      excluding application-specific cases and SPN files.
- [x] Add focused contract, failure, dry-run, and simulation tests.
- [x] Update version, gemspec, README, and version-facing validation metadata
      for Amber 2.7.5.417 and Ruby 4.
- [x] Run deterministic tests, browser tests, lint, package checks, and report
      generation.
- [x] Review the final diff for application-specific leakage and duplicated
      commands.
- [ ] Propose a release commit; commit/push only when explicitly requested.

## Completed

- [x] Reviewed Amber source, CLI, workflow, test model, Rake tasks, README,
      and current version metadata.
- [x] Reviewed Daryn instructions, CLI, option model, web core, browser
      lifecycle, page objects, navigation, evidence helpers, and validation
      notes.
- [x] Identified duplicated shared CLI options and a duplicate Daryn
      `dryrun_option` definition.
- [x] Identified application-specific concerns that must remain outside Amber.
- [x] Established the proposed framework/adapter ownership boundary and
      recorded Daryn as obsolete.
- [x] Recorded YAML plus injected adapters, core evidence capabilities, and the
      Chrome/Brave/Edge/Firefox browser matrix.
- [x] Created this restartable implementation plan for Amber 2.7.5.417.
- [x] Added the initial neutral execution result and YAML-selected adapter
      registry contracts with focused tests.
- [x] Added the command adapter and compatibility bridge returning the existing
      process tuple to LaTeX and ASCII writers.
- [x] Verified the adapter bridge with 457 RSpec examples and focused RuboCop
      checks.
- [x] Added browser-independent YAML web-action dispatch with injected handlers
      returning neutral execution results.
- [x] Verified YAML web dispatch with 461 RSpec examples and focused RuboCop
      checks.
- [x] Added injected web-session lifecycle and neutral evidence collection for
      screenshots, downloads, PDFs, OCR, and logs.
- [x] Verified web-session and evidence contracts with 467 RSpec examples and
      focused RuboCop checks.
- [x] Added the browser-factory contract for Chrome, Brave, Edge, and Firefox
      with configurable driver paths and download directories.
- [x] Verified browser-factory behavior with 471 RSpec examples and focused
      RuboCop checks.
- [x] Discovered installed Arch Linux drivers before Selenium Manager and
      added Linux binary discovery for Brave and Edge.
- [x] Started and cleanly closed Chrome, Brave, Edge, and Firefox using the
      installed browser/driver matrix.
- [x] Added separate `rake spec:browser` integration coverage using local
      browser fixtures for Chrome, Brave, Edge, and Firefox.
- [x] Verified the deterministic suite passes 488 examples and the browser
      suite passes 7 examples.
- [x] Added generic YAML browser actions for navigation, click, input, browser
      assertions, and screenshot evidence without application-specific logic.
- [x] Verified the browser-action contract with focused unit coverage and
      neutral result/evidence assertions.
- [x] Defined the generic web case representation with a `web` browser/configuration
      block and YAML `type: web` steps.
- [x] Added an injected web case runner that preserves step order and closes
      the session even when a web action raises.
- [x] Integrated explicit web cases into Amber's existing test-case lifecycle;
      legacy command cases retain their existing execution path.
- [x] Added generic download and PDF browser actions that record typed evidence
      without application-specific behavior.
- [x] Verified download and PDF evidence actions with focused browser-action
      contract tests.
- [x] Added an injected OCR action with language metadata and generic exact or
      contains translation assertions.
- [x] Verified OCR extraction, translation mismatch, and exact-match behavior
      without selecting an OCR or translation vendor.
- [x] Made an injected OCR engine available to Amber's default web adapter
      while preserving custom adapter-factory and registry precedence.
- [x] Added web dry-run and simulation protection: YAML and step types are
      validated while browser, adapter, OCR, and evidence execution is skipped.
- [x] Added an application-neutral local HTML fixture and integrated browser
      case coverage for YAML navigation, assertion, screenshot, and teardown.
- [x] Added local browser coverage for generic download and PDF evidence
      actions using temporary output directories.
- [x] Added browser-path OCR translation coverage using a local screenshot and
      an injected, vendor-neutral OCR engine.
- [x] Verified `amber --version`, executable help, gem version and Ruby
      requirement, and unpacked gem contents for 2.7.5.417.
- [x] Normalized legacy RuboCop directives and style issues; full repository
      lint now passes across 99 files without offenses.
- [x] Revalidated the deterministic suite (488 examples), browser suite (7
      examples), and report workflow after release hygiene changes.
- [x] Completed the final application-neutrality and CLI-duplication review;
      Amber core contains no Daryn dependency or product-specific workflow.
- [x] Added the `web/primitives` suite to the master validation plan with
      separate navigation/assertion and evidence test cases.
- [x] Integrated web steps with the existing LaTeX/ASCII step writers so
      browser results and typed evidence appear in validation output.
- [x] Limited `validate:amber` browser execution to Chrome/ChromeDriver while
      retaining the all-browser matrix in the separate browser task.
- [x] Completed `validate:amber`: deterministic tests, Chrome web validation,
      master-plan execution, and LaTeX report build/deploy all passed.
- [x] Enforced the Rails-like factory transformation for all web artifacts:
      relative YAML paths resolve beside the mirrored case log under
      `test-output/factory/case/...`; absolute paths and directory escapes are
      rejected.
- [x] Added the master-plan browser evidence cases for navigation, screenshot,
      download, PDF, and injected OCR translation validation.
- [x] Added the explicit `--report-dir` boundary so Amber can process an
      application's `report/factory` and write only to its `report/test-output`.
- [x] Updated Amber self-validation to invoke the same explicit report boundary
      that Paperboy will use.
- [x] Replaced the environment-dependent `bzless` validation command with the
      standard Linux `ls` command.
- [x] Added the Daryn migration matrix covering the plan, suites, 33 cases,
      and all `factory/config` entries.
- [x] Defined and documented a neutral YAML navigation and teleport format for
      consuming applications such as Paperboy; Daryn SPN command strings are
      not part of the Amber contract.
- [x] Added a neutral report-factory page fixture, navigation document, and
      input fixture for local teleport validation.
- [x] Added confined factory-file and `fixture://` resolution with traversal
      protection.
- [x] Added navigation YAML validation, route selection, teleport execution,
      and injected navigation-adapter input delivery.
- [x] Migrated the remaining supported generic Daryn intents into neutral
      controls and repeated-screenshot Amber cases, with exclusions recorded
      for product-specific, SPN, scroll, and deliberate-failure behavior.
- [x] Expanded the neutral local page fixture into a reusable web capability
      workbench and added an advanced web suite for dynamic controls and
      evidence actions.
