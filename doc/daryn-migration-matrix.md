# Daryn to Amber Migration Matrix

This matrix records the migration decision for every Daryn report-factory
plan, suite, case, and configuration item reviewed for Amber 2.7.5.417.

The migration target is Amber's application-neutral factory:

```text
factory/plan/master
factory/suite/web/primitives
factory/case/web/primitives
factory/config
```

`migrate` means the generic factory intent can be merged into Amber's existing
factory. `rewrite` means the intent is useful, but the Daryn RSpec command,
page object, URL, resource, or helper must be replaced with Amber-native YAML
steps and injected adapters. `exclude` means the item is application-specific
or has no Amber framework responsibility.

## Plans and suites

| Daryn input | Decision | Amber destination or reason |
| --- | --- | --- |
| `plan/123-VNV-059523` | migrate | Merge its generic validation role into `factory/plan/master`. |
| `suite/cli/options` | migrate | Merge generic browser/language option coverage into Amber's CLI suite; exclude Daryn username/password cases. |
| `suite/actions` | rewrite | Merge generic browser lifecycle, file, YAML, environment, timing, and output behavior into `web/primitives`. |
| `suite/page` | rewrite | Merge generic navigation, assertion, screenshot, download, PDF, OCR, and failure-evidence behavior into `web/primitives`. |

## Cases

| Daryn case | Decision | Migration treatment |
| --- | --- | --- |
| `cli/options/browser` | migrate | Merge with Amber's existing browser option case. |
| `cli/options/language` | migrate | Merge with Amber's existing language option case. |
| `cli/options/password` | exclude | Daryn/application credential option. |
| `cli/options/username` | exclude | Daryn/application credential option. |
| `concurrency` | rewrite | Use injected browser sessions and neutral local pages; no Daryn page objects. |
| `copy-download-file` | rewrite | Use Amber download evidence and case-scoped output. |
| `element-page-range` | rewrite | Use neutral HTML controls and generic assertions. |
| `find-file` | rewrite | Use a neutral local fixture and generic file assertion. |
| `id-text-search-altpath` | rewrite | Use generic element lookup and text assertion. |
| `id-text-search-default` | rewrite | Use generic element lookup and text assertion. |
| `initialize-browser` | rewrite | Use the YAML-selected browser adapter and lifecycle. |
| `message-log` | rewrite | Use Amber evidence/log output without Daryn logging helpers. |
| `multi-exception` | rewrite | Validate multiple failed web steps and retained evidence. |
| `open-pdf-externally` | rewrite | Use a neutral local PDF/download fixture. |
| `page-load-time` | rewrite | Use a neutral local page and generic timing evidence if retained by the contract. |
| `pdf-screenshots` | rewrite | Use generic PDF and screenshot actions. |
| `puts-with-translate` | rewrite | Replace Daryn output strategy with injected OCR/translation behavior. |
| `puts-without-translate` | rewrite | Validate generic text output without Daryn page helpers. |
| `raise-exception` | rewrite | Validate web-step failure handling and evidence retention. |
| `read-config` | rewrite | Read only neutral Amber fixture configuration. |
| `read-yaml` | rewrite | Read only neutral YAML fixture data. |
| `screenshot-case-name` | rewrite | Use Amber case-scoped screenshot evidence. |
| `screenshot-input-data` | rewrite | Use generic input actions against a local fixture. |
| `screenshot-page-name` | rewrite | Use Amber factory case paths, not Daryn page names. |
| `screenshot-scroll-multi` | rewrite | Implement only if scrolling is added to the generic web contract. |
| `screenshot-scroll-page` | rewrite | Implement only if scrolling is added to the generic web contract. |
| `screenshot-steps` | rewrite | Validate multiple screenshots without overwriting. |
| `screenshot-validate-changes` | rewrite | Use neutral local before/after page content. |
| `str-to-latex` | migrate | Preserve as generic LaTeX compatibility coverage. |
| `teleport` | exclude | Daryn-specific navigation syntax and destinations. |
| `test-env-info` | migrate | Use Amber's generic environment evidence. |
| `translate` | rewrite | Use injected, vendor-neutral OCR/translation with neutral local fixtures. |
| `write-to-file` | rewrite | Use generic command/file output behavior. |

## `factory/config` and related input data

| Daryn input | Decision | Reason or replacement |
| --- | --- | --- |
| `config/app_config.yaml` | exclude | Daryn application configuration. |
| `config/db_scripts/sqlversion.yaml` | exclude | Application/database-specific SQL. |
| `config/input/elementrangepage.yaml` | exclude | Daryn element IDs and application controls. Replace with neutral fixture data only for rewritten cases. |
| `config/input/pagegoogle.yaml` | exclude | Daryn/search-specific input. |
| `config/input/pagetask1.yaml` | exclude | Daryn page input. |
| `config/input/pagetask2.yaml` | exclude | Daryn page input. |
| `config/input/read_yaml_spec.yaml` | exclude | Product organization data. |
| `config/input/uicontrolspage.yaml` | exclude | Daryn element IDs and application controls. |
| `config/teleport.spn` | exclude | Daryn teleport command language and destinations. |
| `config/translations/cs.yaml` | exclude | VCORE application resources and requirements. |
| `config/translations/da.yaml` | exclude | VCORE application resources and requirements. |
| `config/translations/de.yaml` | exclude | VCORE application resources and requirements. |
| `config/translations/en.yaml` | exclude | VCORE application resources and requirements. |
| `config/translations/es.yaml` | exclude | VCORE application resources and requirements. |
| `config/translations/fr-ca.yaml` | exclude | VCORE application resources and requirements. |
| `config/translations/fr.yaml` | exclude | VCORE application resources and requirements. |
| `config/translations/it.yaml` | exclude | VCORE application resources and requirements. |
| `config/translations/ne.yaml` | exclude | VCORE application resources and requirements. |
| `config/translations/nl.yaml` | exclude | VCORE application resources and requirements. |
| `config/translations/no.yaml` | exclude | VCORE application resources and requirements. |
| `config/translations/pl.yaml` | exclude | VCORE application resources and requirements. |
| `config/translations/ro.yaml` | exclude | VCORE application resources and requirements. |
| `config/translations/sv.yaml` | exclude | VCORE application resources and requirements. |
| `config/user_input/pagegoogle.yaml` | exclude | Daryn/search-specific input. |

## Implementation order

1. Keep the matrix and report-directory behavior as the migration contract.
2. Add neutral local web fixtures and rewrite the generic cases in small
   groups under `factory/case/web/primitives`.
3. Merge each rewritten case into `factory/suite/web/primitives` and then into
   `factory/plan/master`.
4. Run Amber's own `--report-dir=report` validation after each group.

## Amber-native migration record

The generic Daryn intents are represented by application-neutral Amber cases.
The names below are Amber factory paths, not Daryn runtime dependencies.

| Generic intent | Amber case or coverage | Status |
| --- | --- | --- |
| Browser startup and teardown | `web/primitives/navigation` | migrated |
| Generic controls and element range | `web/primitives/controls` | migrated |
| Navigation and text assertions | `web/primitives/navigation` | migrated |
| Input and click interactions | `web/primitives/interactions` | migrated |
| Repeated screenshots and screenshot naming | per-action screenshots | migrated |
| Screenshot evidence scoped to a case/page | `web/primitives/evidence` | migrated |
| Download and copied-file evidence | `web/primitives/evidence` | migrated |
| PDF and screenshot evidence | `web/primitives/evidence` | migrated |
| YAML navigation and route input | `web/primitives/teleport` | migrated |
| OCR/translation validation | injected OCR browser specs | migrated |
| LaTeX string compatibility | existing LaTeX specs | migrated |
| Environment evidence | existing CLI environment case | migrated |

The following Daryn behavior remains intentionally excluded or outside the
passing master validation plan:

- Product credentials, translations, URLs, page objects, database/SQL data,
  SPN teleport syntax, and product workflows remain excluded as recorded
  above.
- Scroll-specific screenshot cases remain excluded until scrolling is part of
  the neutral web contract.
- Deliberate exception cases remain covered by focused failure specs rather
  than the passing master validation plan, so an expected failure cannot abort
  unrelated Amber self-validation.
