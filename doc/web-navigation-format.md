# Amber Neutral Web Navigation Format

Amber web navigation is represented as YAML data. The format describes named
destinations and ordered web actions. It does not contain Ruby, Watir,
Selenium, application page objects, or executable command strings.

The consuming application owns the navigation file beneath its report factory:

```text
report/factory/config/web/navigation.yaml
```

Amber resolves the file relative to `--report-dir`, validates its structure,
and passes the selected route to an injected navigation adapter.

## Format

```yaml
navigation:
  version: 1
  routes:
    - from: start
      to: search
      steps:
        - action: navigate
          target: fixture://web/page_mock.html
        - action: input
          target: search
          parameters:
            value: ${input.query}
        - action: click
          target: submit
```

`from` and `to` are logical destination names. They have no application
meaning to Amber. `steps` contains the same neutral actions supported by the
web adapter: `navigate`, `click`, `input`, `assert`, `screenshot`, `download`,
`pdf`, and `ocr`.

## Teleport case step

A test case selects a route with a web step:

```yaml
web:
  browser: Chrome
  navigation_file: config/web/navigation.yaml
  input_root: config/input

steps:
  - type: web
    action: teleport
    target: search
    parameters:
      from: start
      input: search.yaml
```

The `teleport` action means “execute the route from the named source to the
named destination.” Amber validates the route and preserves step ordering. The
injected navigation adapter owns destination resolution, application page
objects, credentials, and any application-specific behavior.

## Input files

Input files are application-owned YAML data beneath the report factory. Amber
resolves them below `input_root`, rejects paths that escape that root, and
passes the parsed data to the adapter. Amber does not assign meaning to keys
such as `query`, `controls`, or `page`.

```yaml
query: Amber framework
```

The `${input.key}` notation is reserved for adapter-side substitution in route
steps. Amber must not require a particular input schema.

## Case-local web input

A web case may define input values directly beneath its `web` mapping. The
mapping key is the target control id used by an `input` step:

```yaml
web:
  browser: Chrome
  input:
    start-date: '2026-09-20'
    end-date: '2026-09-21'

steps:
  - type: web
    action: input
    target: start-date
  - type: web
    action: input
    target: end-date
```

Amber resolves the value before browser execution. An explicit
`parameters.value` takes precedence when a step needs to override the
case-local value. Every case-local input target must identify a control id;
an input step without a matching value is rejected before the browser starts.

To enter all case-local values as one reported action, use `fill` with a
logical form target. The values are still applied by their control ids:

```yaml
- type: web
  action: fill
  target: search-form
```

`fill` is equivalent to one input operation for each key in `web.input` and
does not require a screenshot after every individual control.

## Validation rules

- The top-level key must be `navigation`.
- `navigation.version` must be supported; version `1` is the initial format.
- Every route requires `from`, `to`, and a non-empty `steps` array.
- Each route step requires an `action`.
- `from`/`to` names are logical strings, not URLs or filesystem paths.
- `navigation_file` and `input` paths are relative to the report factory.
- Absolute paths and `..` traversal are rejected.
- Amber reports malformed routes before starting a browser.
- Application-specific execution is supplied through an injected adapter.

## Paperboy usage

Paperboy can implement a navigation adapter for its own pages while retaining
the same factory structure:

```text
Paperboy/report/factory/config/web/navigation.yaml
Paperboy/report/factory/config/input/*.yaml
Paperboy/report/factory/config/web/*.html
```

The Amber validation report uses the same contract with local neutral fixtures,
so the Amber repository remains the working example for downstream projects.
