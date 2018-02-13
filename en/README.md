### Language Conventions
The amber driver supports languages with the following strict convention.

en/
  factory/
    plan/
      aplan/
        aplan.yaml
    suite/
      asuite/
        asuite.yaml
    case
      acase/
        acase.yaml
fr/
  factory/
    plan/
      aplan/
        aplan.yaml
    suite/
      asuite/
        asuite.yaml
    case
      acase/
        acase.yaml
gr/
  factory/
    plan/
      aplan/
        aplan.yaml
    suite/
      asuite/
        asuite.yaml
    case
      acase/
        acase.yaml

### Examples
```bash
amber --language=fr --case=acase

amber --language=gr --plan=aplan

amber --langauge=en --suite=asuite
```
