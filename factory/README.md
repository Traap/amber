## Conventions
The amber driver consumes yaml files with the following strict convention.
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
amber --case=acase

amber --plan=aplan

amber --suite=asuite
```
      
