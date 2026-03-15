# Development

### Enable project settings suitable for local development

-> create a file:
```haskell
--- ./cabal.project.local ---
import: misc/dev.project
```

If a cabal.project.local is present it can be ignored with:
```sh
cabal --project-file=cabal.project.no-local ..
```
