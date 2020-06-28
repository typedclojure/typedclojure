# Releases

To release a new version of typedclojure, run the following script
and follow the instructions:

```
./script/prep-actions-release.sh
```

Then push the resulting commit to `typedclojure/typedclojure` on the
`main` branch.

GitHub Actions will automatically deploy the release to Clojars and
bump the pom.xml version and README.md versions.
