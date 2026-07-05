# Releases

To release a new version of typedclojure, run the following script
and follow the instructions:

```
./script/prep-actions-release.sh
```

Then push the resulting commit to `typedclojure/typedclojure` on the
`main` branch by sending a PR and preserving the commit message exactly
in the squashed commit of the merged PR.

GitHub Actions will automatically deploy the version specified in the
file `stable-version` at the root of repo to Clojars.
