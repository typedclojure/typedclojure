# Testing Guidelines for AI Agents

## Iterating on Unit Tests with Error Messages and Column Numbers

When working on tests that check exact error messages including line/column numbers (e.g., tests using `is-tc-err-messages`), use this efficient workflow:

### Setup
1. **Start a file watcher** in the background that automatically runs tests when files change:
   ```bash
   clojure -M:test:kaocha:spec-skip-macros -m kaocha.runner --watch --focus <test-namespace>
   ```
   Example:
   ```bash
   clojure -M:test:kaocha:spec-skip-macros -m kaocha.runner --watch --focus typed-test.clj.checker.path-type-test
   ```

2. **Redirect watcher output** to a log file for easy inspection:
   ```bash
   clojure -M:test:kaocha:spec-skip-macros -m kaocha.runner --watch --focus <test-namespace> 2>&1 | tee /tmp/watcher.log
   ```

### Workflow

1. **Capture error formats first**: Create a temporary test file that captures the actual error output for all test cases:
   ```clojure
   (deftest capture-errors-test
     (testing "Error case 1"
       (let [result (is-tc-err-messages <test-code>)]
         (println "\n===== TEST 1 =====")
         (clojure.pprint/pprint result)))
     ;; Add more test cases...
     (is (= 1 2)))  ; Force failure to see output
   ```

2. **Run once to capture** the error structures and column numbers.

3. **Update tests incrementally**: With the watcher running, update test assertions one at a time with the correct error structures.

4. **Monitor watcher output**: After each change, the watcher automatically re-runs tests. Check `/tmp/watcher.log` or the terminal to see:
   - Which tests pass
   - Which tests fail with specific differences (e.g., column number mismatches)
   - The exact format of actual vs expected

5. **Iterate**: Adjust column numbers and error messages based on watcher feedback until all tests pass.

### Key Points

- **Column numbers vary** based on the indentation and formatting of the actual test code (not the temporary capture script)
- The watcher provides **immediate feedback** without manual test execution
- **Watch the diff output** - kaocha shows you exactly what's different (e.g., `column -28 +35`)
- **Error messages must match exactly** including newlines (`\n`) and tabs (`\t`)
- **Type representations** in errors may be simplified (e.g., `String` instead of `t/Str`, `t/Any` instead of `(t/U nil t/Any)`)

### Example Error Structure

```clojure
'{:type-errors
  [{:type-error :typed.clojure/type-mismatch-error,
    :env {:line "REMOVED_LINE", :column 30, :file "test_file.clj"},
    :form (:access_token res),
    :data {:expected-type nil, :actual-type String},
    :message "Type mismatch:\n\nExpected: \tnil\n\nActual: \tString"}]}
```

### Troubleshooting

- If column numbers don't match, check the actual indentation in your test file
- If types don't match (e.g., `String` vs `t/Str`), use what the error actually reports
- If tests intermittently fail, ensure the watcher has finished running before checking results
- Always verify the final error messages make semantic sense for the test case

### Cleanup

- Remove temporary capture test files after extracting error formats
- Stop the watcher when done: `Ctrl+C` or use `stop_bash` tool
