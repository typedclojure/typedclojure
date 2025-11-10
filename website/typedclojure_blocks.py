import os
import re
import subprocess

def on_page_markdown(markdown, page, config, files):
    """
    MkDocs hook: on_page_markdown

    Replaces type-checked clojure code blocks matched with doc-test metadata
    comments with rendered HTML, using an external babashka script.

    For every block:
        <!-- doc-test: id=<id> ... -->
        ```clojure
        ...
        ```
      Calls: (in parent dir) bb script/render-clojure-block.clj <markdown-file-path> <doc-test-id>
      and uses its output as the annotated HTML for that block.

    See plugins/typedclojure_blocks.md for documentation and transformation expectations.
    """
    page_path = page.file.abs_src_path if hasattr(page.file, "abs_src_path") else None
    if not page_path or not os.path.exists(page_path):
        return markdown

    # Location from parent directory
    script_rel_path = os.path.join('script', 'render-clojure-block.clj')

    # Regex as before
    PATTERN = re.compile(
        r"<!--\s*doc-test:.*?id=([a-f0-9]{8}).*?-->\s*```clojure\s*([\s\S]*?)```",
        re.IGNORECASE,
    )

    def block_replacer(match):
        doc_test_id = match.group(1)
        code_block_content = match.group(2)
        code_block_content = code_block_content.lstrip('\n')
        # Prepare to run bb from project root (..)
        project_root = os.path.abspath(os.path.join(os.getcwd(), ".."))
        bb_command = [
            "bb",
            script_rel_path,
            page_path,
            doc_test_id,
        ]
        try:
            bb_result = subprocess.run(
                bb_command,
                check=True,
                capture_output=True,
                text=True,
                cwd=project_root  # change working directory!
            )
            html_output = bb_result.stdout.strip()
        except subprocess.CalledProcessError as e:
            return (
                f'<pre><code>{code_block_content}</code></pre>\n'
                f'<div style="color: red;">'
                f"BABASHKA RENDER ERROR for block id {doc_test_id}:<br>"
                f"{e.stderr}</div>"
            )
        return html_output

    new_markdown = PATTERN.sub(block_replacer, markdown)
    return new_markdown
