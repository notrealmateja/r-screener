#!/usr/bin/env python3
"""Static check for the Python embedded in .github/workflows/*.yml.

Companion to lint_workflow_r.R, which covers the inline `Rscript -e '...'`
blocks. The failure-reporting steps build their scripts as shell heredocs, and
nothing executed them until a run actually failed — so a syntax error there
would sit undetected until the exact moment the pipeline needed to tell someone
it was broken. The alert would then fail silently, because the step ends in
`|| true; exit 0` so that reporting a failure can never itself fail the job.

That is the worst possible place for an unchecked block, and it is checked here.

Reproduces how bash sees the heredoc: YAML strips the `run: |` block scalar's
common indentation, so the body is dedented by the `cat` line's own indent.
"""

import io
import os
import re
import sys

HEREDOC = re.compile(r"^(?P<indent>\s*)cat > (?P<path>\S+) <<'(?P<tag>\w+)'\s*$")


def blocks(path):
    """Yield (name, start_line, source) for each quoted Python heredoc."""
    lines = io.open(path, encoding="utf-8").read().split("\n")
    i = 0
    while i < len(lines):
        m = HEREDOC.match(lines[i])
        if not m or not m.group("path").endswith(".py"):
            i += 1
            continue
        indent, tag = m.group("indent"), m.group("tag")
        body, j = [], i + 1
        while j < len(lines) and lines[j].strip() != tag:
            line = lines[j]
            body.append(line[len(indent):] if line.startswith(indent) else line)
            j += 1
        if j >= len(lines):
            yield (m.group("path"), i + 1, None)   # unterminated
        else:
            yield (m.group("path"), i + 1, "\n".join(body))
        i = j + 1


def main(argv):
    wf_dir = argv[1] if len(argv) > 1 else ".github/workflows"
    if not os.path.isdir(wf_dir):
        print("No workflow directory at %s" % wf_dir)
        return 0

    files = sorted(f for f in os.listdir(wf_dir) if f.endswith((".yml", ".yaml")))
    if not files:
        print("No workflow files found in %s" % wf_dir)
        return 0

    problems = 0
    for f in files:
        full = os.path.join(wf_dir, f)
        for name, line, src in blocks(full):
            label = "%s:%d (%s)" % (full, line, name)
            if src is None:
                print("FAIL %s\n  unterminated heredoc (no closing tag)" % label)
                problems += 1
                continue
            try:
                compile(src, name, "exec")
            except SyntaxError as e:
                print("FAIL %s\n  %s on heredoc line %s: %s"
                      % (label, e.msg, e.lineno, (e.text or "").rstrip()))
                problems += 1
            else:
                print("ok   %s (%d lines)" % (label, len(src.split("\n"))))

    if problems:
        print("\n%d embedded Python block(s) failed the check." % problems)
        return 1
    print("\nAll embedded Python blocks passed.")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
