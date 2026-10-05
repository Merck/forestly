// forestly interactive widgets for the main AE forest `lt` table.
//
// Filtering (AE criteria, incidence) is now done by lt's own typed filters in
// the table's control bar, so the only forestly-owned control left is the CSV
// download button. It reads the table's current filtered view through lt's
// public contract (`el._lt.view()` and `el._ltSpec`) -- no crosstalk, no
// globals, everything wired inside a per-container closure.

(() => {
  // Resolve the lt controller once it exists. lt mounts on load; a widget script
  // may run first, so poll a few animation frames rather than assume ordering.
  function whenReady(root, cb) {
    const el = root.querySelector(".lt-table");
    if (el && el._lt) return cb(el);
    requestAnimationFrame(() => whenReady(root, cb));
  }

  // CSV download: the visible columns of every row in the table's current view
  // (what the lt filters keep, across all pages), using the displayed text.
  function wireDownload(root, el) {
    const btn = root.querySelector(".forestly-download");
    if (!btn) return;
    btn.addEventListener("click", () => {
      const spec = el._ltSpec || {};
      const cols = spec._cols || [];
      const disp = spec._display || spec.data || {};
      const esc = (v) => {
        const s = v == null ? "" : String(v);
        return /[",\n]/.test(s) ? '"' + s.replace(/"/g, '""') + '"' : s;
      };
      const lines = [cols.map(esc).join(",")];
      for (const r of el._lt.view()) // 1-based row indices
        lines.push(cols.map((c) => esc((disp[c] || [])[r - 1])).join(","));
      const blob = new Blob([lines.join("\n")], { type: "text/csv" });
      const a = document.createElement("a");
      a.href = URL.createObjectURL(blob);
      a.download = "ae-forest.csv";
      a.click();
      URL.revokeObjectURL(a.href);
    });
  }

  function init(root) {
    if (root.dataset.forestlyOn) return;
    root.dataset.forestlyOn = "1";
    whenReady(root, (el) => wireDownload(root, el));
  }

  const scan = () =>
    document.querySelectorAll(".forestly-ae").forEach(init);

  if (document.readyState === "loading")
    document.addEventListener("DOMContentLoaded", scan);
  else scan();
})();
