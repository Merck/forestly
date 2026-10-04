// forestly interactive widgets for the main AE forest `lt` table.
//
// Each `.forestly-ae` container holds one lt table plus forestly-owned controls
// (parameter dropdown, incidence range slider, CSV download). The controls drive
// the table purely through lt's external contract: `el._lt.filter(id, fn)` to
// register row predicates and `el._ltSpec.data` to read raw column values. No
// crosstalk, no globals -- everything is wired inside a per-container closure.

(() => {
  // Resolve the lt controller once it exists. lt mounts on load; a widget script
  // may run first, so poll a few animation frames rather than assume ordering.
  function whenReady(root, cb) {
    const el = root.querySelector(".lt-table");
    if (el && el._lt) return cb(el);
    requestAnimationFrame(() => whenReady(root, cb));
  }

  // Parameter dropdown: keep only rows of the selected analysis. The stored
  // value lives in the hidden `parameter` column.
  function wireParam(root, lt) {
    const sel = root.querySelector(".forestly-param select");
    if (!sel) return;
    const apply = () => {
      const v = sel.value;
      lt.filter("param", (row) => String(row.parameter) === v);
    };
    sel.addEventListener("change", apply);
    apply(); // default to the first option
  }

  // Incidence range slider: two overlaid range inputs form one dual-thumb track.
  // Keeps rows whose row-max incidence (`col`) falls within [lo, hi].
  function wireSlider(root, lt) {
    const box = root.querySelector(".forestly-slider");
    if (!box) return;
    const col = box.dataset.col;
    const lo = box.querySelector("input.lo");
    const hi = box.querySelector("input.hi");
    const loOut = box.querySelector(".lo-out");
    const hiOut = box.querySelector(".hi-out");
    const apply = () => {
      let a = +lo.value, b = +hi.value;
      if (a > b) { const t = a; a = b; b = t; } // thumbs may cross
      loOut.textContent = a;
      hiOut.textContent = b;
      lt.filter("incidence", (row) => {
        const x = +row[col];
        return x >= a && x <= b;
      });
    };
    lo.addEventListener("input", apply);
    hi.addEventListener("input", apply);
    apply();
  }

  // CSV download: the visible columns of every row kept by the forestly filters
  // (parameter + incidence), using the table's displayed text.
  function wireDownload(root, el, lt) {
    const btn = root.querySelector(".forestly-download");
    if (!btn) return;
    btn.addEventListener("click", () => {
      const spec = el._ltSpec || {};
      const cols = spec._cols || [];
      const disp = spec._display || spec.data || {};
      const n = (spec.data && cols.length) ? (spec.data[cols[0]] || []).length : 0;
      const preds = Object.values((lt.state && lt.state.predicates) || {});
      const esc = (v) => {
        const s = v == null ? "" : String(v);
        return /[",\n]/.test(s) ? '"' + s.replace(/"/g, '""') + '"' : s;
      };
      const lines = [cols.map(esc).join(",")];
      for (let r = 0; r < n; r++) {
        const raw = {};
        for (const c in spec.data) raw[c] = spec.data[c][r];
        if (preds.length && !preds.every((p) => p(raw))) continue;
        lines.push(cols.map((c) => esc((disp[c] || [])[r])).join(","));
      }
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
    whenReady(root, (el) => {
      wireParam(root, el._lt);
      wireSlider(root, el._lt);
      wireDownload(root, el, el._lt);
    });
  }

  const scan = () =>
    document.querySelectorAll(".forestly-ae").forEach(init);

  if (document.readyState === "loading")
    document.addEventListener("DOMContentLoaded", scan);
  else scan();
})();
