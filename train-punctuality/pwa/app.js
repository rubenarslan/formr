/* Zug PWA: polls /api/options, renders ranked option cards + calibration tab. */

const REFRESH_MS = 60000;
let currentDay = "auto";
let currentTab = "options";
let lastPayload = null;
let timer = null;

const $ = (sel) => document.querySelector(sel);

function fmtTime(iso) {
  if (!iso) return "–";
  return new Date(iso).toLocaleTimeString("de-DE", { hour: "2-digit", minute: "2-digit" });
}

function fmtDelay(min) {
  if (min === null || min === undefined) return "–";
  return (min >= 0 ? "+" : "") + min + "'";
}

function delayed(iso, min) {
  if (!iso || min === null || min === undefined) return "–";
  return new Date(new Date(iso).getTime() + min * 60000).toLocaleTimeString("de-DE", {
    hour: "2-digit",
    minute: "2-digit",
  });
}

function desyncBadge(vendo, iris, threshold) {
  if (iris === null || iris === undefined || Math.abs(iris - vendo) < threshold) return "";
  return `<span class="desync" title="bahn.de vs. Anzeigetafel (IRIS)">bahn.de ${fmtDelay(vendo)} / Tafel ${fmtDelay(iris)}</span>`;
}

function legRow(leg, threshold) {
  const cancelled = leg.cancelled ? " cancelled" : "";
  return `<div class="leg${cancelled}">
    <span class="line">${leg.line || "?"}</span>
    <span>${fmtTime(leg.dep.planned)} ${leg.dep.station} (Gl. ${leg.dep.platform || "?"})
      → ${fmtTime(leg.arr.planned)} ${leg.arr.station}</span>
    <span class="delays">${fmtDelay(leg.arr.iris_delay_min ?? leg.arr.vendo_delay_min)}
      ${desyncBadge(leg.arr.vendo_delay_min, leg.arr.iris_delay_min, threshold)}</span>
    ${leg.cancelled ? '<span class="cancel-flag">AUSFALL</span>' : ""}
  </div>`;
}

function card(opt, threshold) {
  const m = opt.model;
  const modelBlock = m
    ? `<div class="model">
        <div class="est">
          <span class="big">${delayed(opt.scheduled_arr, m.eff_q50)}</span>
          <span class="label">realistisch (Median)</span>
        </div>
        <div class="est">
          <span class="big">${delayed(opt.scheduled_arr, m.eff_q80)}</span>
          <span class="label">80&nbsp;%-sicher${m.tail_open ? "≥" : ""}</span>
        </div>
        <div class="probs">
          Anschluss verpasst: ${(m.p_miss * 100).toFixed(0)} % ·
          Ausfall: ${(m.p_cancel * 100).toFixed(1)} %
        </div>
      </div>`
    : `<div class="model model-missing">Modell nicht erreichbar — nur DB-Prognose</div>`;

  return `<article class="card badge-${opt.badge}${opt.cancelled_now ? " cancelled" : ""}">
    <div class="head">
      <span class="dep">${fmtTime(opt.scheduled_dep)}</span>
      <span class="arrow">→</span>
      <span class="arr">${fmtTime(opt.scheduled_arr)}</span>
      <span class="db">DB: ${delayed(opt.scheduled_arr, opt.db_forecast_arr_delay_min)}
        (${fmtDelay(opt.db_forecast_arr_delay_min)})</span>
      <span class="badge">${opt.cancelled_now ? "AUSFALL" : opt.badge}</span>
    </div>
    ${modelBlock}
    <details><summary>${opt.legs.length} Abschnitt(e)</summary>
      ${opt.legs.map((l) => legRow(l, threshold)).join("")}
    </details>
  </article>`;
}

async function loadOptions() {
  const status = $("#status");
  try {
    const r = await fetch(`/api/options?day=${currentDay}`, { cache: "no-store" });
    if (!r.ok) throw new Error(`HTTP ${r.status}`);
    lastPayload = await r.json();
    status.textContent = "";
    render();
  } catch (e) {
    status.textContent = `⚠ ${e.message} — zeige letzte Daten`;
    status.className = "stale";
  }
}

function render() {
  if (!lastPayload) return;
  const t = lastPayload.desync_threshold_min ?? 2;
  $("#options").innerHTML =
    (lastPayload.options || []).map((o) => card(o, t)).join("") ||
    "<p>Keine Verbindungen gefunden.</p>";
  $("#updated").textContent =
    "Stand: " + new Date(lastPayload.generated_at).toLocaleTimeString("de-DE");
}

async function loadCalibration() {
  const el = $("#calibration");
  el.innerHTML = "<p>Lade…</p>";
  try {
    const r = await fetch("/api/calibration", { cache: "no-store" });
    const c = await r.json();
    let html = `<h2>Kalibrierung (n=${c.n_resolved})</h2>`;
    for (const [src, v] of Object.entries(c.db_bias || {})) {
      html += `<p><b>${src === "vendo" ? "bahn.de" : "IRIS/Tafel"}</b>:
        real − Prognose = ${v.mean_underestimate_min} min im Schnitt;
        ${(v.p_worse_than_forecast * 100).toFixed(0)} % schlechter als prognostiziert (n=${v.n})</p>`;
    }
    for (const [bucket, entry] of Object.entries(c.buckets || {})) {
      html += `<h3>Horizont ${bucket} min (n=${entry.n})</h3><div class="bars">`;
      for (const [q, cov] of Object.entries(entry.coverage || {})) {
        const nominal = parseInt(q.slice(1), 10);
        html += `<div class="bar-row"><span>${q}</span>
          <div class="bar"><div class="fill" style="width:${cov * 100}%"></div>
          <div class="nominal" style="left:${nominal}%"></div></div>
          <span>${(cov * 100).toFixed(0)} % (Soll ${nominal} %)</span></div>`;
      }
      html += "</div>";
    }
    if (c.cancellation) {
      html += `<p>Ausfälle: beobachtet ${(c.cancellation.observed_rate * 100).toFixed(1)} %,
        vorhergesagt ${(c.cancellation.mean_predicted * 100).toFixed(1)} % (n=${c.cancellation.n})</p>`;
    }
    if (!c.n_resolved) html += "<p>Noch keine aufgelösten Fahrten — nach den ersten Reisetagen gibt es hier Daten.</p>";
    el.innerHTML = html;
  } catch (e) {
    el.innerHTML = `<p>⚠ ${e.message}</p>`;
  }
}

function switchTab(tab) {
  currentTab = tab;
  $("#options").hidden = tab !== "options";
  $("#calibration").hidden = tab !== "calibration";
  document.querySelectorAll("#tab-toggle button").forEach((b) =>
    b.classList.toggle("active", b.dataset.tab === tab)
  );
  if (tab === "calibration") loadCalibration();
}

document.querySelectorAll("#day-toggle button").forEach((b) =>
  b.addEventListener("click", () => {
    currentDay = b.dataset.day;
    document.querySelectorAll("#day-toggle button").forEach((x) =>
      x.classList.toggle("active", x === b)
    );
    loadOptions();
  })
);
document.querySelectorAll("#tab-toggle button").forEach((b) =>
  b.addEventListener("click", () => switchTab(b.dataset.tab))
);

document.addEventListener("visibilitychange", () => {
  if (!document.hidden) loadOptions();
});

loadOptions();
timer = setInterval(() => {
  if (!document.hidden && currentTab === "options") loadOptions();
}, REFRESH_MS);
