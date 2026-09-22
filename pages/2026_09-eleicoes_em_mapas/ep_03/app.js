(async () => {
  const status = document.getElementById("status");
  const config = window.MAP_CONFIG;
  const periodControls = document.getElementById("period-controls");
  const yearStart = document.getElementById("year-start");
  const yearEnd = document.getElementById("year-end");
  const periodSlider = document.getElementById("period-slider");
  const periodSelection = periodSlider.querySelector(".period-selection");
  const periodHelp = document.getElementById("period-help");
  let helpDismissed = false;

  function dismissHelp() {
    helpDismissed = true;
    periodHelp.hidden = true;
  }

  periodSlider.addEventListener("pointerdown", (event) => {
    if (!periodControls.disabled && event.button === 0) dismissHelp();
  });
  periodSlider.addEventListener("keydown", (event) => {
    if (!periodControls.disabled && ["ArrowLeft", "ArrowRight", "ArrowUp", "ArrowDown", "PageUp", "PageDown", "Home", "End", "Escape"].includes(event.key)) dismissHelp();
  });
  const period = config?.periodFilter;
  let periodLayers = [];
  const histogramBars = [];
  const areaFormatter = new Intl.NumberFormat("pt-BR", { maximumFractionDigits: 2 });

  function updatePeriodLabels() {
    document.getElementById("period-total-label").textContent = `Total no período entre ${yearStart.value} e ${yearEnd.value}`;
    const span = period.maxYear - period.minYear || 1;
    periodSlider.style.setProperty("--start", `${100 * (Number(yearStart.value) - period.minYear) / span}%`);
    periodSlider.style.setProperty("--end", `${100 * (Number(yearEnd.value) - period.minYear) / span}%`);
    periodSlider.classList.toggle("is-collapsed", yearStart.value === yearEnd.value);
    periodSlider.classList.toggle("is-full", Number(yearStart.value) === period.minYear && Number(yearEnd.value) === period.maxYear);
    let selectedTotal = 0;
    for (const { year, value, bar } of histogramBars) {
      const selected = year >= Number(yearStart.value) && year <= Number(yearEnd.value);
      bar.classList.toggle("is-selected", selected);
      if (selected) selectedTotal += value;
    }
    document.getElementById("period-total").value = histogramBars.length
      ? `${areaFormatter.format(selectedTotal)} ${config.histogram?.unit || ""}`.trim()
      : "—";
    for (const handle of [yearStart, yearEnd]) {
      handle.setAttribute("aria-valuenow", handle.value);
      handle.setAttribute("aria-valuetext", handle.value);
      handle.setAttribute("aria-valuemin", handle === yearStart ? period.minYear : yearStart.value);
      handle.setAttribute("aria-valuemax", handle === yearEnd ? period.maxYear : yearEnd.value);
    }
  }

  if (period) {
    const ticks = document.getElementById("period-ticks");
    const span = period.maxYear - period.minYear;
    const histogram = document.getElementById("period-histogram");
    const values = (config.histogram?.values || []).filter(({ year, value }) =>
      Number.isInteger(year) && year >= period.minYear && year <= period.maxYear && Number.isFinite(value) && value >= 0
    );
    const peak = Math.max(1, ...values.map(({ value }) => value));
    document.getElementById("histogram-caption").textContent = config.histogram?.label || "Nenhum total anual disponível";
    histogram.setAttribute("aria-label", `${config.histogram?.label || "Nenhum total anual disponível"}. ${values.map(({ year, value }) => `${year}: ${value} ${config.histogram?.unit || "km²"}`).join("; ")}`);
    periodSlider.style.setProperty("--bar-width", `${92 / (span || 1)}%`);
    periodSlider.style.setProperty("--bar-half-width", `${46 / (span || 1)}%`);
    for (const { year, value } of values) {
      const bar = document.createElement("span");
      bar.className = "histogram-bar";
      bar.style.left = `${100 * (year - period.minYear) / (span || 1)}%`;
      bar.style.height = `${value / peak * 100}%`;
      histogram.append(bar);
      histogramBars.push({ year, value, bar });
    }
    const middle = Math.round((period.minYear + period.maxYear) / 2);
    for (let year = period.minYear; year <= period.maxYear; year++) {
      const tick = document.createElement("span");
      tick.className = "period-tick";
      tick.style.left = `${100 * (year - period.minYear) / (span || 1)}%`;
      if (year === period.minYear || year === middle || year === period.maxYear) {
        tick.classList.add("major-tick");
        const label = document.createElement("span");
        label.textContent = year;
        tick.append(label);
      }
      ticks.append(tick);
    }
    yearStart.value = period.minYear;
    yearEnd.value = period.maxYear;
    updatePeriodLabels();
  }

  function showStatus(message) {
    status.textContent = message;
    status.hidden = false;
  }

  if (!config?.accessToken || !config.accessToken.startsWith("pk.")) {
    showStatus("Configure um token público (pk.*) do Mapbox em config.js para exibir o mapa.");
    return;
  }

  if (!window.mapboxgl) {
    showStatus("Não foi possível carregar a biblioteca do mapa. Verifique sua conexão e recarregue a página.");
    return;
  }

  if (!mapboxgl.supported()) {
    showStatus("Este navegador não oferece suporte aos recursos gráficos necessários para exibir o mapa.");
    return;
  }

  try {
    const panel = document.querySelector(".map-panel");
    const container = document.getElementById("map");
    let keepContentFitted = true;

    function positionHelp() {
      if (helpDismissed || periodControls.disabled) return;
      periodHelp.hidden = false;
      const rect = panel.getBoundingClientRect();
      const track = periodSelection.getBoundingClientRect();
      const beside = rect.right + 12 + periodHelp.offsetWidth <= container.clientWidth - 16;
      periodHelp.classList.toggle("below-panel", !beside);
      const left = beside ? rect.right + 12 : rect.left;
      const centerY = track.top + track.height / 2;
      const top = beside
        ? Math.max(16, Math.min(centerY - periodHelp.offsetHeight / 2, container.clientHeight - periodHelp.offsetHeight - 16))
        : rect.bottom + 12;
      periodHelp.style.left = `${left}px`;
      periodHelp.style.top = `${top}px`;
      periodHelp.style.setProperty("--arrow-y", `${centerY - top - 6}px`);
      periodHelp.style.setProperty("--arrow-x", `${track.left + track.width / 2 - left - 6}px`);
    }
    panel.addEventListener("scroll", positionHelp, { passive: true });

    function fitOptions() {
      const width = container.clientWidth;
      const height = container.clientHeight;
      const gap = Math.min(24, width / 10, height / 10);
      const panelRect = panel.getBoundingClientRect();
      // On narrow embeds reserve space above the map content instead of beside it.
      const padding = { top: gap, right: gap, bottom: gap, left: gap };
      if (width >= 640) {
        padding.left = panelRect.right + gap;
      } else {
        padding.top = Math.min(panelRect.bottom + gap, height * 0.6);
      }
      return { padding, maxZoom: config.maxFitZoom ?? 10, bearing: 0, pitch: 0 };
    }

    // Fetch the published style explicitly so recently replaced sources do not
    // remain stale in the browser's cached style response.
    let style = config.style;
    if (typeof style === "string" && style.startsWith("mapbox://styles/")) {
      const path = style.slice("mapbox://styles/".length);
      const url = new URL(`https://api.mapbox.com/styles/v1/${path}`);
      url.searchParams.set("access_token", config.accessToken);
      url.searchParams.set("fresh", "true");
      const response = await fetch(url, { cache: "no-store" });
      if (!response.ok) throw new Error(`Style request failed: ${response.status}`);
      style = await response.json();
    }

    const map = new mapboxgl.Map({
      container: "map",
      accessToken: config.accessToken,
      style,
      minZoom: config.minZoom ?? 4,
      bounds: config.bounds,
      fitBoundsOptions: fitOptions(),
      locale: {
        "NavigationControl.ZoomIn": "Aproximar",
        "NavigationControl.ZoomOut": "Afastar",
        "NavigationControl.ResetBearing": "Orientar para o norte",
      },
    });
    map.addControl(new mapboxgl.NavigationControl(), "top-right");

    function fitContent() {
      map.fitBounds(config.bounds, { ...fitOptions(), duration: 0, retainPadding: false });
      if (map.getZoom() < (config.minZoom ?? 4)) {
        map.setZoom(config.minZoom ?? 4);
      }
    }

    function syncHistogramColors() {
      // The current map uses a year-based match expression on the fill layer.
      // Read the style itself so the histogram and map share one color source.
      const layer = map.getStyle().layers.find((item) =>
        item.type === "fill" && item.source === period.source && item["source-layer"] === period.sourceLayer
      );
      const paint = layer?.paint?.["fill-color"];
      const isYearMatch = Array.isArray(paint) && paint[0] === "match"
        && paint[1]?.[0] === "get" && paint[1]?.[1] === period.property;
      const groups = new Map();
      const unstyledYears = new Set();
      for (const { year, bar } of histogramBars) {
        let color = typeof paint === "string" ? paint : isYearMatch ? paint[paint.length - 1] : "#7b8794";
        if (isYearMatch) {
          for (let i = 2; i < paint.length - 1; i += 2) {
            const years = Array.isArray(paint[i]) ? paint[i] : [paint[i]];
            if (years.includes(year)) { color = paint[i + 1]; break; }
          }
        }
        const definitions = config.histogram?.legendGroups || [];
        const groupIndex = definitions.findIndex((group) =>
          group.ranges?.some(([start, end]) => year >= start && year <= end)
        );
        color = definitions[groupIndex]?.color || color;
        if (typeof color !== "string") continue;
        // Keep supplied totals visible even when the map has no visible year color.
        if (color === "transparent" || /rgba\([^)]*,\s*0(?:\.0*)?\s*\)/.test(color)) {
          color = "#7b8794";
          unstyledYears.add(year);
        }
        bar.style.setProperty("--bar-color", color);
        const key = groupIndex >= 0 ? `group:${groupIndex}` : `color:${color}`;
        if (!groups.has(key)) groups.set(key, {
          years: [], colors: new Set(), title: definitions[groupIndex]?.title || "",
        });
        groups.get(key).years.push(year);
        groups.get(key).colors.add(color);
      }
      const legend = document.getElementById("histogram-legend");
      legend.replaceChildren();
      for (const { colors, years, title: configuredTitle } of groups.values()) {
        years.sort((a, b) => a - b);
        const ranges = [];
        let first = years[0];
        let last = first;
        for (const year of years.slice(1)) {
          if (year === last + 1) { last = year; continue; }
          ranges.push(first === last ? `${first}` : `${first}–${last}`);
          first = last = year;
        }
        ranges.push(first === last ? `${first}` : `${first}–${last}`);
        const item = document.createElement("li");
        for (const color of colors) {
          const swatch = document.createElement("span");
          swatch.className = "legend-swatch";
          swatch.style.backgroundColor = color;
          swatch.setAttribute("aria-hidden", "true");
          item.append(swatch);
        }
        const note = years.every((year) => unstyledYears.has(year)) ? " · sem cor visível no mapa" : "";
        const title = typeof configuredTitle === "string" ? configuredTitle.trim() : "";
        item.append(document.createTextNode((title || ranges.join(", ")) + note));
        if (title) item.title = ranges.join(", ");
        legend.append(item);
      }
    }

    function applyGroupColors() {
      const definitions = config.histogram?.legendGroups || [];
      if (!definitions.some((group) => group.color)) return;
      const year = ["to-number", ["get", period.property], -1];
      for (const layer of map.getStyle().layers) {
        if (layer.source !== period.source || layer["source-layer"] !== period.sourceLayer) continue;
        const property = { fill: "fill-color", line: "line-color", circle: "circle-color" }[layer.type];
        if (!property) continue;
        const fallback = layer.paint?.[property] ?? "#000000";
        const expression = ["case"];
        for (const group of definitions) {
          if (!group.ranges?.length) continue;
          const condition = ["any", ...group.ranges.map(([start, end]) =>
            ["all", [">=", year, start], ["<=", year, end]]
          )];
          expression.push(condition, group.color || fallback);
        }
        if (expression.length > 1) {
          expression.push(fallback);
          map.setPaintProperty(layer.id, property, expression);
        }
      }
    }

    let filterTimer = null;
    let lastAppliedPeriod = null;
    let filterInFlight = false;
    let lastFilterTime = -Infinity;
    const filterInterval = 100;

    function commitPeriod() {
      if (periodControls.disabled || !periodLayers.length) return;
      const key = `${yearStart.value}:${yearEnd.value}`;
      if (key === lastAppliedPeriod) return;
      // Keep at most one filter update in flight. The inputs hold the newest
      // selection, so intermediate drag positions never become a work queue.
      if (filterInFlight || filterTimer !== null) return;
      const delay = filterInterval - (performance.now() - lastFilterTime);
      if (delay > 0) {
        filterTimer = setTimeout(() => {
          filterTimer = null;
          commitPeriod();
        }, delay);
        return;
      }
      filterInFlight = true;
      lastFilterTime = performance.now();
      applyPeriodFilter();
      lastAppliedPeriod = key;
    }

    // Resume with the newest selection once Mapbox finishes rendering its work.
    map.on("idle", () => {
      filterInFlight = false;
      commitPeriod();
    });

    // Immediate brush feedback, with live map updates paced to rendering speed.
    function previewPeriod() {
      updatePeriodLabels();
      commitPeriod();
    }

    function applyPeriodFilter() {
      const year = ["to-number", ["get", period.property], -1];
      const filter = ["all",
        ["has", period.property],
        [">=", year, Number(yearStart.value)],
        ["<=", year, Number(yearEnd.value)],
      ];
      for (const layer of periodLayers) {
        map.setFilter(layer.id, layer.filter ? ["all", layer.filter, filter] : filter);
      }
    }

    let activePointer = null;

    function movePeriod(start, end, delta) {
      if (periodControls.disabled) return;
      const shift = Math.max(period.minYear - start, Math.min(period.maxYear - end, Math.round(delta)));
      yearStart.value = start + shift;
      yearEnd.value = end + shift;
      previewPeriod();
    }

    let rangeDrag = null;
    periodSelection.addEventListener("pointerdown", (event) => {
      if (periodControls.disabled || event.button !== 0 || activePointer !== null) return;
      const start = Number(yearStart.value);
      const end = Number(yearEnd.value);
      if (end - start === period.maxYear - period.minYear) return;
      event.preventDefault();
      activePointer = event.pointerId;
      rangeDrag = { x: event.clientX, start, end, width: periodSlider.getBoundingClientRect().width };
      periodSelection.setPointerCapture(event.pointerId);
      periodSlider.classList.add("is-dragging-range");
    });
    periodSelection.addEventListener("pointermove", (event) => {
      if (!rangeDrag || activePointer !== event.pointerId || rangeDrag.width <= 0) return;
      movePeriod(rangeDrag.start, rangeDrag.end,
        (event.clientX - rangeDrag.x) / rangeDrag.width * (period.maxYear - period.minYear));
    });
    for (const eventName of ["pointerup", "pointercancel", "lostpointercapture"]) {
      periodSelection.addEventListener(eventName, (event) => {
        if (activePointer !== event.pointerId) return;
        rangeDrag = null;
        activePointer = null;
        periodSlider.classList.remove("is-dragging-range");
        commitPeriod();
      });
    }

    for (const input of [yearStart, yearEnd]) {
      function setYear(value) {
        if (periodControls.disabled) return;
        const min = input === yearStart ? period.minYear : Number(yearStart.value);
        const max = input === yearEnd ? period.maxYear : Number(yearEnd.value);
        input.value = Math.max(min, Math.min(max, Math.round(value)));
        previewPeriod();
      }
      let drag = null;
      input.addEventListener("pointerdown", (event) => {
        if (periodControls.disabled || event.button !== 0 || activePointer !== null) return;
        activePointer = event.pointerId;
        input.focus();
        drag = { id: event.pointerId, x: event.clientX, year: Number(input.value) };
        input.setPointerCapture(event.pointerId);
      });
      input.addEventListener("pointermove", (event) => {
        if (!drag || drag.id !== event.pointerId) return;
        const width = periodSlider.getBoundingClientRect().width;
        if (width > 0) setYear(drag.year + (event.clientX - drag.x) / width * (period.maxYear - period.minYear));
      });
      for (const eventName of ["pointerup", "pointercancel", "lostpointercapture"]) {
        input.addEventListener(eventName, (event) => {
          if (activePointer !== event.pointerId) return;
          drag = null;
          activePointer = null;
          commitPeriod();
        });
      }
      input.addEventListener("keydown", (event) => {
        if (activePointer !== null) return;
        const steps = { ArrowLeft: -1, ArrowDown: -1, ArrowRight: 1, ArrowUp: 1, PageDown: -5, PageUp: 5 };
        if (event.key in steps) {
          event.preventDefault();
          if (event.shiftKey) movePeriod(Number(yearStart.value), Number(yearEnd.value), steps[event.key]);
          else setYear(Number(input.value) + steps[event.key]);
        } else if (event.key === "Home" || event.key === "End") {
          event.preventDefault();
          setYear(event.key === "Home" ? period.minYear : period.maxYear);
        }
      });
      input.addEventListener("blur", () => {
        if (activePointer === null) commitPeriod();
      });
    }

    // Preserve manual exploration when the iframe is resized.
    map.on("movestart", (event) => {
      if (event.originalEvent) keepContentFitted = false;
    });

    map.on("load", () => {
      status.hidden = true;
      periodLayers = (map.getStyle().layers || []).filter((layer) =>
        period && layer.source === period.source && layer["source-layer"] === period.sourceLayer
      ).map((layer) => ({ id: layer.id, filter: map.getFilter(layer.id) }));
      if (periodLayers.length) {
        syncHistogramColors();
        applyGroupColors();
        periodControls.disabled = false;
        positionHelp();
        commitPeriod();
      } else {
        const available = [...new Set((map.getStyle().layers || [])
          .filter((layer) => layer["source-layer"])
          .map((layer) => `${layer.source} / ${layer["source-layer"]}`))];
        console.error("Period layer mismatch", { expected: period, available });
        showStatus(`A camada ${period?.sourceLayer || "não configurada"} não foi encontrada na fonte ${period?.source || "não configurada"} do estilo carregado. Confira a configuração e a publicação do estilo.`);
      }
      // Clear the constructor's persistent padding before fitting again.
      map.setPadding(0);
      if (keepContentFitted) fitContent();
    });
    map.on("error", (event) => {
      console.error("Mapbox error:", event.error);
      showStatus("Não foi possível carregar parte do mapa. Verifique sua conexão e o token do Mapbox e recarregue a página.");
    });

    // Also handles container size changes when embedded in a responsive iframe.
    const resizeObserver = new ResizeObserver(() => {
      map.resize();
      positionHelp();
      if (keepContentFitted) fitContent();
    });
    resizeObserver.observe(container);
    resizeObserver.observe(panel);
    map.on("remove", () => {
      clearTimeout(filterTimer);
      resizeObserver.disconnect();
    });
  } catch (error) {
    periodControls.disabled = true;
    console.error("Map initialization failed:", error);
    showStatus("Não foi possível iniciar o mapa. Verifique a configuração do Mapbox e recarregue a página.");
  }
})();
