// Use a public Mapbox token (pk.*), never a secret token.
// Because this file is public, restrict the token in Mapbox to the production
// origin (https://infoamazonia.github.io) and any explicitly needed dev origins.
window.MAP_CONFIG = {
  accessToken: "pk.eyJ1IjoiaW5mb2FtYXpvbmlhIiwiYSI6ImNtcXBjMzAzMTA2bXoycG40Nmpjc29raHgifQ.nc4_MfOWZPLoeBdSveu36w",
  style: "mapbox://styles/infoamazonia/cmu32vp7h001m01qzc7qp6gdn",
  // Legal Amazon extent from the style's infoamazonia.86ir08a9 tileset.
  // Fit this region rather than the global basemap or the surrounding mask.
  bounds: [[-73.983182, -18.041561], [-43.951919, 5.269581]],
  maxFitZoom: 10,
  minZoom: 4, // Keep navigation at zoom levels where the data draws.
  // Annual area values supplied for this project.
  histogram: {
    label: " ",
    unit: "km²",
    // Inclusive year ranges; a group can include multiple periods.
    // Empty titles display the years. If ranges overlap, the first group wins.
    legendGroups: [
      { title: "Governo Lula", color: "#9a3548", ranges: [[2003, 2010], [2023, 2026]] },
      { title: "Governo Dilma", color: "#d48c99", ranges: [[2011, 2016]] },
      { title: "Governo Temer", color: "#ab9548", ranges: [[2017, 2018]] },
      { title: "Governo Bolsonaro", color: "#21536d", ranges: [[2019, 2022]] },
    ],
    values: [
      { year: 2003, value: 25396 },
      { year: 2004, value: 27772 },
      { year: 2005, value: 19014 },
      { year: 2006, value: 14286 },
      { year: 2007, value: 11651 },
      { year: 2008, value: 12911 },
      { year: 2009, value: 7464 },
      { year: 2010, value: 7000 },
      { year: 2011, value: 6418 },
      { year: 2012, value: 4571 },
      { year: 2013, value: 5891 },
      { year: 2014, value: 5012 },
      { year: 2015, value: 6207 },
      { year: 2016, value: 7893 },
      { year: 2017, value: 6947 },
      { year: 2018, value: 7536 },
      { year: 2019, value: 10129 },
      { year: 2020, value: 10851 },
      { year: 2021, value: 13038 },
      { year: 2022, value: 11594 },
      { year: 2023, value: 9064 },
      { year: 2024, value: 6518 },
      { year: 2025, value: 5731 },
      { year: 2026, value: 4294 },
    ],
  },
  periodFilter: {
    source: "composite",
    // Use the tileset's source-layer name, not the style layer's id.
    sourceLayer: "prodes_deter_desmatamento_20032026",
    property: "year",
    minYear: 2003,
    maxYear: 2026,
  },
};
