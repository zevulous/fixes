# Philippine National Road Grade and Terrain Slope Dataset

**Version 1.0 — September 2026**

A national terrain-following road grade layer covering 194,101 km of the
Philippine road network, plus a national bare-earth slope raster, built
entirely from open data and externally validated against satellite laser
altimetry.

---

## 0. Read this first: what this dataset is and is not

**This is a screening product. It is not engineering-grade and must not be
used for road design.**

The distinction matters enough to state precisely. Road grade, in the sense
a civil engineer uses the term, is the longitudinal slope of a constructed
running surface, established by a surveyed centerline profile with a known
instrument accuracy chain and control network. That measurement carries
professional liability; someone signs it.

What this dataset contains is different. It is the slope of the *terrain
surface* along the mapped centerline of a road, sampled from a 30-metre
digital elevation model. The roadway itself is narrower than a single pixel
of that model — a two-lane Philippine national road is about 7 m wide, a
barangay road narrower still — so no pixel in the source DEM sees only road.
Every elevation sample is an average of the carriageway together with its
shoulders, ditches, cut faces, embankment slopes, and adjacent vegetation.

The practical consequence is a systematic upward bias in steep terrain. Roads
are engineered precisely to *avoid* following raw terrain: cuts flatten
climbs, fills carry roads across dips, embankments and retaining structures
hold alignment across side slopes, and bridges and tunnels bypass terrain
entirely. None of that engineering is resolvable at 30 m. Where a road was
cut into a hillside, this dataset reports something closer to the hillside.

The dataset is nonetheless useful, and quantifiably so, because the bias is
not uniform. It is near zero on the gentle terrain that carries most of the
network's kilometres, and large only in the steep narrow-valley terrain that
is a small fraction of it. The reliability stratification in Section 6 tells
you which is which, and Section 7 gives external evidence that the
stratification works.

Appropriate uses: corridor screening and prioritisation, landslide and
hazard exposure of the road network, accessibility and travel-cost modelling,
energy and emissions estimation for freight routing, identifying candidates
for ground survey, regional and national descriptive statistics, and any
analysis where a well-characterised approximation with stated uncertainty is
more valuable than no data at all.

Inappropriate uses: road design or reconstruction, geometric compliance
assessment, any regulatory or contractual determination, per-segment claims
about a specific road without independent verification, and anything where a
several-percent error on an individual segment would matter.

---

## 1. Why this dataset exists

There is no openly available road grade data for the Philippines.

This is not for want of the underlying measurements. They exist, in three
places, none of them open:

**NAMRIA holds a nationwide 5 m IfSAR digital terrain model.** Intermap flew
the entire archipelago — roughly 300,000 km² — in 89 days in 2013, delivering
5 m post spacing at about 2.0 m horizontal and 1 m vertical RMSE. This is the
dataset built for exactly this purpose, and at 5 m posts a 7 m carriageway is
at least resolvable rather than invisible. It is a paid frontline service.
Freedom of Information requests for it are declined as falling outside the
FOI framework; the documented route is a formal letter to the Administrator,
in practice an institutional memorandum of agreement.

**DPWH holds as-built plans and surveyed vertical alignments** for the
national road network, roughly 35,000 km, maintained through the Road and
Bridge Inventory under Department Order 124 s. 2016. These are true
engineering profiles. They are held per-project, largely as scanned drawings
rather than a queryable database, distributed across District Engineering
Offices, and obtaining them at scale is a records-retrieval exercise measured
in months. Provincial, municipal, and barangay roads fall under LGU
jurisdiction and are not in this system at all.

**UP TCAGP holds 1 m LiDAR DTMs** from the DREAM and Phil-LiDAR programmes,
free for academic, government, and research use through the LiPAD portal.
Coverage is floodplains and priority river basins, not national. The
programme ended in 2017 and the portal is maintained by former researchers
as volunteer work, with requests routed through forms and turnaround measured
in weeks.

Against that, the openly available alternatives are thin. OpenStreetMap
supports an `incline` tag; across the entire Philippine extract, 5,385
features carry it, of which 5,192 are direction markers (`up`, `down`, `yes`,
`steep`) with no magnitude. Filtering to numeric values on actual road
classes leaves **eight features nationally**, all round-number mapper
estimates (10%, 15%, 20%) on minor roads. That is not a dataset; it is
anecdote.

So the gap this fills is real: for most of the Philippine road network, no
grade estimate of any kind is publicly available. A characterised
approximation is a substantial improvement over nothing, provided its limits
are stated as clearly as its contents.

A note on what shaped the design. This build was constrained to sources
requiring no discretionary request — no letters to administrators, no
institutional agreements, no human deciding whether to grant access.
Automatic registration (a form, instant access) was permitted. That
constraint closed off every sub-30 m option and forced the accuracy question
to be answered by characterisation rather than by better data. In retrospect
this produced a better-documented product than easier access would have,
because the uncertainty had to be measured rather than assumed away.

---

## 2. Data sources

| Source | Version / extract | Role | Licence |
|---|---|---|---|
| FABDEM | v1.2 (Hawker et al. 2022) | Bare-earth DEM; primary elevation | CC BY-NC-SA 4.0 |
| Copernicus DEM GLO-30 | Current AWS open-data release | Surface DEM; disagreement covariate | © ESA, free and open |
| OpenStreetMap | `philippines-260903.osm.pbf` (Geofabrik daily, 3 Sep 2026) | Road centerlines and attributes | ODbL 1.0 |
| GADM | v4.1, Philippines level 0 | Land mask | Free for academic/non-commercial |
| ICESat-2 ATL08 | 2019-01-01 to 2025-12-31, via SlideRule PhoREAL | Independent validation | NASA, public domain |
| EGM2008 geoid | `us_nga_egm08` via PROJ CDN | Vertical datum transformation | Public |

### 2.1 Why FABDEM as the primary elevation source

FABDEM is Copernicus GLO-30 with forest canopy and building heights removed
by a machine-learning correction trained on ICESat-2 and global land-cover
data. GLO-30 is itself derived from TanDEM-X and is the best openly available
30 m global DEM — void-filled, radar-derived, notably smoother than SRTM,
NASADEM, or ASTER GDEM.

But GLO-30 is a *surface* model. Over the forested terrain that covers much
of the Cordillera, Sierra Madre, and the Mindanao interior, its elevations
include canopy. Slope computed from a canopy surface is dominated by
canopy-edge discontinuities — abrupt 20–30 m steps at forest boundaries that
appear as cliffs and have nothing to do with the ground.

For terrain slope and road grade, this is the single largest error source at
30 m resolution, larger than pixel size. FABDEM addresses it directly. The
validation in Section 7 confirms the choice was correct: FABDEM's median
residual against ICESat-2 in cleanly measurable conditions is 0.01 m.

### 2.2 Why GLO-30 was retained

Rather than discarding GLO-30 after switching to FABDEM, both were kept and
warped to an identical grid. Their difference,

```
disagreement = GLO30 − FABDEM
```

was initially conceived as a canopy-height proxy. It turned out to be
something more useful, and the reinterpretation is discussed in Section 6.

### 2.3 Sources deliberately not used

**ALOS PALSAR RTC "12.5 m"** from ASF is widely cited in the literature and
is not a 12.5 m DEM. It is radiometrically terrain-corrected radar output
resampled from a 30 m source. Slope computed from it is 30 m slope plus
interpolation artefacts, presented at a resolution the data does not support.

**Resampling to a finer grid.** Warping 30 m data to 12 m produces a smoother
raster containing identical information, and silently shifts slope class
breaks. It creates the appearance of resolution without the substance.

**ML super-resolution DEMs.** These generate plausible terrain texture that
is not veridical. Acceptable for visualisation, disqualifying for slope
statistics.

**Sentinel-1 InSAR.** Theoretically a route to ~20 m, but interferometric
coherence over steep, wet, tropical forest is poor enough that months of work
would likely yield something worse than FABDEM.

**TanDEM-X 12 m.** Free for science via DLR proposal, but capped at
100,000 km² per proposal against a 300,000 km² country, and with no open
announcements of opportunity at time of writing.

---

## 3. Coordinate reference system

All processing is in a single custom Lambert conformal conic projection:

```
+proj=lcc +lat_1=7 +lat_2=19 +lat_0=13 +lon_0=122 +datum=WGS84 +units=m +no_defs
```

**Rationale.** The Philippines spans UTM zones 50N through 52N. Processing
per-zone would require stitching three mosaics with seams running through the
country, and any national statistic would need to account for zone
boundaries. A single conformal projection avoids this.

Conformality is the property that matters for slope. A conformal projection
preserves local angles and makes scale locally isotropic — equal in all
directions at a point — which means the ratio of vertical rise to horizontal
run is preserved regardless of aspect. An equal-area projection would
distort slope by direction; a plate carrée (geographic degrees) would
distort it by a factor of roughly 1/cos(latitude) in the east–west direction,
which at Philippine latitudes is a 3–7% error before any other consideration.

With standard parallels at 7°N and 19°N bracketing the country's 4°–21°N
extent, scale error stays below about 0.5% nationally — negligible against
the error budget established in Section 7.

**Grid geometry.** Both DEMs and all derived rasters share exactly:

```
size:    35,551 columns × 60,617 rows  (2.155 × 10⁹ pixels)
pixel:   30 m × 30 m
bounds:  left   −558278.8148624903
         bottom −927326.0623519261
         right   508251.1851375097
         top     891183.9376480739
```

Pixel alignment is not cosmetic. The disagreement covariate is a pixel-wise
difference between two rasters, and a half-pixel misregistration would
introduce a spurious terrain-correlated signal into exactly the variable used
to stratify reliability.

**Vertical datum.** FABDEM and GLO-30 are both referenced to the EGM2008
geoid (orthometric heights). ICESat-2 `h_te_median` as returned by SlideRule
is referenced to the WGS84 ellipsoid. In the Philippines the separation is
approximately −43 to −45 m and varies spatially.

This is a trap worth naming explicitly. In initial testing, `pyproj` returned
input coordinates *unchanged* when the EGM2008 grid was unavailable — height
0.0 in, height 0.0 out, no error, no warning. A silent no-op is worse than a
crash, because it produces a plausible-looking answer that is wrong by 43 m.
All datum conversions in this work are guarded:

```python
def to_orthometric(lon, lat, h_ellipsoid):
    out = _transformer.transform(lon, lat, h_ellipsoid)
    if abs(out[2] - h_ellipsoid) < 1.0:
        raise RuntimeError("geoid transform inactive — EGM2008 grid missing")
    return out[2]
```

The 1.0 m threshold is safe because the separation is nowhere near zero
anywhere in the Philippines.

---

## 4. Processing pipeline

### 4.1 Digital elevation models

**GLO-30.** Tile list retrieved from the AWS open-data bucket manifest and
filtered by name to the Philippine bounding box (4°–21°N, 116°–127°E),
yielding 109 tiles. Fetching from the manifest rather than constructing tile
names means only tiles that actually exist are requested — open ocean cells
are absent from the bucket and are skipped rather than 404-ing. Total
download 1.5 GB; the tiles are compressed cloud-optimised GeoTIFFs and most
Philippine cells are largely ocean, which compresses to almost nothing.

Every tile was verified openable with `gdalinfo` before mosaicking. A
truncated download fails silently at read time, typically deep inside a
subsequent long-running warp.

Tiles were assembled into a GDAL virtual raster (no pixel duplication),
reprojected to the LCC grid with bilinear resampling, and clipped to the GADM
level-0 land boundary.

**The land mask is not optional.** Copernicus encodes ocean as elevation 0,
not as nodata. Without clipping, every coastline becomes a 0-to-terrain
discontinuity that `gdaldem slope` correctly reports as a near-vertical
cliff. The effect on national statistics is large: unmasked mean slope is
3.38° (diluted by flat ocean) with maximum 89.66° (coastline artefacts);
masked, the same raster gives mean 12.06° and maximum 75.37°. The masked
maximum is steep but physically plausible for Philippine terrain; the
unmasked one is an artefact of the encoding.

**FABDEM.** Retrieved for the bounding box (116, 4, 127, 21) via the `fabdem`
Python package, which handles tile selection and assembly. Raw output is
39,600 × 61,200 at 1 arc-second — 11° of longitude by 17° of latitude,
9 GB uncompressed float. Warped to the identical LCC grid with bilinear
resampling and explicit target extent matching the GLO-30 product.

Warnings during this warp about the value −9999 being "changed to −9999" are
benign: FABDEM already uses −9999 internally as nodata, and GDAL was
reporting that the value passed through unchanged.

### 4.2 Terrain slope raster

```
gdaldem slope <fabdem> <out> -alg Horn -compute_edges
gdalwarp -cutline gadm41_PHL_0.shp -crop_to_cutline <out> <out_land>
```

Horn's method fits a plane to the 3×3 neighbourhood with distance weighting.
The alternative, Zevenbergen–Thorne, fits a fourth-order surface and is
sharper on smooth terrain but noisier on radar-derived DEMs, which carry
speckle. Horn is the appropriate default here.

`-compute_edges` prevents a one-pixel nodata border at raster and island
boundaries — material in an archipelago with thousands of small islands where
a large fraction of land pixels are near a coast.

Output units are degrees. `-p` would give percent rise if preferred.

### 4.3 Road network extraction

The Geofabrik daily extract was read directly by GDAL's OSM driver. Note
that `philippines-latest.osm.pbf` is a redirect; the resolved filename
`philippines-260903.osm.pbf` pins the extract date and **should be recorded
in any reproduction attempt**, because OSM changes daily and Geofabrik does
not serve historical dailies. The extract itself is retained as the
reproducibility anchor.

By default GDAL's OSM driver places most tags in a catch-all HSTORE column.
`osmconf.ini` was modified to promote `bridge`, `tunnel`, `layer`, `incline`,
`surface`, and `oneway` to first-class columns in the `lines` layer.

Extraction with `-where "highway IS NOT NULL"` yields **1,615,512 features**.
The class distribution:

| Class | Count | In dataset |
|---|---|---|
| residential | 717,889 | no |
| service | 326,973 | no |
| track | 116,228 | no |
| unclassified | 108,047 | **yes** |
| path | 102,466 | no |
| footway | 86,079 | no |
| tertiary | 61,607 | **yes** |
| secondary | 29,057 | **yes** |
| primary | 24,171 | **yes** |
| trunk | 15,242 | **yes** |
| steps | 7,116 | no |
| pedestrian | 3,155 | no |
| proposed | 3,055 | no |
| motorway | 2,923 | **yes** |
| construction | 2,689 | no |

The six included classes total **241,047 ways / 194,101 km**.

**Rationale for the class filter.** `motorway` through `unclassified` is the
roadway network in the OSM sense — routes carrying vehicular through-traffic.
`residential` and `service` (1.04M features) are predominantly urban and
short, where a 30 m pixel spans the entire street plus adjacent buildings and
the elevation sample bears little relation to the street surface. `track`,
`path`, `footway`, `steps`, and `pedestrian` (315k features) are not roads.
`proposed` and `construction` do not exist yet.

Including the excluded classes would roughly quintuple the vertex count while
adding data of substantially lower reliability. The filter is a parameter and
can be changed; the trade-off should be made deliberately.

### 4.4 Route reconstruction

**This step exists because of a diagnosed failure, and the diagnosis is worth
recording.**

OSM splits ways wherever any tag changes — a surface change, a lane count
change, a name suffix, an administrative boundary — not at physically
meaningful breaks. Profiling each OSM way independently means computing a
derivative on a truncated signal, and the truncation is arbitrary.

The initial implementation did exactly that. On the Kennon Road test case,
34 km of road arrived as 133 separate ways with a median length of 277 m and
a 25th percentile of 116 m. With a 100 m derivative window, a 116 m way has
*no* interior vertices — every point sits within a half-window of an end,
where the regression fits 3–5 points instead of 11. Diagnostic output was
unambiguous: all ten worst-offending vertices were at a way terminus, eight
of them at exactly the first or last vertex, and edge-vertex p95 grade was
35.5% against 22.3% mid-segment.

The fix reconstructs continuous routes before profiling:

1. Round every way's start and end coordinate to a 1 m grid (`SNAP = 1.0`).
2. Union-find over ways sharing a rounded endpoint, producing connected
   components.
3. `shapely.ops.linemerge` on each component to build continuous LineStrings.
4. Profile the merged geometry; attribute back to source ways afterwards.

On Kennon this collapsed 133 ways into **1 route group**. Nationally,
241,047 ways became **43,495 route groups** yielding **92,435 continuous
parts** (a group can yield multiple parts where it branches — `linemerge`
returns a MultiLineString and each component is profiled separately).

Edge vertices on Kennon fell from 402 to 48, and core versus all-vertex
statistics converged (median 6.89% vs 6.78%, p95 23.50% vs 23.23%),
confirming edges were no longer pathological.

**An honest correction to the initial diagnosis.** Fixing segmentation did
*not* materially reduce the extreme tail. Pre-fix mid-segment vertices gave
p95 22.3% and max 41.5%; post-fix core gave 23.5% and 45.7%. The edge
vertices were genuinely noisier, but they were not the source of the tail.
The tail is the resolution floor, discussed in Section 5.4. The route
reconstruction was still correct and necessary — it removed a real artefact —
but the initial hypothesis about what it would fix was wrong.

### 4.5 Densification and elevation sampling

Each continuous route part is densified to vertices at a **fixed 20 m spacing**
(`SPACING = 20.0`) via `shapely.interpolate` along the line. Parts shorter
than 60 m (`MIN_LEN`) are dropped as too short to support a 100 m derivative
window.

Fixed-interval densification rather than using OSM's native vertices is
essential. OSM vertex spacing is wildly non-uniform — dense on curves, sparse
on straights — and a derivative computed over irregular spacing weights
curves far more heavily than tangents, biasing results toward exactly the
locations where mapping precision is worst.

National total: **9,829,137 vertices**.

Elevation is sampled from both DEMs by **bilinear interpolation**
(`scipy.ndimage.map_coordinates`, `order=1`) through windowed reads. Nearest-
neighbour sampling would produce a staircase elevation profile, and
differencing a staircase yields alternating zero and spike grades. This alone
would render the output meaningless.

Sampling is tiled at 4,096 px with one-pixel overlap because the source
rasters are 2.155 billion pixels each and cannot be held in memory. DEM
coverage achieved: **99.9%**.

### 4.6 Grade computation

Grade is the **first derivative of elevation with respect to along-route
distance**, computed as a Savitzky–Golay filter with `polyorder=1`,
`deriv=1`, window 100 m (`RUN = 100.0`, i.e. 5 vertices at 20 m spacing),
preceded by Savitzky–Golay smoothing with window 9 vertices (180 m) and
`polyorder=2`.

**Savitzky–Golay with polyorder=1 and deriv=1 is mathematically identical to
a rolling ordinary-least-squares regression of elevation on distance**,
evaluated at each point. The explicit per-window `numpy.polyfit`
implementation was replaced by the filter formulation purely for speed — a
factor of roughly one thousand, which is the difference between the national
run being feasible and not.

**Why regression rather than endpoint differencing.** Differencing the
elevations at the two ends of a window uses two samples and discards the rest,
amplifying DEM noise directly into the output. Regression uses every vertex
in the window and is substantially more robust to the speckle characteristic
of radar-derived elevation data.

**Why 100 m.** This is the most consequential single parameter, and it
encodes a definition rather than a computation. Grade is scale-dependent: a
20 m window and a 200 m window give different, both-correct answers for the
same road. 100 m was chosen as a compromise between resolving genuinely
steep pitches (a 200 m window averages a short 12% climb into a longer gentle
one) and averaging over noise (a 40 m window on 30 m data is differentiating
adjacent pixels, which is nearly all noise). Users requiring a different
definition should recompute rather than reinterpret; the parameter is
exposed.

Outputs are stored as fractional grade, percent, and degrees.

### 4.7 Flagging

Two flags are computed and stored per vertex, and both are **excluded from
summary statistics but retained in the data**:

**`structure`** — the vertex lies on a way tagged `bridge` or `tunnel`.
57,286 vertices nationally. Retaining rather than deleting these allows their
grades to be compared against neighbours as a diagnostic; anomalous values
there confirm the flag is doing work.

**`edge`** — the vertex lies within a half-window (50 m) of a route part
terminus, where the derivative is computed on a truncated window. 552,301
vertices nationally. Note this is a genuine boundary condition after route
reconstruction, not the artefact of arbitrary splitting that motivated
Section 4.4.

A further 6,096 vertices have undefined grade (parts too short, or nodata in
the DEM). Core sample after exclusions: **9,216,958 vertices**.

### 4.8 Attribution

Vertices are joined back to source OSM ways by `geopandas.sjoin_nearest`
with `max_distance = 40 m` (twice the vertex spacing), processed in chunks of
200,000 to bound memory. The distance cap prevents a vertex from matching a
road on the opposite side of a valley. Summaries are keyed on `osm_id` so
results join to standard OpenStreetMap identifiers rather than internal
indices.

---

## 5. Results

### 5.1 National summary

| Statistic | Value |
|---|---|
| Network length | 194,101 km |
| Source ways | 241,047 |
| Route parts profiled | 92,435 |
| Vertices | 9,829,137 |
| Core vertices (after flags) | 9,216,958 |
| Median \|grade\| | 1.67% |
| p95 \|grade\| | 15.54% |
| DEM coverage | 99.9% |
| Total runtime | ~6 minutes |

### 5.2 By road class

| Class | Vertices | Median \|grade\| | p95 \|grade\| |
|---|---|---|---|
| motorway | 46,077 | 0.42% | 4.63% |
| trunk | 447,998 | 0.87% | 10.23% |
| primary | 720,509 | 1.30% | 14.64% |
| secondary | 1,067,529 | 1.47% | 14.56% |
| tertiary | 2,644,546 | 1.78% | 15.31% |
| unclassified | 4,290,299 | 1.89% | 16.42% |

**This table is the strongest internal validation available.** The ordering
is monotonic in road standard: motorway < trunk < primary ≈ secondary <
tertiary < unclassified. Higher-standard roads are built to tighter geometric
tolerances and show gentler grades. Nothing in the pipeline knows about road
classification — the class column is carried along but plays no part in the
computation — so this ordering is an emergent property of the data.

The motorway figure is the sharpest test. Philippine expressways are designed
to roughly 4–6% maximum grade. The pipeline independently recovers a p95 of
4.63% from a 30 m DEM. That is the method working correctly on roads wide
and flat enough for a 30 m pixel to actually see.

### 5.3 Independent test case: Kennon Road

Kennon Road (OSM class `trunk`) climbs from Rosario, La Union to Baguio
through the Bued River gorge — a narrow, heavily engineered mountain highway
with sustained real grades in the 6–10% range.

| Statistic | Value |
|---|---|
| Length profiled | 34 km |
| Source ways | 133 |
| Route groups after merge | 1 |
| Core vertices | 1,618 |
| Median \|grade\| | 6.89% |
| p95 \|grade\| | 23.50% |
| Max \|grade\| | 45.68% |

**The median is correct.** 6.89% against a true sustained 6–10% is a good
result from 30 m data on one of the most difficult road corridors in the
country.

**The tail is not.** Kennon does not have 23% pitches, and 45.68% is a cliff.
This is the resolution floor made visible: the road is roughly 7 m wide in a
gorge with near-vertical sidewalls, so a 30 m pixel centred on the carriageway
also contains rock face. No filtering recovers a road narrower than the
sampling interval.

Kennon was chosen deliberately as a near-worst case. It is the terrain where
this method is least reliable, and it is retained in documentation as the
practical upper bound on error rather than tuned away.

### 5.4 On the extreme tail

National maximum grade is 105.85% (46.6°), which is not a road. Values above
roughly 30% should be treated as artefacts.

The temptation is to widen the derivative window until the tail disappears.
This was considered and rejected: a 200 m window produces a tidier
distribution by flattening genuine steep pitches along with the artefacts,
buying presentational cleanliness with accuracy. The tail is instead handled
by the reliability stratification, which identifies *where* the method is
failing rather than suppressing the evidence that it is.

---

## 6. Reliability stratification

### 6.1 Construction

The stratification uses the pixel-wise difference between the two DEMs:

```
disagreement = GLO30 − FABDEM
```

| Tier | Condition | Vertices | Share | Length |
|---|---|---|---|---|
| high | \|disagreement\| < 1 m | 2,631,077 | 28.5% | 52,622 km |
| moderate | 1–5 m | 4,096,835 | 44.4% | 81,937 km |
| low | 5–15 m, or −5 to −1 m | 2,310,482 | 25.1% | 46,210 km |
| unreliable | > 15 m, or < −5 m | 178,564 | 1.9% | 3,571 km |

Grade statistics by tier:

| Tier | Median \|grade\| | p95 \|grade\| | % vertices > 20% |
|---|---|---|---|
| high | 0.60% | 9.88% | 0.86% |
| moderate | 1.42% | 12.48% | 1.21% |
| low | 4.91% | 20.34% | 5.29% |
| unreliable | 8.07% | 29.10% | 15.36% |

**73% of the network (134,559 km) falls in the high or moderate tiers**,
where median grade is 0.60–1.42% and fewer than 1.3% of vertices exceed 20%.
Only 1.9% is flagged unreliable.

### 6.2 What the covariate actually measures — a correction

The disagreement covariate was conceived as a canopy-height proxy: FABDEM
strips vegetation from GLO-30, so their difference should be vegetation
height. **This interpretation is wrong and the column should not be named
`canopy`.**

The evidence is the symmetry. Stratifying grade by disagreement produces
nearly identical behaviour at both extremes — the strongly-negative stratum
(median grade 10.34%, p95 32.80%, 21.17% of vertices above 20%) behaves like
the strongly-positive stratum (9.79%, 30.72%, 18.22%). If the variable were
canopy height, negative values would be meaningless and would not carry a
consistent signal.

Negative values arise where FABDEM's tree-removal algorithm misfires on steep
terrain and cuts into the hillside, producing bare-earth estimates *below* the
surface model with no vegetation to explain it.

The correct interpretation is that this is a measure of **DEM disagreement**,
and disagreement between two independently-derived elevation models is a
proxy for terrain complexity plus processing difficulty. Where GLO-30 and
FABDEM diverge in either direction, both models are struggling, and any
derivative computed from either is correspondingly less trustworthy.

The column is named `dem_disagree_m` in the released data for this reason.
Users encountering negative values should not interpret them as negative
vegetation.

---

## 7. External validation against ICESat-2

### 7.1 Approach

ICESat-2 carries a photon-counting laser altimeter with decimetre-class
vertical precision. The ATL08 land and vegetation product classifies returns
and reports terrain height on 100 m along-track segments. Where ICESat-2
tracks cross the Philippines, they provide independent elevation truth.

Retrieval used **SlideRule** (`slideruleearth.io`), which performs
subsetting and PhoREAL processing server-side and returns results directly,
avoiding a several-hundred-gigabyte bulk download of ATL03 granules.

The country was tiled into 93 one-degree cells intersecting the GADM land
boundary. Each was requested separately with per-tile output, making the job
resumable. Temporal range 2019-01-01 to 2025-12-31.

| Retrieval statistic | Value |
|---|---|
| Tiles requested / completed | 93 / 93 |
| Tile-level failures | 0 |
| Server-side track read failures | 2,534 |
| Total runtime | 3.2 hours |
| Terrain returns retrieved | ~150 million |
| Subsampled for analysis (1 in 40) | 856,948 |

The 2,534 track-read failures are server-side HDF5 read errors on individual
beam-tracks, roughly 1–2% of track-reads, randomly distributed with respect
to date, beam, and geography. They constitute random thinning, not
systematic loss.

Quality filtering: `gnd_ph_count > 0` (mandatory — segments with zero
ground photons report `h_te_median = 0.0` as a null sentinel, which would
otherwise appear as thousands of phantom sea-level points), plausible
elevation range, and finite DEM samples.

**Datum resolution.** Whether SlideRule's `h_te_median` was ellipsoidal or
orthometric was determined empirically rather than assumed. Testing both
against FABDEM: assuming orthometric gave a median residual of 44.73 m,
tracking the local geoid separation of 43.08–44.77 m almost exactly; assuming
ellipsoidal and converting gave 0.66 m. **`h_te_median` is ellipsoidal.**

### 7.2 Headline results

Residual is defined as ICESat-2 terrain height (converted to EGM2008) minus
FABDEM.

| Statistic | Value |
|---|---|
| n | 856,948 |
| Median residual | −0.34 m |
| IQR | 4.76 m |
| MAD | 2.42 m |

**By DEM disagreement:**

| Disagreement | n | Median | IQR |
|---|---|---|---|
| < −15 m | 315 | −32.49 | 16.78 |
| −15 to −5 | 14,937 | −19.81 | 11.32 |
| −5 to −1 | 65,131 | −8.13 | 8.74 |
| −1 to 1 | 306,875 | −0.33 | **1.82** |
| 1 to 5 | 237,497 | −0.81 | 4.70 |
| 5 to 15 | 191,566 | 3.50 | 10.79 |
| > 15 m | 40,627 | 16.27 | 16.42 |

**By terrain slope:**

| Slope | n | Median | IQR |
|---|---|---|---|
| 0–5° | 434,291 | −0.34 | 1.89 |
| 5–10° | 125,360 | −1.13 | 8.36 |
| 10–20° | 166,149 | −0.58 | 15.03 |
| 20–30° | 83,155 | 0.12 | 24.84 |
| > 30° | 32,237 | 1.16 | 37.68 |

**By ICESat-2 measured canopy height:**

| Canopy | n | Median | IQR |
|---|---|---|---|
| < 1 m | 114,047 | −0.00 | 0.81 |
| 1–5 m | 153,821 | −0.29 | 2.06 |
| 5–15 m | 376,483 | −0.63 | 6.40 |
| > 15 m | 212,597 | −1.31 | 12.57 |

Two conclusions. First, **the reliability tiers are externally validated**:
residual IQR rises from 1.82 m to 16.42 m across disagreement bins, and from
1.89 m to 37.68 m across slope bins, exactly as the stratification predicts.
Second, **medians stay near zero across slope and canopy strata while IQR
grows** — these are noise indicators, not bias indicators.

### 7.3 A correction was fitted and rejected

Regressing residual on disagreement produced strikingly consistent
coefficients across three geographically separate regions:

| Region | n | Slope | Intercept |
|---|---|---|---|
| Luzon | 360,403 | 1.076 | −3.47 |
| Visayas | 254,280 | 0.957 | −4.05 |
| Mindanao/Palawan | 240,308 | 1.005 | −4.21 |

Three independent regions, three near-identical relationships, slope
approximately 1.0. Taken at face value this implies

```
truth ≈ FABDEM + (GLO30 − FABDEM) − 4  =  GLO30 − 4
```

— that wherever the two DEMs disagree, GLO-30 is closer to truth than
FABDEM, minus a constant offset. A single national correction appeared
justified, and would have roughly halved vertical scatter.

**This was tested by stratification before application, and the test rejected
it.**

| Stratum | n | Regression slope | Intercept |
|---|---|---|---|
| canopy < 1 m | 114,046 | 0.662 | −0.30 |
| canopy 1–5 m | 153,806 | 1.122 | −1.46 |
| canopy 5–15 m | 376,110 | 1.360 | −4.63 |
| canopy > 15 m | 211,029 | 1.251 | −9.74 |
| slope 0–5° | 434,285 | 0.245 | −1.07 |
| slope 5–10° | 125,357 | 0.810 | −3.95 |
| slope 10–20° | 165,945 | 1.174 | −6.52 |
| slope 20–90° | 113,648 | 1.594 | −10.98 |
| **flat (<5°) + open (<1 m canopy)** | **110,313** | **0.054** | **−0.13** |

In the cleanest stratum — flat terrain with no canopy, where ICESat-2 ground
returns are unambiguous — **the regression slope is 0.054 and the intercept
is −0.13 m, with median residual 0.01 m on 110,313 points.**

A slope of zero means disagreement carries no information about FABDEM's
error under conditions where the reference data is trustworthy. The
relationship appears *only* as canopy and terrain slope increase — precisely
the conditions under which ICESat-2's ground classification degrades, because
few photons penetrate dense canopy to the forest floor and because a 100 m
along-track segment on steep terrain spans real elevation change that is
collapsed into a single height.

**The apparent correction is an artefact of reference-data error, not a
property of the DEMs.** Applying it would have imported ICESat-2's canopy and
slope biases into the elevation model, degrading it while appearing to
improve internal consistency.

The correction was therefore not applied, and FABDEM is released unmodified.

This is recorded in detail because the naive national fit was convincing —
three independent regions agreeing to within 12% on the slope coefficient is
the kind of result that normally indicates a real relationship. Only
conditional analysis distinguished a genuine signal from a shared artefact.

### 7.4 Validated accuracy statement

- FABDEM is **unbiased** where independently verifiable: intercept −0.13 m,
  median residual 0.01 m, in flat open terrain (n = 110,313).
- National residual: median −0.34 m, IQR 4.76 m across all conditions.
- Vertical uncertainty scales strongly with terrain slope (IQR 1.89 m → 37.68 m)
  and canopy height (0.81 m → 12.57 m).
- The reliability tiers correctly identify **noise**, not correctable bias.

---

## 8. Known limitations

**Resolution floor.** The roadway is narrower than one pixel. Cuts, fills,
embankments, retaining structures, and switchback geometry are not
resolvable. This is not addressable by processing.

**Systematic upward bias in steep terrain.** Grade follows raw terrain where
real roads were engineered to avoid it. The bias is small on gentle terrain
and large in narrow valleys; the reliability tiers indicate which applies.

**Extreme values are artefacts.** Anything above roughly 30%, and certainly
the national maximum of 105.85%, is not a road.

**Bridges and tunnels are flagged only if tagged.** OSM tagging is
incomplete; untagged structures produce phantom dives and climbs.

**OSM geometry quality varies.** Centerline positional accuracy is not
uniform, and a laterally displaced centerline samples the wrong terrain. This
matters most in steep terrain, where the two error sources compound.

**Scale dependence.** Grade is defined at a 100 m run length. Values are not
comparable to grades defined over different baselines without recomputation.

**Temporal mismatch.** The OSM extract is September 2026; FABDEM derives from
Copernicus acquisitions of 2011–2015; ICESat-2 validation spans 2019–2025.
Roads built or realigned after the DEM epoch are sampled against pre-existing
terrain.

**Class coverage.** Residential, service, track, and path classes are
excluded — roughly 1.35 million OSM features. Most Philippine road
*kilometres* by count are in these classes.

**Validation is of elevation, not grade.** ICESat-2 validates the DEM's
vertical accuracy. Grade error is inferred from elevation error and terrain
characteristics. There is no direct ground-truth grade measurement anywhere
in this work, because none is openly available — the eight numeric OSM
`incline` tags are insufficient for any statistical statement.

---

## 9. Reproduction

Environment: GDAL 3.13.3, Python 3.9, macOS (Apple Silicon). Full package
versions in `requirements.txt`.

```
1. Copernicus GLO-30    109 tiles from s3://copernicus-dem-30m (manifest-filtered)
2. FABDEM v1.2          bbox (116, 4, 127, 21) via `fabdem` package
3. Warp both            to LCC grid, 30 m, bilinear, identical -te
4. Land mask            GADM 4.1 PHL level 0 cutline
5. Slope raster         gdaldem slope -alg Horn -compute_edges
6. OSM extract          philippines-260903.osm.pbf, modified osmconf.ini
7. grade.py all         route merge → densify → sample → SG derivative
8. Reliability tiers    from dem_disagree_m
9. ice_national.py      93 tiles, SlideRule ATL08/PhoREAL
10. ice_analyze.py      residuals, stratified regressions
```

Total compute: approximately 30 minutes for the DEM and grade pipeline, plus
3.2 hours for ICESat-2 retrieval.

**Reproducibility caveat.** OpenStreetMap changes continuously and Geofabrik
does not serve historical daily extracts. Exact reproduction requires the
retained `philippines-260903.osm.pbf`. SHA-256 checksums for all released
files are in `MANIFEST.txt`.

---

## 10. Licensing and attribution

This dataset is derived from FABDEM, which is licensed **CC BY-NC-SA 4.0**.
That licence propagates: **this dataset is non-commercial and share-alike.**

Required attributions:

- Hawker, L., Uhe, P., Paulo, L., Sosa, J., Savage, J., Sampson, C., Neal, J.
  (2022). A 30 m global map of elevation with forests and buildings removed.
  *Environmental Research Letters*, 17(2), 024016.
- Copernicus DEM © ESA.
- © OpenStreetMap contributors, ODbL 1.0.
- ICESat-2 ATL08, NASA NSIDC DAAC. Processing via SlideRule Earth.
- GADM v4.1.

---

## 11. Acknowledgement of what is missing

The correct data for this problem exists and is held by NAMRIA, DPWH, and
UP TCAGP. This dataset is a substitute built under a no-institutional-access
constraint, and it should be replaced rather than extended if that access
becomes available.

Specifically: NAMRIA's 5 m IfSAR DTM would likely reduce the resolution floor
enough to make per-segment values meaningful on national roads. DPWH as-built
vertical alignments are the actual answer for the 35,000 km national network
and would additionally provide the ground-truth grade measurements this work
lacks entirely. LiPAD's 1 m LiDAR would permit direct validation of grade
rather than inference from elevation error.

Until then, this is the best openly reproducible estimate available, and its
uncertainty is characterised rather than assumed.
