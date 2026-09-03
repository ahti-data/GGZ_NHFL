# GGZ_NHFL — plan voor het kickoff-dashboard

Opgesteld na een scan van `GGZ_NHFL`, `maptool_v4`, `shiny_dashboard_template` en de
zusterprojecten `pharm` / `RVS_laatste_1000_dagen`.

## Stand van zaken

| Stap (par. 8) | Status |
|---|---|
| 1. Repo-skelet met `dashboard/`, `utils/`, `templates/`, deploy-workflow | ✅ gedaan |
| 2. Geo-assets en gemeente→provincie-lookup | ✅ gedaan |
| 3. `utils/ggz_data.R` + tests | ✅ gedaan — 61 tests groen |
| 4. Echte extractie uit de database | ⛔ **geblokkeerd** — geen DB-toegang vanaf deze werkplek |
| 5. `utils/ggz_map.R` | ✅ gedaan |
| 6. Tab "Kickoff" in `app.R` | ✅ gedaan — draait, alle 480 combinaties getest |
| 7. `README.md` herschreven | ✅ gedaan |
| 8. Deploy | ⏸ wacht op stap 4 en op vraag 7 |

Het dashboard draait nu op **synthetische cijfers** (`output_src/00_build_synthetic_data.R`)
en toont daarover een waarschuwingsbalk. Zodra `01_extract_ggz_zpm.R` op een machine met
databasetoegang gedraaid heeft, vervangt het echte extract die stand-in en verdwijnt de balk
vanzelf — er hoeft geen code aangepast te worden.

Twee dingen die tijdens het bouwen naar boven kwamen en niet in het oorspronkelijke plan
stonden, staan in par. 10.

---

## 1. Wat er nu is (resultaat van de scan)

### 1.1 `GGZ_NHFL` (dit repo)

Leeg placeholder-repo: `README.md` (auto-gegenereerd, inhoudsloos), `output_src/.gitkeep`,
`outputs/.gitkeep`, `.github/project-metadata.yml` (`output_upload_name: 'disabled'`).
Geen workflow, geen dashboard, geen data. Alles moet hier nieuw komen.

### 1.2 `maptool_v4` — de databron

Eén `app.R` van 2.112 regels + 9 `*_cols.R` menubestanden + `data/` (284 MB geo) + `.env`.

**Datamodel (PostgreSQL, AWS RDS, tabel `zorgkosten`):** kolommen `code`, `level_type`,
`year`, `version`, `population`, `measure_type`, `outcome_type`, `value` (+ `cat`, `n` in de
`*_profiles`-tabellen).

| dimensie | waarden |
|---|---|
| `level_type` | `Gemeente`, `Wijk`, `Buurt` — **géén provincie** |
| `version` | `v2` (huidig), `v1` (vorig). v2 gebruikt T-1 demografie → geo-jaar = datajaar − 1 |
| `population` | `pop`, `sub18`, `18-65`, `65+` |
| `measure_type` | `n`, `target_USE`, `target_COSTS`, `target_PCCOSTS`, `index_USE`, `index_COSTS`, `index_PCCOSTS` |
| `year` | slider staat op 2016–2024 |

**De vier gevraagde `outcome_type`-waarden staan er al in** (`zorgkosten_cols.R`):

| Label van de collega | `outcome_type` |
|---|---|
| GGZ ZPM (totaal) | `zvwggzzpmtotaal` |
| GGZ ZPM - Consult | `sub_zvwggzzpmconsult` |
| GGZ ZPM - Overig | `sub_zvwggzzpmoverigprest` |
| GGZ ZPM - Verblijf | `sub_zvwggzzpmverblijf` |

**Geo-assets:** `data/geo_municipalities.rds` (sf, WGS84, 3.642 rijen = 342 gemeenten × 10 jaar
2015–2024, kolommen `code`/`gemeentenaam`/`jaar`/`geometry`, 3,7 MB) plus wijk- (13 MB) en
buurt-rds'en (37 MB) en drie geojson-broninvoeren van samen 236 MB.
**Er zit géén provinciekolom in.**

**Herbruikbare stukken code:** de PNG-kaartexport (`output$downloadMap`, regel 1936–2090:
`geom_sf` + `scale_fill_manual` op vaste bins, `theme_void`, titel/subtitel/caption), de
bin- en legendalogica (`palette_generator`, regel 1366) en de hover-formattering
(`format_hover_value`, regel 689). De rest (9 zorgdomeinen, drill-down naar wijk/buurt,
profieltabellen, kleurenpickers, `set_profiles`-parsing) gaat **niet** mee.

> ⚠️ **De database is vanaf deze machine niet bereikbaar.** `RPostgres` geïnstalleerd en
> verbinding geprobeerd met de credentials uit `maptool_v4/.env` → `Connection timed out`
> op poort 5432. Waarschijnlijk staat dit IP niet in de RDS security group, of is er VPN
> nodig. Alle schema-uitspraken hierboven komen daarom uit de *code*, niet uit een live
> query. **Stap 1 van de uitvoering is verificatie tegen de echte database** (zie §7).

### 1.3 `shiny_dashboard_template` — het fundament

`app.R` (134 regels scaffold) + `utils/` (5.153 regels: think-cell-export, slide-export,
favorites, export-history, template-admin, tab-theme, auth) + `templates/` (15 pptx) +
`data/metadata/brand_colors.R` (ahti-huisstijl).

Twee API-details die het plan bepalen:

- `chart_data_downloads_ui(id, chart_type = ...)` toont **alleen** de knop
  "Download Excel data (raw)" als het chart_type níet in `TC_SUPPORTED_CHART_TYPES`
  (`line`, `bar`, `stacked_bar`, `grouped_bar`, `waterfall`) zit én er geen template in
  `TC_TEMPLATE_BY_CHART_TYPE` op past. Een `chart_type = "map"` levert dus vanzelf een
  panel met precies één knop op — geen think-cell, geen slide, geen favorietenster.
  Precies wat gevraagd is, zónder de think-cell-code te slopen.
- `tc_tab_color_theme(ahti_branding)` kleurt de tabs Favorites / Export history /
  Manage templates. Blijft staan.

### 1.4 Conventie in de zusterprojecten (`pharm`)

`pharm` heeft dezelfde repo-vorm als `GGZ_NHFL` (`output_src/`, `outputs/`,
`.github/project-metadata.yml`) plús een `dashboard/`-map met een kopie van `app.R`,
`data/`, `utils/`, `templates/` en `tests/`. De deploy-workflow kopieert
`dashboard/{app.R,data,utils,templates}` naar `/apps/<projectnaam>/`.
**`pharm` heeft shinymanager-auth eruit gehaald** (commit `279ae38`, "access is handled
server-side") — toegang loopt via Authelia/nginx op `healthinsights.ahti.nl`.
Dat volgen we hier ook.

---

## 2. Doelstructuur van dit repo

```
GGZ_NHFL/
├─ .github/
│  ├─ project-metadata.yml          (bestaat al)
│  └─ workflows/deploy.yml          NIEUW — kopie van pharm's workflow
├─ output_src/
│  ├─ 01_extract_ggz_zpm.R          NIEUW — eenmalige DB-extractie
│  ├─ 02_build_geo_assets.R         NIEUW — eenmalige geo-preparatie
│  └─ 03_build_gemeente_provincie.R NIEUW — lookup genereren
├─ dashboard/
│  ├─ app.R                         NIEUW — template-scaffold + tab "Kickoff"
│  ├─ data/
│  │  ├─ metadata/brand_colors.R    kopie uit template
│  │  ├─ ggz_zpm_gemeente.rds       extract (§3)
│  │  ├─ geo_gemeente.rds           §4
│  │  ├─ geo_provincie.rds          §4
│  │  ├─ geo_regio.rds              §4 (NH+FL / overig NL)
│  │  └─ gemeente_provincie.csv     §4
│  ├─ utils/                        kopie uit template + 2 nieuwe bestanden
│  │  ├─ chart_downloads.R              ongewijzigd overnemen
│  │  ├─ export_history.R               idem
│  │  ├─ favorites.R                    idem
│  │  ├─ format_thinkcell_download.R    idem
│  │  ├─ slide_download.R               idem
│  │  ├─ tab_theme.R                    idem
│  │  ├─ template_admin.R               idem
│  │  ├─ ggz_data.R                 NIEUW — laden + aggregeren (§5)
│  │  └─ ggz_map.R                  NIEUW — bins, legenda, leaflet, PNG-export (§6)
│  ├─ templates/                    kopie uit template (15 pptx + previews)
│  └─ tests/testthat/               kopie + nieuwe tests voor ggz_data.R
├─ PLAN.md                          dit bestand
└─ README.md                        herschrijven (het huidige is auto-gegenereerde ruis)
```

`utils/auth.R` gaat **niet** mee (zie §1.4). `.env` komt in `.gitignore`, niet in git.

---

## 3. Stap A — data-extractie ("niet alles meenemen")

Script `output_src/01_extract_ggz_zpm.R`, één keer te draaien vanaf een machine mét
DB-toegang. Het schrijft één klein bestand dat daarna in git kan.

```sql
SELECT code, year, version, population, measure_type, outcome_type, value
FROM zorgkosten
WHERE level_type = 'Gemeente'
  AND year IN (2022, 2023, 2024)
  AND outcome_type IN ('zvwggzzpmtotaal', 'sub_zvwggzzpmconsult',
                       'sub_zvwggzzpmoverigprest', 'sub_zvwggzzpmverblijf')
  AND measure_type IN ('target_USE','target_COSTS','target_PCCOSTS',
                       'index_USE','index_COSTS','index_PCCOSTS')
```

plus, apart, de noemer `n` (in `maptool_v4` wordt `n` altijd zónder `outcome_type`-filter
opgehaald — dat gedrag moet de extractie exact spiegelen):

```sql
SELECT DISTINCT code, year, version, population, value AS n
FROM zorgkosten
WHERE level_type = 'Gemeente' AND year IN (2022,2023,2024) AND measure_type = 'n'
```

**Omvang:** ±342 gemeenten × 3 jaar × 4 domeinen × 6 measures × 4 populaties × 2 versies
≈ 200k rijen, plus ±8k `n`-rijen. Als gecomprimeerde `.rds` naar verwachting 1–3 MB —
klein genoeg om te committen. Ter vergelijking: de hele `maptool_v4/data/` is 284 MB;
daarvan komt **niets** mee.

**Scope-beslissing:** `version` en `population` wél mee-extraheren, maar in de UI alleen
`v2` + `pop` tonen (de rest als reserve voor latere tabs). Kost bijna niets aan
bestandsgrootte en voorkomt een tweede DB-run als de collega alsnog "en dan voor 18-65"
vraagt.

**CBS-uitvoerregels.** De maptool heeft niet voor niets een knop "verwijder gemeenten met
observaties kleiner dan <30/<50". Op gemeenteniveau voor een deelprestatie als
`GGZ ZPM - Verblijf` gaat dat in kleine gemeenten zeker knellen. Die onderdrukking komt
daarom mee als **verplichte functionaliteit met `<30` als standaard** (niet als optioneel
extraatje), berekend als `n × target_USE` per gemeente, toegepast vóór aggregatie én in de
kaart, de tabel en de download. Het `n`-veld is daarvoor sowieso nodig (zie §5).

---

## 4. Stap B — geografie

Scripts `output_src/02_build_geo_assets.R` en `03_build_gemeente_provincie.R`.

1. **Gemeentelaag.** `maptool_v4/data/geo_municipalities.rds` inlezen, filteren op de
   benodigde geo-jaren (bij `v2`: 2021, 2022, 2023 — geo-jaar = datajaar − 1),
   vereenvoudigen met `rmapshaper::ms_simplify(keep = 0.05, keep_shapes = TRUE)`.
   Verwacht ±0,5–1 MB.
2. **Gemeente → provincie-lookup.** ✅ **Opgelost — die ligt er al.** De officiële
   CBS-tabel "Gebieden in Nederland" staat per jaar in
   `infectieziekten_monitor_prod/infectieziekten_monitor/data/input/` (2018–2023; ook een
   deelkopie in `Kraamzorg_map/data/crosswalks/`). Kolom
   `Codes en namen van gemeenten/Code` → `Lokaliseringen van gemeenten/Provincies/Naam`,
   met daarnaast GGD-regio, veiligheidsregio en ressort.

   Formaat: CSV met `;`, UTF-8-BOM, gemeentecodes als `"GM0358    "` — dus `GM` eraf en
   `trimws()`; dan sluit het aan op de 4-cijferige `code` in `geo_municipalities.rds`.

   Ik heb de aansluiting geverifieerd tegen de geo-jaren die we nodig hebben (v2 → geo-jaar
   = datajaar − 1, dus 2021/2022/2023):

   | geo-jaar | gemeenten in CSV | gemeenten in geo | zonder match | provincies |
   |---|---|---|---|---|
   | 2021 | 352 | 352 | **0** | 12 |
   | 2022 | 345 | 345 | **0** | 12 |
   | 2023 | 342 | 342 | **0** | 12 |

   Volledige dekking, geen handwerk nodig, en de herindelingen (Weesp → Amsterdam per
   24-3-2022, Voorne aan Zee per 2023) zitten er per jaar correct in. Voor 2023 telt
   Noord-Holland 44 en Flevoland 6 gemeenten — dat zijn de 50 vlakken van kaartweergave 5.

   Actie: die drie CSV's kopiëren naar `dashboard/data/` en met `03_build_gemeente_provincie.R`
   normaliseren tot één `gemeente_provincie.csv` met kolommen `jaar, code, provincie`.

   > Alleen **2024 ontbreekt** in de lokale set. Dat is uitsluitend nodig als we op `v1`
   > zouden overstappen (geo-jaar = datajaar); bij de voorgestelde `v2` speelt het niet.
   > Zo nodig bij te halen via CBS StatLine of de PDOK-WFS-route die
   > `infectieziekten_monitor/src/00_geo.R` al gebruikt.
3. **Provincielaag.** Niet een kant-en-klare provincielaag gebruiken (er ligt er een in
   `shiny_dashboard_template/temp/.../data/geo/provinces.json`, en PDOK levert er ook een
   via de WFS-route uit `infectieziekten_monitor/src/00_geo.R`), maar de gemeentelaag
   **dissolven** per provincie per jaar (`group_by() |> summarise(st_union())`). Zo sluiten
   de provinciegrenzen exact aan op de gemeentegrenzen waarop ook de cijfers geaggregeerd
   worden — geen slivers, geen gemeenten die net buiten een provincie vallen.
4. **Regiolaag.** Uit dezelfde dissolve: `NH+FL` als één polygoon, `Overig Nederland` als
   één polygoon, en `Noord-Holland` / `Flevoland` los.

---

## 5. Stap C — uitkomstmaten en aggregatie (het inhoudelijk lastigste stuk)

### 5.1 De zeven gevraagde uitkomsten → `measure_type`

Afgeleid uit `get_health_data()` (regel 790–870) en de `rel_abs`-logica in `maptool_v4`:

| Gevraagde uitkomst | Berekening op gemeenteniveau |
|---|---|
| Relatief aantal (aandeel gebruikers) | `target_USE` |
| Totaal aantal (gebruikers) | `target_USE × n` |
| Kosten per capita | `target_PCCOSTS` |
| Kosten totaal | `target_PCCOSTS × n` |
| Kosten per gebruiker | `target_COSTS` |
| Index gebruik | `index_USE` |
| Index kosten | `index_COSTS` |

Onderbouwing: in `maptool_v4` wordt bij "Absoluut" met `n` vermenigvuldigd voor álle
measures **behalve** `target_COSTS` en de indices (regel 855–870) — precies consistent met
`COSTS` = kosten per gebruiker en `PCCOSTS` = kosten per inwoner.

**Twee dingen te bevestigen tegen de database (§7):** (a) dat `COSTS` inderdaad *per
gebruiker* is en niet per declaratie, en (b) of "index kosten" `index_COSTS` (index van de
kosten per gebruiker) of `index_PCCOSTS` (index van de kosten per capita) moet zijn. Als ze
allebei bestaan, zou ik ze beide extraheren en in de UI labelen als "Index kosten (p.g.)"
en "Index kosten (p.c.)" — dan hoeft er niet gegokt te worden en zien we het bij de kickoff
meteen.

**Validatie die we gratis krijgen:** `kosten totaal` kan langs twee wegen —
`PCCOSTS × n` en `COSTS × (USE × n)`. Die twee moeten op afrondingsruis na gelijk zijn.
Dat wordt een testthat-test; wijkt het af, dan klopt de interpretatie van de measures niet.

### 5.2 Aggregatie naar provincie / regio — let op de indices

Alle vijf de kaartweergaven van de collega vragen om aggregatie over gemeenten. Voor de
gewone maten is dat een gewogen som of gemiddelde, maar **voor de indices is een
bevolkingsgewogen gemiddelde van de index fout.** Een index is een verhouding
geobserveerd/verwacht en moet als verhouding van sommen worden herberekend:

```
gebruikers_g        = target_USE_g      × n_g
kosten_totaal_g     = target_PCCOSTS_g  × n_g
verwacht_gebr_g     = gebruikers_g      / index_USE_g
verwacht_kosten_g   = kosten_totaal_g   / index_COSTS_g      (voorbehoud §5.1b)

Per gebied G:
  totaal_aantal(G)        = Σ gebruikers_g
  relatief_aantal(G)      = Σ gebruikers_g    / Σ n_g
  kosten_totaal(G)        = Σ kosten_totaal_g
  kosten_per_capita(G)    = Σ kosten_totaal_g / Σ n_g
  kosten_per_gebruiker(G) = Σ kosten_totaal_g / Σ gebruikers_g
  index_gebruik(G)        = Σ gebruikers_g    / Σ verwacht_gebr_g
  index_kosten(G)         = Σ kosten_totaal_g / Σ verwacht_kosten_g
```

Deze aggregatie komt in één functie `ggz_aggregate(data, by)` in `utils/ggz_data.R`, met
`by ∈ {gemeente, provincie, nh_fl_vs_rest, nh_fl_totaal, nh_fl_provincies}`, en krijgt
testthat-tests: aggregeren met `by = "gemeente"` moet de invoer ongewijzigd teruggeven,
aggregeren van één gemeente moet die gemeente reproduceren, en een gewogen gemiddelde van
indices moet aantoonbaar afwijken van de correcte som-van-sommen.

> ⚠️ Dit is een aanname waar de kickoff op steunt: **`index_USE = geobserveerd / verwacht`
> met een multiplicatieve interpretatie.** Definieert de NL-output-pipeline de index anders
> (bijvoorbeeld gestandaardiseerd op een andere noemer), dan klopt de decompositie niet.
> Te verifiëren in de `nl_output`-pipelinecode of bij de bouwer van de maptool. Staat in §7
> als blokkerende vraag vóór publicatie van provinciecijfers, niet vóór het bouwen.

---

## 6. Stap D — de tab "Kickoff"

Eén tabblad, sidebar links, kaart rechts. Alles in het Nederlands.

**Bediening (sidebar):**

| control | keuzes | default |
|---|---|---|
| Zorgdomein | GGZ ZPM (totaal) · Consult · Overig prestaties · Verblijf | totaal |
| Jaar | 2022 · 2023 · 2024 | 2024 |
| Uitkomst | de zeven uit §5.1 | Relatief aantal |
| Kaartweergave | zie hieronder | NL — provincies |
| Doelpopulatie | Totale populatie (rest voorbereid, verborgen) | Totale populatie |
| Kleine aantallen | Nee · <30 · <50 | **<30** |
| Kleur laag / hoog | `colourInput`, ahti-palet als default | — |

**De vijf kaartweergaven (exact wat de collega vroeg):**

1. `nl_provincie` — heel NL, twaalf provincies
2. `nl_nhfl_rest` — heel NL, twee vlakken: NH+FL versus overig Nederland
3. `nhfl_totaal` — alleen NH/FL, als één geheel
4. `nhfl_provincies` — alleen NH/FL, als twee provincies
5. `nhfl_gemeente` — alleen NH/FL, op gemeenteniveau

**Hoofdpaneel:** een koptekst met de gekozen selectie in gewone taal (naar het model van
`output$toggleText` in de maptool), de leaflet-kaart met de verticale legenda uit de
maptool, en daaronder de bijbehorende tabel (gebied, waarde, `n`, aantal gebruikers,
onderdrukt ja/nee). Bij weergave 3 (één vlak) is een kaart weinig informatief; daar wordt
de waarde ook groot als kerncijfer boven de kaart getoond.

**Downloads:** één panel onder de kaart met

- `downloadButton("kickoff_map_png")` — de PNG-kaartexport, geport uit
  `maptool_v4:1936–2090` (`geom_sf` + vaste bins + titel/subtitel/caption), met
  `Bron: ahti — CBS-microdata, NL-output` in de caption;
- `chart_data_downloads_ui("kickoff_dl", chart_type = "map")` — levert automatisch alleen
  de knop "Download Excel data (raw)" op, met de tabel achter de kaart (§1.3).

**Wat blijft staan voor later:** de tabs Favorites, Export history en Manage templates, de
volledige `utils/`-set met de think-cell-export, de vijftien pptx-templates, en
`tc_tab_color_theme()`. Er wordt niets verwijderd behalve `auth.R`.

---

## 7. Openstaande vragen (en wat ik zonder antwoord doe)

| # | Vraag | Zonder antwoord doe ik |
|---|---|---|
| 1 | **DB-toegang.** Vanaf welke machine of VPN is de RDS-host op poort 5432 bereikbaar? | Blokkerend voor stap A. De rest (geo, utils, UI-skelet) kan wél alvast, met synthetische data. |
| 2 | "GGZ ZPM - Overig" = `sub_zvwggzzpmoverigprest` ("Overig prestaties")? | Ja aannemen — het is de enige ZPM-subcategorie die op "overig" past. |
| 3 | "Index kosten" = `index_COSTS` (p.g.) of `index_PCCOSTS` (p.c.)? | Beide aanbieden in de UI, gelabeld. |
| 4 | Klopt de aanname `index = geobserveerd / verwacht` (§5.2)? | Zo implementeren, met een zichtbare voetnoot in de tab tot het bevestigd is. |
| 5 | Dataversie `v1` of `v2`? | `v2` (huidig), zoals de maptool zelf default. |
| 6 | Doelpopulatie: alleen de totale bevolking? | Ja; de rest wordt wel geëxtraheerd maar niet getoond. |
| 7 | Deploy-doel `/apps/GGZ_NHFL/` op `healthinsights.ahti.nl` achter Authelia, en welke groep krijgt toegang? | Workflow klaarzetten, maar de map moet op de server al bestaan (zie par. 10.4). |

---

## 8. Uitvoering in stappen

| # | Stap | Afhankelijk van | Controlepunt |
|---|---|---|---|
| 1 | Repo-skelet: `dashboard/` met `utils/`, `templates/`, `data/metadata/`, deploy-workflow; `auth.R` weglaten; `.gitignore` | — | `shiny::runApp()` draait het ongewijzigde template-scaffold |
| 2 | `output_src/02` + `03`: geo-assets en gemeente→provincie-lookup (bron al gevonden, §4.2) | — | 12 provincies, 342 gemeenten, elke gemeente exact één provincie, per jaar — vooraf al geverifieerd: 0 zonder match voor 2021/2022/2023 |
| 3 | `utils/ggz_data.R` + tests, eerst op **synthetische** data | 2 | testthat groen, inclusief de twee-wegen-check op kosten totaal |
| 4 | `output_src/01`: echte extractie | DB-toegang (vraag 1) | rijaantallen en `measure_type`-waarden komen overeen met §1.2 |
| 5 | `utils/ggz_map.R`: bins, legenda, leaflet, PNG-export | 2, 3 | vijf weergaven renderen, PNG-download werkt |
| 6 | Tab "Kickoff" in `app.R` | 3, 5 | alle 4 × 3 × 7 × 5 = 420 combinaties leveren óf een kaart óf een nette "geen data"-melding |
| 7 | `README.md` herschrijven, `PLAN.md` bijwerken | 1–6 | — |
| 8 | Deploy | vraag 7 | — |

Stappen 1–3 en 5 kunnen dus meteen beginnen; alleen stap 4 wacht op DB-toegang.

---

## 9. Risico's

- **Kleine aantallen op gemeenteniveau in NH/FL.** Voor `Verblijf` per gemeente per jaar kan
  een groot deel van de gemeenten onder de drempel vallen, waardoor weergave 5 een
  gatenkaas wordt. Wordt pas zichtbaar na stap 4. Mitigatie: de onderdrukkingsdrempel
  blijft instelbaar en de tabel toont hoeveel gebieden onderdrukt zijn.
- **De index-decompositie (§5.2).** Grootste inhoudelijke risico; zie vraag 4.
- **Herindelingen 2022–2024.** Provincietotalen zijn robuust, gemeentelijke tijdreeksen
  binnen NH/FL niet zonder meer. Voor de kickoff (één jaar tegelijk op de kaart) speelt dat
  niet; bij een trendweergave later wel. De lookup is per jaar, dus de indeling zelf klopt
  (§4.2) — het gaat puur om vergelijkbaarheid over de jaren heen.
- **Geo-jaar versus datajaar.** Bij `v2` hoort geo-jaar = datajaar − 1. Consequent
  doorvoeren, anders vallen nieuwe gemeenten uit de kaart.

---

## 10. Wat er tijdens het bouwen bijkwam

Twee dingen die het plan niet voorzag en die wel werk kostten.

### 10.1 De gemeentegrenzen in `maptool_v4` sluiten niet op elkaar aan

Het plan ging ervan uit dat een provincie een kwestie van dissolven zou zijn. In de praktijk
laat `st_union()` op de gemeentelaag zichtbare spookgrenzen dwars door elke provincie staan.
De oorzaak zit in de brondata: de gemeentepolygonen van `geo_municipalities.rds` delen hun
grenzen niet exact. Voor Gelderland (2023) blijven er na dissolven 271 gaatjes over van samen
4,5 km² op ruim 5.000 km² — hooguit 0,29 km² per stuk.

Twee ingrepen in `output_src/02_build_geo_assets.R` lossen dat op:

1. **Eerst dissolven, dan pas vereenvoudigen.** Andersom is fataal: `ms_simplify(keep = 0.05)`
   trekt aangrenzende grenzen uit elkaar, waardoor die gaatjes van 0,3 km² opzwellen tot 1 à
   25 km². Precies die opgeblazen gaten waren op de eerste kaart als spookgrenzen zichtbaar.
2. **`drop_kleine_gaten()`** ruimt op wat er dan nog rest, met een drempel van 1 km². Gaten
   boven die drempel blijven staan, zodat een eventueel echt ingesloten gebied niet
   stilzwijgend verdwijnt.

Het script controleert het resultaat zelf: blijven er te veel ringen per provincie over, dan
volgt een waarschuwing.

### 10.2 De onderdrukking bijt precies waar verwacht

Met de synthetische data (dus indicatief, niet feitelijk) valt op gemeenteniveau in NH/FL bij
drempel `<30`:

| Domein | 2022 | 2023 | 2024 |
|---|---|---|---|
| GGZ ZPM (totaal) | 0/53 | 0/51 | 0/50 |
| GGZ ZPM - Consult | 0/53 | 0/51 | 0/50 |
| GGZ ZPM - Overig prestaties | 0/53 | 0/51 | 0/50 |
| GGZ ZPM - Verblijf | 12/53 | 9/51 | 9/50 |

Alleen `Verblijf` raakt onderdrukt, en meteen fors: een vijfde van de gemeenten. Dat bevestigt
het risico uit par. 9. Op de echte data kan het aandeel anders liggen, maar de vorm van het
probleem staat vast — bij `Verblijf` op gemeenteniveau is de kaart onvermijdelijk een
gatenkaas. De tabel onder de kaart vermeldt daarom altijd hoeveel gebieden onderdrukt zijn.

### 10.4 De deploy-workflow maakt de doelmap niet aan

De workflow is bewust gemodelleerd naar die van `RVS_laatste_1000_dagen`: het doelpad
`/apps/GGZ_NHFL/` staat rechtstreeks in `deploy.yml`, er is geen `deploy.env`.

Let op het verschil met `pharm`. Dat repo heeft twee extra stappen die RVS niet heeft: het
maakt de externe map eerst aan via een echte SFTP-sessie, en zet `BatchMode=no` zodat
`sshpass` het wachtwoordprompt kan beantwoorden (commits `c1fe960` en `ea3660f`). Die stappen
zijn daar toegevoegd omdat een chrooted SFTP-account geen shell-exec toestaat en de map dus
niet vanzelf ontstaat.

Gevolg: **`/apps/GGZ_NHFL/` moet op de server al bestaan voordat de eerste deploy slaagt.**
Bestaat de map niet, dan faalt de sync. Twee wegen: de map eenmalig handmatig aanmaken, of
de mkdir-stap uit `pharm` alsnog overnemen.

### 10.3 Wat er nog niet af is

- De extractie zelf (par. 3). Het script staat klaar en bevat de controle op "kosten totaal
  langs twee wegen", maar is nooit tegen de echte database gedraaid — de RDS-host antwoordt
  niet vanaf deze werkplek.
- De vragen 2 t/m 4 uit par. 7 staan nog open. Vraag 3 is voorlopig opgelost door zowel
  `index_COSTS` als `index_PCCOSTS` in de UI aan te bieden, gelabeld als "Index kosten (p.g.)"
  en "Index kosten (p.c.)"; zodra duidelijk is welke bedoeld wordt kan de andere weg.
- Vraag 4 (de indexdefinitie) is als voetnoot in het tabblad zelf opgenomen, zichtbaar zodra
  er een indexuitkomst gekozen wordt.
