# frcadastre<img src="man/figures/frcadastre_logo.png" align="right" height="138" alt="" />

<!-- badges: start -->
  [![R-CMD-check](https://github.com/mucau/frcadastre/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mucau/frcadastre/actions/workflows/R-CMD-check.yaml)
  [![CRAN status](https://www.r-pkg.org/badges/version/frcadastre)](https://CRAN.R-project.org/package=frcadastre)
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## The French cadastre
### Short definition

The **cadastre** is the set of maps, registers and databases that describe land parcels : their boundaries, surface, and usage.  
In France, the cadastre is managed by the [**Direction Generale des Finances Publiques (DGFiP)**](https://www.economie.gouv.fr/dgfip). It is primarily used as a fiscal tool to calculate local property taxes, but also as a spatial reference dataset for urban planning, GIS analysis, and land management.  

It is important to note that the cadastre **has no legal value of property title**: boundaries are indicative and do not replace legal surveying.  

### Short history  
- The modern French cadastre was created under Napoleon in the early 19th century to establish a uniform taxation base.  
- Earlier cadastral-like systems existed (terrains registers, feudal maps), but without national standardization.  
- Since the 1980s, progressive digitization led to the **computerized cadastral plan (PCI = Plan Cadastral Informatise)**.  
- Today, nearly all of metropolitan France is covered digitally (except Strasbourg, for historical reasons).  
- Open data initiatives now provide public access through [https://cadastre.data.gouv.fr](https://cadastre.data.gouv.fr).   

---

## Available cadastral datasets

Different products exist, each with specific use cases and limitations:

- [**PCI (Plan Cadastral Informatise – Vector & Image)**](https://cadastre.data.gouv.fr/datasets/plan-cadastral-informatise  ): produced by DGFiP in EDIGEO and DXF formats. This is the "raw" dataset, continuously updated. It is the reference but difficult to use (complex format, heavy, split into sheets).  

- [**PCI Etalab**](https://cadastre.data.gouv.fr/datasets/cadastre-etalab): conversion of PCI raw data into more user-friendly formats (GeoJSON, Shapefile, …). Updated at the same frequency as PCI. Still split by departments or communes.

- [**PCI Express**](https://geoservices.ign.fr/parcellaire-express-pci): produced by IGN. A quarterly-updated assembly of PCI data, ideal for searching data by wrapping location method.  

- [**BD Parcellaire**](https://geoservices.ign.fr/bdparcellaire): an IGN product, where PCI was geometrically adjusted. No longer updated since 2018.  

- [**RPCU (Referentiel Parcellaire Cadastral Unique)**](https://geoservices.ign.fr/rpcu): joint project between DGFiP and IGN aiming at producing a unified, geometrically corrected dataset. Still a prototype, not usable yet. Tools such as *RPCUtools* allow users to apply their own geometric adjustments.

To get PCI Express or BD Parcellaire, you can use the amazing [happign package](https://github.com/paul-carteron/happign). To use PCI and PCI Etalab, stay here ! :)

### Focus on PCI

Key facts about PCI (computerized cadastral plan):  

- Around **600,000 sheets (plans)** covering almost the whole territory (except Strasbourg).  
- Two versions: **Vector PCI** (EDIGEO format) and **Image PCI** (scans of historical paper maps).  
- Continuously updated by DGFiP.  
- EDIGEO is rich but complex: many codes, hierarchical structures, and a format that is not directly GIS-friendly.  
- Working with PCI requires:  
  - downloading many files (by sheet, commune, or department),  
  - parsing EDIGEO or DXF.

### Focus on PCI Etalab

The **Etalab cadastre** is a simplified and standardized version of PCI, intended for open data and GIS use.  

- Built from PCI EDIGEO data (and Strasbourg's local cadastre).  
- Entities include parcels, fiscal subdivisions, sections, sheets, municipalities, buildings, and place names.  
- Provided as **GeoJSON** (compressed or not) and **Shapefile**.  
- Download available at different levels: commune, department, or entire France.  
- Data are juxtaposed sheets, without geometric corrections (possible overlaps at boundaries).  

---

## The frcadastre package

### Purpose

The **frcadastre** R package aims to:  
1. Automate downloading of cadastral datasets (PCI and PCI Etalab).  
2. Provide functions to directly access ready-to-use data.  
3. Facilitate reproducible workflows for GIS and spatial analysis in R.  

### Main features

For a quickly use, you can run directly `get_etalab()` to get `sf` objects from PCI Etalab.

If you want to access the initial (more complete) raw data, you can use:
- `get_pci()`: download PCI data (in DXF or EDIGEO format);
- `get_etalab()`: download PCI Etalab raw datasets (in compressed GeoJSON format) 
by asking raw layer (use `get_etalab_layernames("raw")`)

You have also some utility functions to manage IDU for cadastral parcelles.

### Getting start

```r
# Just run
install_github("mucau/frcadastre")

# To get a commune, just ask with his INSEE code
commune <- get_etalab(72187, data="communes")

# Continue with parcels ...
parcels <- get_etalab(72187, data="parcelles")

# And some raw data ...
borne <- get_etalab_raw(72187, data="borne")
```

### Problems and Issues

- Please report any issues or bugs you may encounter on the [dedicated
  page on github](https://github.com/mucau/frcadastre/issues).

---

Have fun ! :)
