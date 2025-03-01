##
# Data
##
listOfSites <- tibble::tribble(
                             ~name,          ~type,   ~color, ~colorBorder, ~country, ~suffix, ~bboxXMin, ~bboxXMax, ~bboxYMin, ~bboxYMax, ~gridNx, ~gridNy, ~zoomin,
                  "LTER_EU_IT_102",   "lacustrine", "#03a3b8",     "#04d0eb",  "ITA",      "",         0,         0,         0,         0,    0.28,     0.4, 0,
                  "LTER_EU_IT_096",   "lacustrine", "#03a3b8",     "#04d0eb",  "ITA",  "_001",         0,         0.02,      0,         0,    0.13,    0.29, 0,
                  "LTER_EU_IT_093",   "lacustrine", "#03a3b8",     "#04d0eb",  "ITA",      "",         0,         0,         0,         0,    0.35,    0.18, 0,
                  "LTER_EU_IT_092",   "lacustrine", "#03a3b8",     "#04d0eb",  "ITA",      "",     -0.001,         0.001,      0,         0,     0.37,    0.81, 0,
                  "LTER_EU_IT_091",   "lacustrine", "#03a3b8",     "#04d0eb",  "ITA",      "",         0,         0,         0,         0,     0.27,     0.81, 0,
                  "LTER_EU_IT_090",   "lacustrine", "#03a3b8",     "#04d0eb",  "ITA",      "",         0,         0,         0,         0,     0.22,    0.8, 0,
                  "LTER_EU_IT_089",   "lacustrine", "#03a3b8",     "#04d0eb",  "ITA",      "",         0,         0,         0,         0,     0.28,    0.8, 0,
                  "LTER_EU_IT_088",   "lacustrine", "#03a3b8",     "#04d0eb",  "ITA",      "",         0,         0,         -0.001,         0.001,    0.35,    0.8, 0,
                  "LTER_EU_IT_087",   "lacustrine", "#03a3b8",     "#04d0eb",  "ITA",      "",         0,         0,         0,         0,     0.3,     0.7, 0,
                  "LTER_EU_IT_055",   "lacustrine", "#03a3b8",     "#04d0eb",  "NPL",      "",         0,         0,         0,         0,       0.2,       0.75, 0,
                  "LTER_EU_IT_054",   "lacustrine", "#03a3b8",     "#04d0eb",  "NPL",      "",         0,         0,         0,         0,       0.2,       0.75, 0,
                  "LTER_EU_IT_053",   "lacustrine", "#03a3b8",     "#04d0eb",  "ITA",      "",         0,         0,         -0.01,         0,     0.65,     0.8, 0,
                  "LTER_EU_IT_052",   "lacustrine", "#03a3b8",     "#04d0eb",  "ITA",      "",         -0.005,         0.005,         -0.005,         0.005,     0.28,     0.8, 0,
                  "LTER_EU_IT_051",   "lacustrine", "#03a3b8",     "#04d0eb",  "ITA",      "",         0,         0,         0,         0,     0.5,     0.8, 0,
                  "LTER_EU_IT_050",   "lacustrine", "#03a3b8",     "#04d0eb",  "ITA",      "",         -0.01,         0,         0,         0.01,     0.13,    0.8, 0,
                  "LTER_EU_IT_049",   "lacustrine", "#03a3b8",     "#04d0eb",  "ITA",      "",         0,         0,         0,         0,     0.3,     0.4, 0,
                  "LTER_EU_IT_048",   "lacustrine", "#03a3b8",     "#04d0eb",  "ITA",      "",         0,         0,         0,         0,     0.3,     0.3, 0,
                  "LTER_EU_IT_047",   "lacustrine", "#03a3b8",     "#04d0eb",  "ITA",      "",         0,         0,         0,         0,    0.15,    0.67, 0,
                  "LTER_EU_IT_046",   "lacustrine", "#03a3b8",     "#04d0eb",  "ITA",      "",         0,         0,         0,         0,    0.75,     0.7, 0,
                  "LTER_EU_IT_045",   "lacustrine", "#03a3b8",     "#04d0eb",  "ITA",      "",         0,         0,         0,         0,     0.7,    0.35, 0,
                  "LTER_EU_IT_044",   "lacustrine", "#03a3b8",     "#04d0eb",  "ITA",      "",         0,         0,         0,         0,    0.35,    0.75, 0,
                  "LTER_EU_IT_043",   "lacustrine", "#03a3b8",     "#04d0eb",  "ITA",      "",         0,         0,         0,         0,     0.2,    0.65, 0,
                  "LTER_EU_IT_042",   "lacustrine", "#03a3b8",     "#04d0eb",  "ITA",      "",     -0.01,      0.01,         0,         0,    0.6,    0.8, 0,
                  "LTER_EU_IT_018",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",         0,         0,         0,         0,    0.34,    0.18, 0,
                  "LTER_EU_IT_021",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",         0,         0,         0,         0,    0.18,    0.18, 0,
                  "LTER_EU_IT_022",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",   "a",         -0.05,         0.05,         0,         0,    0.22,     0.18, 0,
                  "LTER_EU_IT_022",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",   "b",     -0.01,      0.01,         0,         0,    0.1,     0.28, 0,
                  "LTER_EU_IT_023",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",         0,         0,         0,         0,    0.35,     0.18, 0,
                  "LTER_EU_IT_025",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",         0,         0,    -0.001,         0,     0.35,    0.18, 0,
                  "LTER_EU_IT_027",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",    -0.002,         0,         0,         0,    0.15,    0.35, 0,
                  "LTER_EU_IT_029",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",    -0.001,     0.001,    -0.001,    0.001,     0.35,     0.2, 0,
                  "LTER_EU_IT_031",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",    -0.001,     0.001,    -0.001,     0.001,    0.3,     0.2, 0,
                  "LTER_EU_IT_032",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",    -0.002,     0.002,     -0.002,    0.002,    0.3,     0.2, 0,
                  "LTER_EU_IT_034",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",     -0.01,      0.01,     -0.001,      0.001,     0.15,     0.28, 0,
                  "LTER_EU_IT_035",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",     -0.01,      0.01,     -0.01,      0.01,     0.3,     0.2, 0,
                  "LTER_EU_IT_036",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",     -0.01,      0.01,     -0.01,      0.01,    0.22,     0.2, 0,
                  "LTER_EU_IT_037",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",         0,         0,         0,         0,    0.18,     0.2, 0,
                  "LTER_EU_IT_073",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",    -0.001,     0.001,    -0.001,     0.001,     0.3,     0.2, 0,
                  "LTER_EU_IT_074",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",    -0.001,     0.001,    -0.001,     0.001,    0.23,     0.2, 0,
                  "LTER_EU_IT_033",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",         0,         0,         0,         0,    0.23,     0.8, 0,
                  "LTER_EU_IT_075",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",    -0.001,     0.001,    -0.001,     0.001,     0.3,     0.8, 0,
                  "LTER_EU_IT_076",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",    -0.001,     0.001,    -0.001,     0.001,    0.9,     0.75, 0,
                  "LTER_EU_IT_077",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",    -0.001,     0.001,    -0.001,     0.001,     0.7,     0.8, 0,
                  "LTER_EU_IT_078",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",    -0.001,     0.001,    -0.001,     0.001,     0.7,     0.8, 0,
                  "LTER_EU_IT_080",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",    -0.001,     0.001,    -0.001,     0.001,    0.9,    0.75, 0,
                  "LTER_EU_IT_081",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",    -0.001,     0.001,         0,         0,    0.8,     0.7, 0,
                  "LTER_EU_IT_083",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",         0,         0,    -0.001,     0.001,    0.23,    0.19, 0,
                  "LTER_EU_IT_085",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",         0,         0,         0,         0,    0.4,    0.19, 0,
                  "LTER_EU_IT_101",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",         0,         0,         0,         0,    0.5,     0.8, 0,
                  "LTER_EU_IT_109",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",         0,         0.01,         0,         0,    0.82,    0.75, 0,
                  "LTER_EU_IT_016", "transitional", "#43903f",     "#5ecc58",  "ITA",      "",         0,         0,         0,         0,    0.3,    0.18, 0,
                  "LTER_EU_IT_064", "transitional", "#43903f",     "#5ecc58",  "ITA",      "",         0,         0,         0,         0,    0.34,    0.81, 0,
                  "LTER_EU_IT_065", "transitional", "#43903f",     "#5ecc58",  "ITA",      "",         0,         0,         0,         0,    0.72,    0.8, 0,
                  "LTER_EU_IT_066", "transitional", "#43903f",     "#5ecc58",  "ITA",      "",         -0.01,         0,         0,         0,    0.35,    0.17, 0,
                  "LTER_EU_IT_095", "transitional", "#43903f",     "#5ecc58",  "ITA",      "",     -0.01,         0.01,         -0.01,         0.01,    0.8,    0.75, 0,
                  "LTER_EU_IT_105", "transitional", "#43903f",     "#5ecc58",  "ITA",      "",     -0.01,      0.01,    -0.001,     0.001,    0.3,    0.20, 0,
                  "LTER_EU_IT_100",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",         0,         0,         0,         0,    0.5,     0.8, 0,
                  # TODO "LTER_EU_IT_026",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",         0,         0,         -0.1,         0.1,    0.5,     0.8, 0,
                  "LTER_EU_IT_058",  "marine",      "#055ca8",     "#057ae1",  "ITA",      "",         -0.1,         0.1,         -0.1,         0.1,    0.2,     0.5, -11,
                  "LTER_EU_IT_059",  "marine",      "#055ca8",     "#057ae1",  "ITA",      "",         -0.1,         0.1,         -0.1,         0.1,    0.2,     0.5, -12,
                  "LTER_EU_IT_060",  "marine",      "#055ca8",     "#057ae1",  "ITA",      "",         -0.1,         0.1,         -0.1,         0.1,    0.8,     0.8, -8,
                  "LTER_EU_IT_061",  "marine",      "#055ca8",     "#057ae1",  "ITA",      "",         -0.1,         0.1,         -0.1,         0.1,    0.8,     0.5, -8,
                  "LTER_EU_IT_028",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",         -0.1,         0.001,         -0.001,         0.001,    0.5,     0.8, -7,
                  "LTER_EU_IT_039",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",         0,         0,         0,         0,    0.5,     0.8, -9,
                  "LTER_EU_IT_098",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",         0,         0,         0,         0,    0.5,     0.8, -5,
                  "LTER_EU_IT_099",  "terrestrial", "#b07c03",     "#e8a303",  "ITA",      "",         0,         0,         0,         0,    0.8,     0.5, -5,
                  "LTER_EU_IT_040", "transitional", "#43903f",     "#5ecc58",  "ITA",      "",     1,         1,         1,         1,    0.2,    0.5, -11,
                  "LTER_EU_IT_041", "transitional", "#43903f",     "#5ecc58",  "ITA",      "",     1,         1,         1,         1,    0.8,     0.5, -9,
                  "LTER_EU_IT_104", "transitional", "#43903f",     "#5ecc58",  "ITA",      "",     1,         1,         1,         1,    0.8,    0.5, -6,
                  "LTER_EU_IT_056", "marine", "#055ca8",     "#057ae1",  "ITA",      "",     0,         0,         0,         0,    0.2,    0.2, 0,
                  "LTER_EU_IT_057", "marine", "#055ca8",     "#057ae1",  "ITA",      "",     -0.1,         0,         0,         0,    0.15,    0.78, 0,
                  "LTER_EU_IT_062", "marine", "#055ca8",     "#057ae1",  "ITA",      "",     0,         0,         0,         0,    0.83,    0.78, 0,
                  "LTER_EU_IT_063", "marine", "#055ca8",     "#057ae1",  "ITA",      "",     -0.01,         0,         0,         0,    0.13,    0.5, 0,
                  "LTER_EU_IT_015", "marine", "#055ca8",     "#057ae1",  "ITA",      "",     -0.01,         0,         0,         0,    0.2,    0.2, 0,
                  "LTER_EU_IT_107", "marine", "#055ca8",     "#057ae1",  "ITA",      "",     -0.01,         0,         0,         0,    0.85,    0.6, 0
                 )
biomeColor <- tibble::tribble(
  ~geoBonBiome, ~fill, ~border,
  "marine", "#055ca8", "#057ae1",
  "coastal", "#43903f", "#5ecc58",
  "fresh_water_lakes", "#03a3b8", "#04d0eb",
  "terrestrial", "#b07c03", "#e8a303"
)
