server <- function(input, output,session) {

  #dashboard call
  #---------------------------------------------------------------------------------------
  dcf.dashboard::dcf_dashboard_server(
    input = input, output = output, session = session,
    tasks = c(
      "Task I.2: Nominal catches"="task-I.2",
      "Task II.1: Catch"="task-II.1",
      "Task II.2: Effort"="task-II.2",
      "Task III.1: Fleet engagement"="task-III.1"
    ),
    reporting_entity = c("Flagstates" = "flagstate"),
    ref_reporting_entities = "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/regional/wecafc/cl_flagstate.csv",
    gisviewer_url = "https://wecafc-firms.d4science.org/data-viewer/?mode=2D&baseview=World%20Imagery&views=%5B%22pid%3Drdb_firms_fishery_points%2Clyr%3Drdb_firms_fishery_points%2Cstrategy%3Dogc_filters%2Cpar%3D(AGENCY%20IN(%27WECAFC%27))%2Cgeom%3Dthe_geom%2Cgeomtype%3Dgml%3APointPropertyType%2Cop%3D1%2Cq%3Dfalse%22%2C%22pid%3Drdb_firms_resource_points%2Clyr%3Drdb_firms_resource_points%2Cstrategy%3Dogc_filters%2Cpar%3D(AGENCY%20IN(%27WECAFC%27))%2Cgeom%3Dthe_geom%2Cgeomtype%3Dgml%3APointPropertyType%2Cop%3D1%2Cq%3Dfalse%22%2C%22pid%3Dfao_area_31_capture_production%2Clyr%3Dfao_capture_production%2Cstrategy%3Dogc_viewparams%2Cpar%3Dyear%3A2011%2B2012%2B2013%2B2014%2B2015%2B2016%2B2017%2B2018%2B2019%2B2020%2B2021%3Baggregation_method%3Aavg_by_year%2Cvar%3Dmeasurement_value%2Cfun%3Dckmeans%2Cmaptype%3Dgraduated%2Cenv%3Dgeom%3Ageometry%3Bvar%3Ameasurement_value%3Bv1%3A33.18%3Bv2%3A11673.44%3Bv3%3A149207.6%3Bv4%3A243154.45%3Bv5%3A680650.27%3Bv6%3A680650.27%3Bc1%3A%23e82c3b%3Bc2%3A%23cc2331%3B%2Ccs%3DReds%2Cfill%3D%23e82c3b%2Cstroke%3D%23cc2331%2Ccount%3D53%2Cstyle%3Ddyn_geom-polygon_map-graduated_class-5%2Cgeom%3Dgeometry%2Cgeomtype%3Dgml%3AMultiPolygonPropertyType%2Cop%3D1%2Cq%3Dfalse%22%5D&extent=-140.37817293406107,-18.569372534752684,-5.378172934061055,44.27242434024731&center=-72.87817293406106,12.851525902747316&zoom=4"
  )

}
