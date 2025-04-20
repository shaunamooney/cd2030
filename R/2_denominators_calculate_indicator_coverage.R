#' Calculate Health Coverage Indicators
#'
#' `calculate_indicator_coverage` computes key health coverage indicators across
#' specified administrative levels (national, adminlevel_1, and district). The function
#' integrates data from multiple sources, including DHIS-2, UN estimates, ANC-1,
#' and Penta-1 survey data. It calculates coverage rates for a variety of vaccinations
#' and health metrics based on projected, survey-derived, and estimated denominators.
#'
#' @param .data A `cd_data` tibble containing DHIS-2, UN, ANC-1, and Penta-1 data.
#'   This dataset must include columns for key population and vaccination metrics.
#' @param admin_level Character. Specifies the administrative level for calculations.
#'   Options include:`"national", "adminlevel_1"`, and `"district"`.
#' @param un_estimates Optional. A tibble containing UN population estimates. Required
#'   for national-level calculations.
#' @param sbr Numeric. The stillbirth rate. Default is `0.02`.
#' @param nmr Numeric. Neonatal mortality rate. Default is `0.025`.
#' @param pnmr Numeric. Post-neonatal mortality rate. Default is `0.024`.
#' @param anc1survey Numeric. Survey-derived coverage rate for ANC-1 (antenatal care, first visit). Default is `0.98`.
#' @param dpt1survey Numeric. Survey-derived coverage rate for Penta-1 (DPT1 vaccination). Default is `0.97`.
#' @param twin Numeric. Twin birth rate. Default is `0.015`.
#' @param preg_loss Numeric. Pregnancy loss rate. Default is `0.03`.
#'
#' @return A tibble of class `cd_indicator_coverage` containing calculated coverage
#'   indicators for the specified administrative level.
#'
#' @examples
#' \dontrun{
#'   # Calculate coverage indicators at the national level
#'   coverage_data <- calculate_indicator_coverage(
#'     .data = dhis2_data,
#'     admin_level = "national",
#'     un_estimates = un_data
#'   )
#'
#'   # Calculate coverage indicators at the district level
#'   coverage_data <- calculate_indicator_coverage(
#'     .data = dhis2_data,
#'     admin_level = "district"
#'   )
#' }
#'
#' @export
calculate_indicator_coverage <- function(.data,
                                         admin_level = 'national',
                                         un_estimates = NULL,
                                         sbr = 0.02,
                                         nmr = 0.025,
                                         pnmr = 0.024,
                                         anc1survey = 0.98,
                                         dpt1survey = 0.97,
                                         twin = 0.015,
                                         preg_loss = 0.03) {
  check_cd_data(.data)
  admin_level_cols <- get_admin_columns(admin_level)
  country_iso <- attr(.data, 'iso3')

  output_data <- calculate_populations(.data,
                                       admin_level = admin_level,
                                       un_estimates = un_estimates,
                                       sbr = sbr, nmr = nmr, pnmr = pnmr,
                                       anc1survey = anc1survey, dpt1survey = dpt1survey,
                                       twin = twin, preg_loss = preg_loss) %>%
    select(any_of(c(admin_level_cols, 'year')), starts_with('cov_'))

  new_tibble(
    output_data,
    class = 'cd_indicator_coverage',
    admin_level = admin_level,
    iso3 = country_iso
  )
}

calculate_populations <- function(.data,
                                  admin_level = 'national',
                                  un_estimates = NULL,
                                  sbr = 0.02,
                                  nmr = 0.025,
                                  pnmr = 0.024,
                                  anc1survey = 0.98,
                                  dpt1survey = 0.97,
                                  twin = 0.015,
                                  preg_loss = 0.03) {



  pop_dhis2 = under5_dhis2 = under1_dhis2 = livebirths_dhis2 = allbirths_dhis2 =
    wom15_49_dhis2 = year = un_under5y = un_population = un_under1y = un_wom15_49 =
    tot_under5_dhis2 = tot_pop_dhis2 = tot_under1_dhis2 = tot_wom15_49_dhis2 =
    tot_livebirths_dhis2 = tot_allbirths_dhis2 = national = totcbr_dhis2 =
    totpopgrowth = adminlevel_1 = district = totpop_dhis2 = totunder5_dhis2 =
    totunder1_dhis2 = totlivebirths_dhis2 = un_births = totwom15_49_dhis2 =
    un_popgrowth = un_cbr = totpreg_dhis2 = totinftpenta_dhis2 =
    anc1 = totpreg_anc1 = totdeliv_anc1 = totbirths_anc1 =
    totlbirths_anc1 = totinftpenta_anc1 = penta1 =
    totinftpenta_penta1 = totlbirths_penta1 = totbirths_penta1 =
    totdeliv_penta1 = totpreg_un = totinftpenta_un =
    instlivebirths = totdeliv_un = bcg = penta2 = penta3 = measles1 =
    totinftmeasles_un = measles2 = totmeasles2_un = opv1 = opv2 =
    opv3 = pcv1 = pcv2 = pcv3 = rota1 = rota2 = ipv1 = ipv2 = totdeliv_dhis2 =
    totinftmeasles_dhis2 = totmeasles2_dhis2 = totinftmeasles_penta1 =
    totinftmeasles_anc1 = totmeasles2_anc1 = totpreg_penta1 =
    otinftmeasles_penta1 = totmeasles2_penta1 = NULL

  group_vars <- get_admin_columns(admin_level)

  national_population <- prepare_population_metrics(.data, admin_level = admin_level, un_estimates = un_estimates)
  indicator_numerator <- compute_indicator_numerator(.data, admin_level = admin_level)

  output_data <- national_population %>%
    inner_join(indicator_numerator, by = c(group_vars, 'year')) %>%
    mutate(
      # DHIS2 Estimates
      totpreg_dhis2 = totlivebirths_dhis2 * (1 - 0.5* twin)/((1 - sbr)*(1 - preg_loss)),
      totdeliv_dhis2 = totpreg_dhis2 * (1 - preg_loss),
      totbirths_dhis2 = totlivebirths_dhis2/(1 - sbr),
      totinftpenta_dhis2 = totlivebirths_dhis2 - totlivebirths_dhis2 * nmr,
      totinftmeasles_dhis2 = totinftpenta_dhis2 - totinftpenta_dhis2 * pnmr,
      totmeasles2_dhis2 = totinftpenta_dhis2 - totinftpenta_dhis2 * (2 * pnmr),

      # ANC1 Estimates
      totpreg_anc1 = anc1/anc1survey,
      totdeliv_anc1 = totpreg_anc1 * (1 - preg_loss),
      totbirths_anc1 = totdeliv_anc1/(1 - 0.5 * twin),
      totlbirths_anc1 = totbirths_anc1 * (1 - sbr),
      totinftpenta_anc1 = totlbirths_anc1 * (1 - nmr),
      totinftmeasles_anc1 = totinftpenta_anc1 * (1 - pnmr),
      totmeasles2_anc1 = totinftpenta_anc1 * (1 - (2 * pnmr)),

      # Penta1 Estimates
      totinftpenta_penta1 = penta1/dpt1survey,
      totinftmeasles_penta1 = totinftpenta_penta1 * (1 - pnmr),
      totmeasles2_penta1 = totinftpenta_penta1 * (1 - (2 * pnmr)),
      totlbirths_penta1 = totinftpenta_penta1/(1 - nmr),
      totbirths_penta1 = totlbirths_penta1/(1 - sbr),
      totdeliv_penta1 = totbirths_penta1 * (1 - 0.5 * twin),
      totpreg_penta1 = totdeliv_penta1/(1 - preg_loss),
    )

  if (admin_level == 'national') {
    output_data <- output_data %>%
      mutate(
        totpreg_un = un_births * (1 - 0.5 * twin)/((1 - sbr) *(1 - preg_loss)),
        totdeliv_un = totpreg_un * (1 - preg_loss),
        totbirths_un = un_births/(1 - sbr),
        totinftpenta_un = un_births-un_births * nmr,
        totinftmeasles_un = totinftpenta_un - totinftpenta_un * pnmr,
        totmeasles2_un = totinftpenta_un-totinftpenta_un * (2 * pnmr),

        cov_anc1_un = 100 * anc1/(totpreg_un * 1000),
        cov_instlivebirths_un = 100 * instlivebirths/(un_births * 1000),
        cov_instdeliveries_un = 100 * instlivebirths/(totdeliv_un * 1000),

        cov_bcg_un = 100 * bcg/(un_births * 1000),
        cov_penta1_un = 100 * penta1/(totinftpenta_un * 1000),
        cov_penta2_un = 100 * penta2/(totinftpenta_un * 1000),
        cov_penta3_un = 100 * penta3/(totinftpenta_un * 1000),
        cov_measles1_un = 100 * measles1/(totinftmeasles_un * 1000),

        cov_measles2_un = 100 * measles2/(totmeasles2_un * 1000),

        cov_opv1_un = 100 * opv1/(totinftpenta_un * 1000),
        cov_opv2_un = 100 * opv2/(totinftpenta_un * 1000),
        cov_opv3_un = 100 * opv3/(totinftpenta_un * 1000),

        cov_pcv1_un = 100 * pcv1/(totinftpenta_un * 1000),
        cov_pcv2_un = 100 * pcv2/(totinftpenta_un * 1000),
        cov_pcv3_un = 100 * pcv3/(totinftpenta_un * 1000),

        cov_rota1_un = 100 * rota1/(totinftpenta_un * 1000),
        cov_rota2_un = 100 * rota2/(totinftpenta_un * 1000),

        cov_ipv1_un = 100 * ipv1/(totinftpenta_un * 1000),
        cov_ipv2_un = 100 * ipv2/(totinftpenta_un * 1000),

        cov_zerodose_un = 100 * ((totinftpenta_un * 1000 - penta1)/totinftpenta_un * 1000),
        # generating undervax indicators
        cov_undervax_un = 100 * ((totinftpenta_un * 1000 - penta3)/totinftpenta_un * 1000),
        # generating drop-out indicators
        cov_dropout_penta13_un = ((penta1 - penta3)/penta1) * 100,
        cov_dropout_measles12_un = ((measles1 - measles2)/measles1) * 100,
        cov_dropout_penta3mcv1_un = ((penta3 - measles1)/penta3) * 100,
        cov_dropout_penta1mcv1_un = ((penta1-measles1)/penta1) * 100
      )
  }

  output_data %>%
    # Compute coverage  based on projected lives births in DHIS-2
    mutate(
      cov_anc1_dhis2 = 100 * anc1/(totpreg_dhis2 * 1000),
      cov_instlivebirths_dhis2 = 100 * instlivebirths/(totlivebirths_dhis2 * 1000),
      cov_instdeliveries_dhis2 = 100 * instlivebirths/(totdeliv_dhis2 * 1000),

      cov_bcg_dhis2 = 100 * bcg/(totlivebirths_dhis2 * 1000),
      cov_penta1_dhis2 = 100 * penta1/(totinftpenta_dhis2 *1000),
      cov_penta2_dhis2 = 100 * penta2/(totinftpenta_dhis2 * 1000),
      cov_penta3_dhis2 = 100 * penta3/(totinftpenta_dhis2 * 1000),
      cov_measles1_dhis2 = 100 * measles1/(totinftmeasles_dhis2 * 1000),
      cov_measles2_dhis2 = 100 * measles2/(totmeasles2_dhis2 * 1000),

      cov_opv1_dhis2 = 100 * opv1/(totinftpenta_dhis2 * 1000),
      cov_opv2_dhis2 = 100 * opv2/(totinftpenta_dhis2 * 1000),
      cov_opv3_dhis2 = 100 * opv3/(totinftpenta_dhis2 * 1000),

      cov_pcv1_dhis2 = 100 * pcv1/(totinftpenta_dhis2 * 1000),
      cov_pcv2_dhis2 = 100 * pcv2/(totinftpenta_dhis2 * 1000),
      cov_pcv3_dhis2 = 100 * pcv3/(totinftpenta_dhis2 * 1000),

      cov_rota1_dhis2 = 100 * rota1/(totinftpenta_dhis2 * 1000),
      cov_rota2_dhis2 = 100 * rota2/(totinftpenta_dhis2 * 1000),

      cov_ipv1_dhis2 = 100 * ipv1/(totinftpenta_dhis2 * 1000),
      cov_ipv2_dhis2 = 100 * ipv2/(totinftpenta_dhis2 * 1000),

      cov_zerodose_dhis2 = 100 * ((totinftpenta_dhis2 * 1000 - penta1)/totinftpenta_dhis2 * 1000),
      # generating undervax indicators
      cov_undervax_dhis2 = 100 * ((totinftpenta_dhis2 * 1000 - penta3)/totinftpenta_dhis2 * 1000),
      # generating drop-out indicators
      cov_dropout_penta13_dhis2 = ((penta1 - penta3)/penta1) * 100,
      cov_dropout_measles12_dhis2 = ((measles1 - measles2)/measles1) * 100,
      cov_dropout_penta3mcv1_dhis2 = ((penta3 - measles1)/penta3) * 100,
      cov_dropout_penta1mcv1_dhis2 = ((penta1 - measles1)/penta1) * 100
    ) %>%
    # From ANC-1 Derived Denominators
    mutate(
      cov_anc1_anc1 = 100 * anc1/totpreg_anc1,
      cov_instlivebirths_anc1 = 100 * instlivebirths/totlbirths_anc1,
      cov_instdeliveries_anc1 = 100 * instlivebirths/totdeliv_anc1,

      cov_bcg_anc1 = 100 * bcg/totlbirths_anc1,
      cov_penta1_anc1 = 100 * penta1/totinftpenta_anc1,
      cov_penta3_anc1 = 100 * penta3/totinftpenta_anc1,
      cov_measles1_anc1 = 100 * measles1/totinftmeasles_anc1,
      cov_measles2_anc1 = 100 * measles2/totmeasles2_anc1,

      cov_penta2_anc1 = 100 * penta2/totinftpenta_anc1,

      cov_opv1_anc1 = 100 * opv1/totinftpenta_anc1,
      cov_opv2_anc1 = 100 * opv2/totinftpenta_anc1,
      cov_opv3_anc1 = 100 * opv3/totinftpenta_anc1,

      cov_pcv1_anc1 = 100 * pcv1/totinftpenta_anc1,
      cov_pcv2_anc1 = 100 * pcv2/totinftpenta_anc1,
      cov_pcv3_anc1 = 100 * pcv3/totinftpenta_anc1,

      cov_rota1_anc1 = 100 * rota1/totinftpenta_anc1,
      cov_rota2_anc1 = 100 * rota2/totinftpenta_anc1,

      cov_ipv1_anc1 = 100 * ipv1/totinftpenta_anc1,
      cov_ipv2_anc1 = 100 * ipv2/totinftpenta_anc1,

      cov_zerodose_anc1 = 100 * ((totinftpenta_anc1 * 1000 - penta1)/totinftpenta_anc1 * 1000),
      # generating undervax indicators
      cov_undervax_anc1 = 100 * ((totinftpenta_anc1 * 1000 - penta3)/totinftpenta_anc1 * 1000),
      # generating drop-out indicators
      cov_dropout_penta13_anc1 = ((penta1 - penta3)/penta1) * 100,
      cov_dropout_measles12_anc1 = ((measles1 - measles2)/measles1) * 100,
      cov_dropout_penta3mcv1_anc1 = ((penta3-measles1)/penta3) * 100,
      cov_dropout_penta1mcv1_anc1 = ((penta1 - measles1)/penta1) * 100
    ) %>%
    # From PENTA-1 Derived Denominators
    mutate(
      cov_anc1_penta1 = 100 * anc1/totpreg_penta1,
      cov_instlivebirths_penta1 = 100 * instlivebirths/totlbirths_penta1,
      cov_instdeliveries_penta1 = 100 * instlivebirths/totdeliv_penta1,

      cov_bcg_penta1 = 100 * bcg/totlbirths_penta1,
      cov_penta1_penta1 = 100 * penta1/totinftpenta_penta1,
      cov_penta3_penta1 = 100 * penta3/totinftpenta_penta1,
      cov_measles1_penta1 = 100 * measles1/totinftmeasles_penta1,

      cov_measles2_penta1 = 100 * measles2/totmeasles2_penta1,

      cov_penta2_penta1 = 100 * penta2/totinftpenta_penta1,

      cov_opv1_penta1 = 100 * opv1/totinftpenta_penta1,
      cov_opv2_penta1 = 100 * opv2/totinftpenta_penta1,
      cov_opv3_penta1 = 100 * opv3/totinftpenta_penta1,

      cov_pcv1_penta1 = 100 * pcv1/totinftpenta_penta1,
      cov_pcv2_penta1 = 100 * pcv2/totinftpenta_penta1,
      cov_pcv3_penta1 = 100 * pcv3/totinftpenta_penta1,

      cov_rota1_penta1 = 100 *rota1/totinftpenta_penta1,
      cov_rota2_penta1 = 100 * rota2/totinftpenta_penta1,

      cov_ipv1_penta1 = 100 * ipv1/totinftpenta_penta1,
      cov_ipv2_penta1 = 100 * ipv2/totinftpenta_penta1,

      cov_zerodose_penta1 = 100 * ((totinftpenta_penta1 * 1000 - penta1)/totinftpenta_penta1 * 1000),
      # generating undervax indicators
      cov_undervax_penta1 = 100 * ((totinftpenta_penta1 * 1000 - penta3)/totinftpenta_penta1 * 1000),
      # generating drop-out indicators
      cov_dropout_penta13_penta1 = ((penta1 - penta3)/penta1) * 100,
      cov_dropout_measles12_penta1 = ((measles1 - measles2)/measles1) * 100,
      cov_dropout_penta3mcv1_penta1 = ((penta3 - measles1)/penta3) * 100,
      cov_dropout_penta1mcv1_penta1 = ((penta1-measles1)/penta1) * 100
    )
}
