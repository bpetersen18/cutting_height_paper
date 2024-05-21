rule targets:
    input:
        "data/internal/mxg/stem_model.rds",
        "visuals/mxg_stem_model/stem_count_boxplot.tiff",
        "data/internal/mxg/segment_rel_linear_density_stats.csv",
        "visuals/mxg_stem_model/stem_linear_density_vs_nrate_boxplot.tiff",
        "visuals/mxg_stem_model/stem_linear_density_vs_nrate_qqplot.tiff",
        "data/internal/mxg/segment_rel_linear_density_stats.csv",
        "visuals/mxg_stem_model/stem_rel_linear_density_vs_nrate_boxplot.tiff",
        "visuals/mxg_stem_model/stem_rel_linear_density_vs_nrate_qqplot.tiff",
        "visuals/mxg_stem_model/stem_linear_density_vs_segment_nrate_factor.tiff",
        "visuals/mxg_stem_model/stem_cumulative_mass_vs_segment_nrate_factor.tiff",
        "visuals/mxg_stem_model/stem_rel_linear_density_vs_segment_nrate_factor.tiff",
        "visuals/mxg_stem_model/stem_cumulative_mass_percent_vs_segment_nrate_factor.tiff",
        "visuals/mxg_stem_model/stem_rel_linear_density_vs_segment.tiff",
        "visuals/mxg_stem_model/stem_cumulative_mass_percent_vs_segment.tiff",
        "data/internal/mxg/final_linear_regression_inferences.csv",
        "visuals/cutting_height/obs_boxplot.tiff",
        "visuals/cutting_height/obs_field_boxplot.tiff",
        "data/internal/weather/serf_weather_climo_stats.csv",
        "visuals/weather/gdd_2020.tiff",
        "visuals/weather/precip_2020.tiff",
        "data/internal/mxg/model_params.csv"


rule read_mxg_stem:
    input: 
        script = "code/read_mxg_stem.R",
        data = "data/external/cutting_height_experiment_obs.xlsx"
    output:
        "data/internal/mxg/serf_segment_data.csv"
    shell:
        """
        Rscript {input.script}
        """

rule model_mxg_stem_linear_density:
    input:
        script = "code/model_mxg_stem_linear_density.R",
        data = "data/internal/mxg/serf_segment_data.csv"
    output:
        "data/internal/mxg/stem_model.rds",
        "visuals/mxg_stem_model/stem_count_boxplot.tiff",
        "data/internal/mxg/segment_linear_density_stats.csv",
        "visuals/mxg_stem_model/stem_linear_density_vs_nrate_boxplot.tiff",
        "visuals/mxg_stem_model/stem_linear_density_vs_nrate_qqplot.tiff",
        "data/internal/mxg/segment_rel_linear_density_stats.csv",
        "visuals/mxg_stem_model/stem_rel_linear_density_vs_nrate_boxplot.tiff",
        "visuals/mxg_stem_model/stem_rel_linear_density_vs_nrate_qqplot.tiff",
        "visuals/mxg_stem_model/stem_linear_density_vs_segment_nrate_factor.tiff",
        "visuals/mxg_stem_model/stem_cumulative_mass_vs_segment_nrate_factor.tiff",
        "visuals/mxg_stem_model/stem_rel_linear_density_vs_segment_nrate_factor.tiff",
        "visuals/mxg_stem_model/stem_cumulative_mass_percent_vs_segment_nrate_factor.tiff",
        "visuals/mxg_stem_model/stem_rel_linear_density_vs_segment.tiff",
        "visuals/mxg_stem_model/stem_cumulative_mass_percent_vs_segment.tiff",
        "data/internal/mxg/final_linear_regression_inferences.csv",
        "data/internal/mxg/model_params.csv"
    shell:
        """
        Rscript {input.script}
        """

rule plot_cutting_height:
    input:
        script = "code/plot_mxg_cutting_height.R",
        data = "data/external/cutting_height_obs.csv"
    output:
        "visuals/cutting_height/obs_boxplot.tiff",
        "visuals/cutting_height/obs_baled_boxplot.tiff",
        "visuals/cutting_height/obs_resid_panel.tiff",
        "visuals/cutting_height/obs_field_boxplot.tiff"
    shell:
        """
        Rscript {input.script}
        """

rule plot_weather_data:
    input:
        script = "code/plot_weather_data.R",
        data1 = "data/external/weather/columbus_junct_data.txt",
        data2 = "data/external/weather/serf_sm_mesonet.txt"
    output:
        "data/internal/weather/serf_weather_climo_stats.csv",
        "visuals/weather/gdd_2020.tiff",
        "visuals/weather/precip_2020.tiff"
    shell:
        """
        Rscript {input.script}
        """