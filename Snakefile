rule targets:
    input:
        "data/derived/serf_segment_data.csv",
        "data/derived/mxg_stem_model.rds",
        "visuals/stem_count_boxplot.png",
        "visuals/stem_count_boxplot.tiff",
        "visuals/stem_cumulative_mass_fraction_vs_segment_nrate_factor.png",
        "visuals/stem_cumulative_mass_fraction_vs_segment_nrate_factor.tiff",
        "visuals/stem_cumulative_mass_fraction_vs_segment.png",
        "visuals/stem_cumulative_mass_fraction_vs_segment.tiff",
        "visuals/stem_cumulative_mass_vs_segment_nrate_factor.png",
        "visuals/stem_cumulative_mass_vs_segment_nrate_factor.tiff",
        "visuals/stem_mass_fraction_vs_segment_nrate_factor.png",
        "visuals/stem_mass_fraction_vs_segment_nrate_factor.tiff",
        "visuals/stem_mass_fraction_vs_segment.png",
        "visuals/stem_mass_fraction_vs_segment.tiff",
        "visuals/stem_mass_vs_segment_nrate_factor.png",
        "visuals/stem_mass_vs_segment_nrate_factor.tiff",
        "visuals/cutting_height_obs_field.png",
        "visuals/cutting_height_obs_field.tiff",
        "visuals/cutting_height_obs.png",
        "visuals/cutting_height_obs.tiff"

rule read_mxg_stem:
    input: 
        script = "code/read_mxg_stem.R"
    output:
        "data/derived/serf_segment_data.csv"
    shell:
        """
        {input.script}
        """

rule model_mxg_stem_partition:
    input:
        script = "code/model_mxg_stem_partition.R",
        data = "data/derived/serf_segment_data.csv"
    output:
        "data/derived/mxg_stem_model.rds",
        "visuals/stem_count_boxplot.png",
        "visuals/stem_count_boxplot.tiff",
        "visuals/stem_cumulative_mass_fraction_vs_segment_nrate_factor.png",
        "visuals/stem_cumulative_mass_fraction_vs_segment_nrate_factor.tiff",
        "visuals/stem_cumulative_mass_fraction_vs_segment.png",
        "visuals/stem_cumulative_mass_fraction_vs_segment.tiff",
        "visuals/stem_cumulative_mass_vs_segment_nrate_factor.png",
        "visuals/stem_cumulative_mass_vs_segment_nrate_factor.tiff",
        "visuals/stem_mass_fraction_vs_segment_nrate_factor.png",
        "visuals/stem_mass_fraction_vs_segment_nrate_factor.tiff",
        "visuals/stem_mass_fraction_vs_segment.png",
        "visuals/stem_mass_fraction_vs_segment.tiff",
        "visuals/stem_mass_vs_segment_nrate_factor.png",
        "visuals/stem_mass_vs_segment_nrate_factor.tiff"
    shell:
        """
        {input.script}
        """

rule plot_cutting_height:
    input:
        script = "code/plot_mxg_cutting_height.R",
        data = "data/raw/cutting_height_obs.csv"
    output:
        "visuals/cutting_height_obs_field.png",
        "visuals/cutting_height_obs_field.tiff",
        "visuals/cutting_height_obs.png",
        "visuals/cutting_height_obs.tiff"
    shell:
        """
        {input.script}
        """