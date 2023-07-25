rule targets:
    input:
        "data/derived/serf_segment_data.csv",
        "data/derived/mxg_stem_model.rds",
        "visuals/stem_count_boxplot.png",
        "visuals/stem_cumulative_mass_fraction_vs_segment_nrate_factor.png",
        "visuals/stem_cumulative_mass_fraction_vs_segment.png",
        "visuals/stem_cumulative_mass_vs_segment_nrate_factor.png",
        "visuals/stem_mass_fraction_vs_segment_nrate_factor.png",
        "visuals/stem_mass_fraction_vs_segment.png",
        "visuals/stem_mass_vs_segment_nrate_factor.png"

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
        "visuals/stem_cumulative_mass_fraction_vs_segment_nrate_factor.png",
        "visuals/stem_cumulative_mass_fraction_vs_segment.png",
        "visuals/stem_cumulative_mass_vs_segment_nrate_factor.png",
        "visuals/stem_mass_fraction_vs_segment_nrate_factor.png",
        "visuals/stem_mass_fraction_vs_segment.png",
        "visuals/stem_mass_vs_segment_nrate_factor.png"
    shell:
        """
        {input.script}
        """