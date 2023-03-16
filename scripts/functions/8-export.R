




################################# Export Plots #################################

purrr::pwalk(list(partnames_formatted,plist_cps_10bin), function(name,p){
  ggsave(paste0("./plots/individual_plots/",name,".pdf"), p, width = 11,
         height = 8.5, units = "in")
})
partnames_formatted <- as.list(partnames_formatted)

############################## Create Large List ###############################

biglist <- mapply(function(x,y,z){list(test_data=x,demo_data=y,changepoints=z)},
                  x=test_data,y=demo_data,z=changepoints, SIMPLIFY = F)

###################################let's render#################################


rmarkdown::render(
  input = loc,
  output_file = paste0(current$branch, "_", this_year, "_", this_month ,".pdf"),
  output_dir = "finished_reports",
  intermediates_dir = "finished_reports/tex",
  clean = TRUE,
  output_options = list(
    pdf_document = list(
      keep_tex = TRUE,
      includes = list(
        in_header = "path/to/additional_latex_styling.tex"
      )
    )
  )
)
