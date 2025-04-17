

pairwise_comparisons <- function(model, prior_model = NULL, specs, interaction = FALSE) {
  
  packages = c("bayestestR", "emmeans")
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  }
  
  if (is.null(prior_model)) prior_model <- bayestestR::unupdate(model, verbose = TRUE)
  
  for (i in 1:length(specs)) {
    
    par = as.formula(specs[i])
    
    if (interaction) {
      post_contrasts <- emmeans::emmeans(model, par) %>% 
        contrast(interaction = c("pairwise","pairwise"))
      prior_contrasts <- emmeans::emmeans(prior_model, par) %>% 
        contrast(interaction = c("pairwise","pairwise"))
    } else {
      post_contrasts <- pairs(emmeans::emmeans(model, par))
      prior_contrasts <- pairs(emmeans::emmeans(prior_model, par))
    }
    
    print(bayestestR::bayesfactor_parameters(post_contrasts, prior_contrasts))
    
  }
  
}
