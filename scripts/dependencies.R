renv::install('tidyverse')
renv::install('caret')
renv::install('Hmisc')
renv::install('ggpubr')
renv::install('rstatix')

# -----------------------------------
# Don't run the code in this section.
renv::snapshot()
# -----------------------------------

# -----------------------------------
# RUN THIS!!!
renv::hydrate()
