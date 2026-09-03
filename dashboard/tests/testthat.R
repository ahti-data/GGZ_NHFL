library(testthat)

# auth.R is bewust niet overgenomen uit het template: toegang tot dit dashboard
# loopt server-side via Authelia, niet via shinymanager (zie PLAN.md par. 1.4).
source("../utils/format_thinkcell_download.R")
source("../utils/slide_download.R")
source("../utils/template_admin.R")
source("../utils/favorites.R")
source("../utils/export_history.R")
source("../utils/ggz_data.R")
source("../utils/ggz_map.R")

test_dir("testthat")
