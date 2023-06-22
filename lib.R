
library(devtools)

options(devtools.desc.author="Sarah Komasova <first.last@example.com> [aut, cre]")
options(devtools.desc.license="")

use_package("tidyverse", type = "depends")
use_package("haven", type = "depends")
use_package("labelled", type = "depends")
use_package("berryFunctions", type = "depends")
use_package("finalfit", type = "depends")
use_package("xlsx", type = "depends")

