# resources #
# MC Query Guide - https://www.mediacloud.org/documentation/query-guide
# API Tutorial - https://github.com/mediacloud/api-tutorial-notebooks

# to get source IDs
library(reticulate)
os <- import("os")
mediacloud.api <- import("mediacloud.api")
MC_API_KEY = Sys.getenv("MEDIACLOUD_API_KEY")
directory_api = mediacloud.api$DirectoryApi(MC_API_KEY)
matching_sources = directory_api$source_list(name = 'washingtonpost.com')
matching_sources

# Source IDs:
# bloomberg news - 18482; cnbc.com - 1755; bloomberg - 1089; npr - 1096
# business insider - 18710; the verge - 19508; nyt - 1; wapo - 2; ft - 18886
