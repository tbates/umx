#   Copyright 2026- Timothy C. Bates
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
# 
#        https://www.apache.org/licenses/LICENSE-2.0
# 
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

# An institution is the lengthened shadow of one man. Emerson.

# TODO: define a folder structure for gsem projects in umx "project/data/"
umx_CheckProject <- function(project_path = "/path/to/my/project") {
    message("Status of Project folder: ")
	# TODO: does the user have the downloaded HM3 files etc needed?
	# TODO: is munge done? and they have the files needed for ldsc?
	# TODO: is ldsc done and that object is ready?
	# TODO: is the SNP file done?
  }
}

umxGSEM_GetHapMap3RefList <- function(project_path = "/path/to/my/project") {
	# promot user for project path (or default to tempdir() )if no project_path?
  dest_file <- file.path(destination, "w_hm3.snplist")
  if (!file.exists(dest_file)) {
    message("Downloading HapMap3 reference list...")
    download.file("https://raw.githubusercontent.com/bulik/ldsc/master/ldsc/w_hm3.snplist", destfile = dest_file)
  }
  return(dest_file)
}