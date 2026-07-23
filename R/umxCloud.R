#' Run umx / OpenMx Models on High-Core Cloud Nodes via Hetzner & Docker
#'
#' @description
#' \code{umxCloud} provisions an on-demand, high-performance Linux server on Hetzner Cloud,
#' transfers model data over secure SSH, executes optimization inside a pre-built Docker container
#' with full CPU core parallelization, retrieves the fitted model object, and automatically destroys
#' the remote server to stop billing.
#'
#' @details
#' \strong{Why use cloud compute for Structural Equation Modeling?}
#' Heavy OpenMx / umx optimizations (e.g., high-dimensional RAM models, complex GxE interaction surfaces,
#' or multi-thousand iteration bootstraps) are heavily bottlenecked by local physical core limits.
#' \code{umxCloud} allows researchers to scale up to 32 or 64 dedicated physical AMD cores on demand for
#' pennies per run without managing server infrastructure or compiling C++ dependencies from source.
#'
#' @section Step-by-Step Setup Guide for New Users:
#' \enumerate{
#'   \item \strong{Create a Hetzner Cloud Account:}
#'     \itemize{
#'       \item Visit \url{https://console.hetzner.cloud/}.
#'       \item Register for an account. \emph{Important:} Complete registration without an active VPN to pass fraud checks.
#'       \item Complete identity verification (requires credit card pre-authorization or photo ID).
#'     }
#'   \item \strong{Generate a Project API Token:}
#'     \itemize{
#'       \item Log into the Hetzner Console and create a Project (e.g., \code{"Academic Compute"}).
#'       \item In the left menu, go to \strong{Security} -> \strong{API Tokens} -> \strong{Generate API Token}.
#'       \item Select \strong{Read & Write} permissions, give it a name (e.g., \code{"umx_cloud"}), and click Generate.
#'       \item Copy the secret token string immediately.
#'     }
#'   \item \strong{Store Your Token Securely:}
#'     Do not hardcode API keys in R scripts. Store the token in your local \code{.Renviron} file:
#'     \preformatted{
#'     # In R console:
#'     usethis::edit_r_environ()
#'     
#'     # Add this line:
#'     HETZNER_API_TOKEN="your_copied_secret_token_here"
#'     }
#'     Restart R for changes to take effect. \code{umxCloud} reads this variable automatically.
#' }
#'
#' @section Estimated Cost Metrics (Dedicated AMD EPYC Cores):
#' \itemize{
#'   \item \strong{\code{ccx33} (8 Dedicated Cores / 32GB RAM):} ~€0.22 / hour (~$0.24 USD)
#'   \item \strong{\code{ccx53} (32 Dedicated Cores / 128GB RAM):} ~€0.86 / hour (~$0.93 USD)
#'   \item \strong{\code{ccx63} (64 Dedicated Cores / 256GB RAM):} ~€1.60 / hour (~$1.75 USD)
#' }
#'
#' @param model A fitted or unfitted \code{umx} or \code{OpenMx} model object.
#' @param data Optional data frame if not already contained within the model object.
#' @param apiToken Character string. Your Hetzner API key. Defaults to \code{Sys.getenv("HETZNER_API_TOKEN")}.
#' @param serverType Character. Instance size. Defaults to \code{"ccx53"} (32 Dedicated Cores).
#'   Options: \code{"ccx33"} (8 cores), \code{"ccx43"} (16 cores), \code{"ccx53"} (32 cores), \code{"ccx63"} (64 cores).
#' @param dockerImage Character. The public Docker image containing R, OpenMx, and umx.
#'   Defaults to \code{"rocker/geospatial:latest"} or a dedicated custom umx registry image.
#' @param location Character. Datacenter location: \code{"fsn1"} (Falkenstein, DE), \code{"nbg1"} (Nuremberg, DE),
#'   \code{"hel1"} (Helsinki, FI), or \code{"ash"} (Ashburn, VA, US). Defaults to \code{"fsn1"}.
#' @param autoDestroy Logical. Self-destruct server on function completion or error? Defaults to \code{TRUE}.
#'
#' @return A fitted \code{umx} / \code{OpenMx} model object returned from the remote cloud instance.
#'
#' @import httr2 ssh jsonlite
#' @export
#'
#' @examples
#' \dontrun{
#' library(umx)
#' data(twinData)
#' mzData <- subset(twinData, zygosity == "MZFF")
#' dzData <- subset(twinData, zygosity == "DZFF")
#' selDVs <- c("bmi1", "bmi2")
#' 
#' model <- umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData, sep = "")
#' 
#' # Run model on 32 dedicated cloud cores using token from .Renviron
#' fittedModel <- umxCloud(model = model, serverType = "ccx53")
#' summary(fittedModel)
#' }
umxCloud <- function(model, 
                     data = NULL, 
                     apiToken = Sys.getenv("HETZNER_API_TOKEN"), 
                     serverType = "ccx53", 
                     dockerImage = "ghcr.io/timbates/umx-cloud:latest",
                     location = "fsn1",
                     autoDestroy = TRUE) {
  
  # 1. Parameter & Dependency Checks
  if (nchar(apiToken) == 0) {
    stop("Hetzner API Token missing. Set HETZNER_API_TOKEN in .Renviron or pass apiToken explicitly.")
  }
  
  req_pkgs <- c("httr2", "ssh", "jsonlite")
  for (pkg in req_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required for umxCloud. Please install it.", pkg))
    }
  }

  message("--> [1/6] Initializing Hetzner Cloud Provisioning Sequence...")
  
  # 2. Ephemeral SSH Key Pair Generation
  temp_key_dir <- file.path(tempdir(), "umx_ssh")
  dir.create(temp_key_dir, showWarnings = FALSE)
  priv_key_path <- file.path(temp_key_dir, "id_rsa")
  pub_key_path  <- file.path(temp_key_dir, "id_rsa.pub")
  
  if (!file.exists(priv_key_path)) {
    ssh::ssh_key_generate(priv_key_path)
  }
  pub_key_string <- trimws(readLines(pub_key_path, warn = FALSE)[1])
  
  # Hetzner API Base Request
  req_base <- httr2::request("https://api.hetzner.cloud/v1") |> 
    httr2::req_headers(
      Authorization = paste("Bearer", apiToken),
      `Content-Type` = "application/json"
    )
  
  # Register temporary SSH key on Hetzner
  key_name <- paste0("umx-key-", as.integer(Sys.time()))
  key_payload <- list(name = key_name, public_key = pub_key_string)
  
  key_res <- req_base |> 
    httr2::req_url_path_append("ssh_keys") |> 
    httr2::req_body_json(key_payload) |> 
    httr2::req_perform()
  
  hetzner_key_id <- httr2::resp_body_json(key_res)$ssh_key$id
  
  # 3. Server Provisioning with Cloud-Init (Pre-installed Docker)
  server_name <- paste0("umx-run-", as.integer(Sys.time()))
  
  # Cloud-Init script: Ensures Docker is installed and ready
  cloud_init <- '
#cloud-config
package_update: true
packages:
  - docker.io
runcmd:
  - systemctl enable docker
  - systemctl start docker
'
  
  server_payload <- list(
    name = server_name,
    server_type = serverType,
    image = "ubuntu-24.04",
    location = location,
    ssh_keys = list(hetzner_key_id),
    user_data = cloud_init
  )
  
  server_res <- req_base |> 
    httr2::req_url_path_append("servers") |> 
    httr2::req_body_json(server_payload) |> 
    httr2::req_perform()
  
  server_info <- httr2::resp_body_json(server_res)
  server_id   <- server_info$server$id
  ip_address  <- server_info$server$public_net$ipv4$ip
  
  message(sprintf("--> [2/6] Instance created [ID: %s | IP: %s | Type: %s]. Waiting for boot...", 
                  server_id, ip_address, serverType))
  
  # Cleanup Hook: Guarantee server & key self-destruct even on unexpected crash
  if (autoDestroy) {
    on.exit({
      message("--> [6/6] Self-destruction active: Destroying remote server and keys...")
      
      # Delete Server
      try(req_base |> 
            httr2::req_url_path_append("servers", server_id) |> 
            httr2::req_method("DELETE") |> 
            httr2::req_perform(), silent = TRUE)
      
      # Delete SSH Key from Hetzner account
      try(req_base |> 
            httr2::req_url_path_append("ssh_keys", hetzner_key_id) |> 
            httr2::req_method("DELETE") |> 
            httr2::req_perform(), silent = TRUE)
      
      # Remove local ephemeral keys
      unlink(temp_key_dir, recursive = TRUE)
      message("--> [Complete] Cloud resources released. Billing stopped.")
    }, add = TRUE)
  }
  
  # 4. Wait for SSH & Docker Daemon Availability
  message("--> [3/6] Waiting for OS initialization and Docker daemon startup (~35 seconds)...")
  
  connected <- FALSE
  attempts  <- 0
  max_attempts <- 20
  
  while (!connected && attempts < max_attempts) {
    Sys.sleep(3)
    attempts <- attempts + 1
    session <- try(ssh::ssh_connect(paste0("root@", ip_address), keyfile = priv_key_path), silent = TRUE)
    if (!inherits(session, "try-error")) {
      connected <- TRUE
    }
  }
  
  if (!connected) {
    stop("Failed to connect to remote server via SSH within timeout limits.")
  }
  
  # Ensure Docker service is live
  ssh::ssh_exec_wait(session, command = "while ! docker info > /dev/null 2>&1; do sleep 1; done")
  
  # 5. Serialization and File Transfer
  message("--> [4/6] Pushing model payload and launching container execution...")
  
  local_input_path <- file.path(tempdir(), "cloud_input.rds")
  saveRDS(list(model = model, data = data), file = local_input_path)
  
  ssh::scp_upload(session, files = local_input_path, to = "/root/")
  
  # 6. Containerized Execution Command
  # Pulls pre-built container, mounts current path, and runs optimization using all physical cores
  docker_cmd <- sprintf(
    "docker run --rm -v /root:/work %s Rscript -e \"
      library(umx)
      library(OpenMx)
      
      # Utilize all available CPU physical cores on node
      mxOption(NULL, 'Number of Threads', parallel::detectCores())
      
      input <- readRDS('/work/cloud_input.rds')
      
      # Fit model
      if (!is.null(input$data)) {
        fit <- umxRun(input$model, data = input$data)
      } else {
        fit <- umxRun(input$model)
      }
      
      saveRDS(fit, '/work/cloud_output.rds')
    \"", dockerImage
  )
  
  message("--> [5/6] Executing model optimization on cloud CPU array...")
  ssh::ssh_exec_wait(session, command = docker_cmd)
  
  # 7. Fetch Results Back
  local_output_path <- file.path(tempdir(), "cloud_output.rds")
  ssh::scp_download(session, files = "/root/cloud_output.rds", to = tempdir())
  ssh::ssh_disconnect(session)
  
  fitted_model <- readRDS(local_output_path)
  
  # Cleanup local workspace artifacts
  file.remove(local_input_path, local_output_path)
  
  return(fitted_model)
}