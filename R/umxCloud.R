#' Run umx / OpenMx models on ephemeral Hetzner Cloud multicore nodes
#'
#' @description
#' \code{umxCloud} provisions an on-demand Linux server on Hetzner Cloud, transfers a model
#' over SSH, runs optimization inside a Docker container, returns the fitted model, and
#' (by default) destroys the server so billing stops when idle.
#'
#' @details
#' Designed for small labs that need more cores than a laptop for heavy OpenMx/umx fits
#' (long \code{tryHard}, large raw FIML, bootstrap grids), without keeping a permanent server.
#'
#' \strong{First smoke test:} use a cheap shared type such as \code{serverType = "cpx31"}.
#' First run often takes \strong{8–20 minutes} (VM boot, Docker install, image pull, install
#' OpenMx/umx from binaries). After plumbing works, switch to dedicated \code{ccx*} types
#' for real multicore jobs.
#'
#' The model must already contain its data (\code{mxData} / umx builders with data attached).
#' There is no remote \code{data=} argument.
#'
#' @section Step-by-Step Setup:
#' \enumerate{
#'   \item Create a Hetzner Cloud account at \url{https://console.hetzner.cloud/}
#'     (complete verification; avoid VPN during signup if fraud checks fail).
#'   \item Project → Security → API Tokens → Generate (Read & Write). Copy the token.
#'   \item Store it in \code{.Renviron} (never commit the token):
#'     \preformatted{
#'     usethis::edit_r_environ()
#'     # HETZNER_API_TOKEN="..."
#'     }
#'     Restart R. Confirm with \code{nzchar(Sys.getenv("HETZNER_API_TOKEN"))}.
#'   \item Install optional packages once: \code{install.packages(c("httr2", "ssh"))}.
#' }
#'
#' @section Cost notes (approximate, check Hetzner for current prices):
#' \itemize{
#'   \item \strong{Smoke test:} \code{cpx31} / similar shared — a few cents per short trial.
#'   \item \strong{\code{ccx33}} (8 dedicated cores): ~€0.22 / hour.
#'   \item \strong{\code{ccx53}} (32 dedicated cores): ~€0.86 / hour.
#'   \item \strong{\code{ccx63}} (64 dedicated cores): ~€1.60 / hour.
#' }
#' Default \code{autoDestroy = TRUE} deletes the server and temporary SSH key on exit
#' (including errors), unless \code{keepOnError = TRUE} leaves the box for debugging.
#'
#' @param model An \code{MxModel} (e.g. from \code{umxACE(..., autoRun = FALSE)}) with data attached.
#' @param apiToken Hetzner API token. Default \code{Sys.getenv("HETZNER_API_TOKEN")}.
#' @param serverType Hetzner server type. Default \code{"cpx31"} (cheap smoke test).
#'   Use \code{"ccx33"}, \code{"ccx43"}, \code{"ccx53"}, or \code{"ccx63"} for dedicated cores.
#' @param dockerImage Docker image with R. Default public \code{"rocker/r-ver:4.4.2"}.
#'   OpenMx and umx are installed inside the container on first use if missing.
#' @param location Datacenter: \code{"fsn1"}, \code{"nbg1"}, \code{"hel1"}, \code{"ash"}, etc. Default \code{"fsn1"}.
#' @param autoDestroy Destroy server and Hetzner SSH key on exit? Default \code{TRUE}.
#' @param tryHard Passed to remote \code{\link{umxRun}}. Default \code{"no"} for smoke tests.
#' @param maxBootMinutes Max minutes to wait for SSH + Docker readiness. Default 15.
#' @param keepOnError If \code{TRUE}, do not destroy the server when the run fails (for SSH debug).
#'   Default \code{FALSE}. Prints host IP when keeping the server.
#' @param verbose Print progress messages? Default \code{TRUE}.
#'
#' @return Fitted \code{MxModel} from the remote run.
#' @export
#' @family Twin Modeling Functions
#' @seealso \code{\link{umxRun}}, \code{\link{umxACE}}
#'
#' @examples
#' \dontrun{
#' # Requires HETZNER_API_TOKEN in .Renviron and packages httr2, ssh
#' library(umx)
#' data(twinData)
#' mz = twinData[twinData$zygosity == "MZFF", ][1:200, ]
#' dz = twinData[twinData$zygosity == "DZFF", ][1:200, ]
#' m0 = umxACE(
#'   selDVs = c("bmi1", "bmi2"),
#'   mzData = mz, dzData = dz, sep = "",
#'   autoRun = FALSE
#' )
#' # Cheap first try (~cents). Watch Hetzner console: server should appear then vanish.
#' m1 = umxCloud(m0, serverType = "cpx31", tryHard = "no")
#' summary(m1)
#'
#' # After plumbing works, use dedicated cores for heavy jobs:
#' # m1 = umxCloud(m0, serverType = "ccx53", tryHard = "yes")
#' }
umxCloud <- function(
	model,
	apiToken = Sys.getenv("HETZNER_API_TOKEN"),
	serverType = "cpx31",
	dockerImage = "rocker/r-ver:4.4.2",
	location = "fsn1",
	autoDestroy = TRUE,
	tryHard = c("no", "yes", "ordinal", "search"),
	maxBootMinutes = 15,
	keepOnError = FALSE,
	verbose = TRUE
) {
	tryHard = match.arg(tryHard)
	t0 = proc.time()[["elapsed"]]

	msg <- function(...) {
		if (isTRUE(verbose)) {
			message("--> ", paste0(..., collapse = ""))
		}
	}

	# --- Preconditions -------------------------------------------------------
	if (!nzchar(apiToken)) {
		stop("Hetzner API token missing. Set HETZNER_API_TOKEN in .Renviron or pass apiToken=.")
	}
	for (pkg in c("httr2", "ssh")) {
		if (!requireNamespace(pkg, quietly = TRUE)) {
			stop(sprintf("Package '%s' is required for umxCloud. install.packages(\"%s\")", pkg, pkg))
		}
	}
	if (!inherits(model, "MxModel")) {
		stop("model must be an OpenMx MxModel (e.g. umxACE(..., autoRun = FALSE)).")
	}

	# --- Helpers (Hetzner API) -----------------------------------------------
	reqBase = httr2::request("https://api.hetzner.cloud/v1") |>
		httr2::req_headers(
			Authorization = paste("Bearer", apiToken),
			`Content-Type` = "application/json"
		)

	apiPerform <- function(req) {
		httr2::req_perform(req)
	}

	# --- Ephemeral SSH key (unique dir per call) -----------------------------
	msg("[1/7] Creating ephemeral SSH key and registering with Hetzner...")
	tempKeyDir = tempfile(pattern = "umx_ssh_")
	dir.create(tempKeyDir, recursive = TRUE, showWarnings = FALSE)
	privKeyPath = file.path(tempKeyDir, "id_rsa")
	pubKeyPath = file.path(tempKeyDir, "id_rsa.pub")
	ssh::ssh_key_generate(privKeyPath)
	pubKeyString = trimws(readLines(pubKeyPath, warn = FALSE)[1])

	keyName = paste0("umx-key-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", sample.int(1e6, 1))
	keyRes = reqBase |>
		httr2::req_url_path_append("ssh_keys") |>
		httr2::req_body_json(list(name = keyName, public_key = pubKeyString)) |>
		apiPerform()
	hetznerKeyId = httr2::resp_body_json(keyRes)$ssh_key$id

	serverId = NULL
	ipAddress = NULL
	runFailed = FALSE
	session = NULL

	cleanupCloud <- function() {
		shouldDestroy = isTRUE(autoDestroy) && !(isTRUE(keepOnError) && isTRUE(runFailed))
		if (!shouldDestroy) {
			if (isTRUE(keepOnError) && isTRUE(runFailed) && !is.null(ipAddress)) {
				message("--> keepOnError=TRUE: server left running at root@", ipAddress,
					" (id=", serverId, "). Destroy it in the Hetzner console when done.")
			}
			return(invisible(NULL))
		}
		msg("[7/7] Destroying remote server and SSH key (billing stop)...")
		if (!is.null(serverId)) {
			try(
				reqBase |>
					httr2::req_url_path_append("servers", serverId) |>
					httr2::req_method("DELETE") |>
					apiPerform(),
				silent = TRUE
			)
		}
		try(
			reqBase |>
				httr2::req_url_path_append("ssh_keys", hetznerKeyId) |>
				httr2::req_method("DELETE") |>
				apiPerform(),
			silent = TRUE
		)
		unlink(tempKeyDir, recursive = TRUE, force = TRUE)
		msg("Cloud resources released.")
	}
	on.exit(cleanupCloud(), add = TRUE)

	# --- Create server -------------------------------------------------------
	msg("[2/7] Creating server type=", serverType, " location=", location, "...")
	cloudInit = paste(
		"#cloud-config",
		"package_update: false",
		"packages:",
		"  - docker.io",
		"runcmd:",
		"  - systemctl enable docker",
		"  - systemctl start docker",
		sep = "\n"
	)
	serverName = paste0("umx-run-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", sample.int(1e5, 1))
	serverPayload = list(
		name = serverName,
		server_type = serverType,
		image = "ubuntu-24.04",
		location = location,
		ssh_keys = list(hetznerKeyId),
		user_data = cloudInit
	)
	serverRes = reqBase |>
		httr2::req_url_path_append("servers") |>
		httr2::req_body_json(serverPayload) |>
		apiPerform()
	serverInfo = httr2::resp_body_json(serverRes)
	serverId = serverInfo$server$id
	msg("Server id=", serverId, ". Waiting until status=running and IPv4 is ready...")

	# --- Poll running + IPv4 -------------------------------------------------
	deadline = proc.time()[["elapsed"]] + maxBootMinutes * 60
	repeat {
		if (proc.time()[["elapsed"]] > deadline) {
			runFailed = TRUE
			stop("Timed out waiting for server to become running with IPv4 (maxBootMinutes=", maxBootMinutes, ").")
		}
		stRes = reqBase |>
			httr2::req_url_path_append("servers", serverId) |>
			apiPerform()
		st = httr2::resp_body_json(stRes)$server
		ipCandidate = st$public_net$ipv4$ip
		if (identical(st$status, "running") && !is.null(ipCandidate) && nzchar(ipCandidate)) {
			ipAddress = ipCandidate
			break
		}
		Sys.sleep(3)
	}
	msg("Server running at ", ipAddress)

	# --- SSH -----------------------------------------------------------------
	msg("[3/7] Waiting for SSH (deadline ", maxBootMinutes, " min from start)...")
	connected = FALSE
	while (proc.time()[["elapsed"]] <= deadline) {
		sessionTry = try(
			ssh::ssh_connect(paste0("root@", ipAddress), keyfile = privKeyPath, verbose = FALSE),
			silent = TRUE
		)
		if (!inherits(sessionTry, "try-error")) {
			session = sessionTry
			connected = TRUE
			break
		}
		Sys.sleep(3)
	}
	if (!connected) {
		runFailed = TRUE
		stop("Failed to SSH to root@", ipAddress, " within maxBootMinutes.")
	}
	on.exit({
		if (!is.null(session)) {
			try(ssh::ssh_disconnect(session), silent = TRUE)
		}
	}, add = TRUE)

	# --- Docker ready --------------------------------------------------------
	msg("[4/7] Waiting for Docker daemon (cloud-init may still be installing docker.io)...")
	dockerReady = FALSE
	while (proc.time()[["elapsed"]] <= deadline) {
		ec = try(
			ssh::ssh_exec_wait(session, command = "docker info > /dev/null 2>&1"),
			silent = TRUE
		)
		if (!inherits(ec, "try-error") && isTRUE(ec == 0L)) {
			dockerReady = TRUE
			break
		}
		Sys.sleep(3)
	}
	if (!dockerReady) {
		runFailed = TRUE
		stop("Docker not ready on remote within maxBootMinutes. cloud-init may have failed.")
	}

	# --- Payload + remote script ---------------------------------------------
	msg("[5/7] Uploading model and remote runner (docker pull + package install may take several minutes)...")
	workDir = tempfile(pattern = "umx_cloud_payload_")
	dir.create(workDir, showWarnings = FALSE)
	on.exit(unlink(workDir, recursive = TRUE, force = TRUE), add = TRUE)

	localInput = file.path(workDir, "cloud_input.rds")
	localScript = file.path(workDir, "remote_run.R")
	localOutput = file.path(workDir, "cloud_output.rds")
	localError = file.path(workDir, "cloud_error.txt")
	localStatus = file.path(workDir, "cloud_status.rds")

	payload = list(
		model = model,
		tryHard = tryHard,
		localOpenMx = as.character(utils::packageVersion("OpenMx")),
		localUmx = as.character(utils::packageVersion("umx"))
	)
	saveRDS(payload, file = localInput)

	# rocker/r-ver:4.4.x is typically Ubuntu jammy-based; Posit binary repo speeds installs
	remoteScript = paste(
		'options(repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/jammy/latest"))',
		'options(Ncpus = max(1L, parallel::detectCores(), na.rm = TRUE))',
		'',
		'for (p in c("OpenMx", "umx")) {',
		'  if (!requireNamespace(p, quietly = TRUE)) {',
		'    message("Installing ", p, " ...")',
		'    install.packages(p)',
		'  }',
		'}',
		'library(OpenMx)',
		'library(umx)',
		'',
		'n = parallel::detectCores()',
		'if (is.na(n) || n < 1) n = 1L',
		'Sys.setenv(OMP_NUM_THREADS = as.character(n))',
		'mxOption(key = "Number of Threads", value = n)',
		'',
		'input = readRDS("/work/cloud_input.rds")',
		'remoteOpenMx = as.character(packageVersion("OpenMx"))',
		'remoteUmx = as.character(packageVersion("umx"))',
		'if (!identical(remoteOpenMx, input$localOpenMx) || !identical(remoteUmx, input$localUmx)) {',
		'  warning("Package versions differ local vs remote: OpenMx ",',
		'    input$localOpenMx, " vs ", remoteOpenMx, "; umx ",',
		'    input$localUmx, " vs ", remoteUmx)',
		'}',
		'',
		'fit = tryCatch(',
		'  umxRun(input$model, tryHard = input$tryHard, summary = FALSE),',
		'  error = function(e) {',
		'    saveRDS(list(ok = FALSE, error = conditionMessage(e)), "/work/cloud_status.rds")',
		'    writeLines(conditionMessage(e), "/work/cloud_error.txt")',
		'    quit(save = "no", status = 1)',
		'  }',
		')',
		'saveRDS(list(ok = TRUE), "/work/cloud_status.rds")',
		'saveRDS(fit, "/work/cloud_output.rds")',
		sep = "\n"
	)
	writeLines(remoteScript, localScript)

	ssh::scp_upload(session, files = c(localInput, localScript), to = "/root/", verbose = FALSE)

	dockerCmd = sprintf(
		"docker run --rm -v /root:/work %s Rscript /work/remote_run.R",
		shQuote(dockerImage, type = "sh")
	)
	msg("[6/7] Executing model in container: ", dockerImage)
	exitCode = ssh::ssh_exec_wait(session, command = dockerCmd)
	if (!isTRUE(exitCode == 0L)) {
		runFailed = TRUE
		errMsg = "Remote docker/Rscript failed."
		# Best-effort fetch diagnostics
		try(ssh::scp_download(session, files = "/root/cloud_error.txt", to = workDir, verbose = FALSE), silent = TRUE)
		try(ssh::scp_download(session, files = "/root/cloud_status.rds", to = workDir, verbose = FALSE), silent = TRUE)
		if (file.exists(localError)) {
			errMsg = paste(readLines(localError, warn = FALSE), collapse = "\n")
		} else if (file.exists(localStatus)) {
			st = try(readRDS(localStatus), silent = TRUE)
			if (!inherits(st, "try-error") && is.list(st) && !is.null(st$error)) {
				errMsg = st$error
			}
		}
		stop("umxCloud remote run failed (exit ", exitCode, "): ", errMsg)
	}

	ssh::scp_download(session, files = "/root/cloud_output.rds", to = workDir, verbose = FALSE)
	if (!file.exists(localOutput)) {
		runFailed = TRUE
		stop("Remote run reported success but cloud_output.rds was not retrieved.")
	}
	fittedModel = readRDS(localOutput)
	if (!inherits(fittedModel, "MxModel")) {
		runFailed = TRUE
		stop("cloud_output.rds did not contain an MxModel.")
	}

	elapsedMin = (proc.time()[["elapsed"]] - t0) / 60
	msg(sprintf(
		"Done in %.1f min. serverType=%s. Check Hetzner console that the server is gone if autoDestroy=TRUE.",
		elapsedMin, serverType
	))
	return(fittedModel)
}
