#   Copyright 2007-2026 Timothy C. Bates
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

#' Visualizer and Manager for your Markdown To-Do List
#'
#' @description
#' `todo` reads a Markdown to-do file, renders it with a gorgeous dark glassmorphic UI
#' (with live clock and calendar widget), and opens it in your browser (`do = "show"`).
#' Alternatively, it can open the raw markdown file directly in your default system markdown editor (`do = "open"`).
#'
#' By default, it looks for `todo.md` in the current working directory. If none is found,
#' it checks for a saved default directory (set via the `where` parameter). If that is also absent,
#' it initializes a new `todo.md` in the current working directory with a breakthrough checklist template.
#'
#' @param file The filename of the markdown to-do list (defaults to `"todo.md"`).
#' @param html The filename of the output HTML file (defaults to `"todo.html"`).
#' @param do Character. Whether to `"show"` (render HTML and open in browser) or `"open"` (open raw markdown file in default editor).
#' @param where Optional path to a directory. If specified, saves this directory as your default fallback location for todo files.
#' @param browser Logical. If `TRUE` (default), opens the HTML in browser (only applicable when `do = "show"`).
#' @return The resolved path to the HTML or Markdown file (invisibly).
#' @export
#' @family Utility functions
#' @seealso - [umx()]
#' @md
#' @examples
#' \dontrun{
#' # Render and show in browser
#' todo()
#' 
#' # Open raw markdown file in default editor
#' todo(do = "open")
#' 
#' # Save a global default directory and use it
#' todo(where = "~/Documents/todo")
#' }
todo <- function(file = "todo.md", html = "todo.html", do = c("show", "open"), where = NULL, browser = TRUE) {
	do = match.arg(do)
	config_file = path.expand("~/.umx_todo_config.txt")

	# 1. Handle "where" parameter to save default directory
	if (!missing(where) && !is.null(where)) {
		where_path = path.expand(where)
		if (!grepl("^(/|[A-Za-z]:)", where_path)) {
			where_path = file.path(getwd(), where_path)
		}
		where_path = normalizePath(where_path, winslash = "/", mustWork = FALSE)
		if (!dir.exists(where_path)) {
			dir.create(where_path, recursive = TRUE)
		}
		writeLines(where_path, config_file)
		message("Saved default to-do directory: ", where_path)
	}

	# 2. Resolve todo_path
	todo_path = path.expand(file)
	is_absolute = grepl("^(/|[A-Za-z]:)", todo_path)

	if (!is_absolute) {
		# Check if local file exists in getwd()
		local_todo = file.path(getwd(), todo_path)
		if (file.exists(local_todo)) {
			todo_path = local_todo
		} else {
			# Check for saved default directory
			if (file.exists(config_file)) {
				saved_dir = readLines(config_file, n = 1, warn = FALSE)
				if (dir.exists(saved_dir)) {
					todo_path = file.path(saved_dir, todo_path)
					message("Using default to-do directory: ", saved_dir)
				} else {
					todo_path = local_todo
				}
			} else {
				todo_path = local_todo
			}
		}
	}
	todo_path = normalizePath(todo_path, winslash = "/", mustWork = FALSE)

	# 3. Initialize file if it doesn't exist
	if (!file.exists(todo_path)) {
		message("Creating new to-do markdown file at: ", todo_path)
		
		# Default Newton template content
		initial_content = paste(c(
			"# Breakthrough 1: Light",
			"1. [ ] Weight for Plague",
			"2. [ ] Pass light through prism",
			" a. [ ] Pass that light through another prism!!! fuck yes!",
			"3. Write \"Opticks\" Hard deadline: 1704! ",
			"",
			"# Breakthrough 2: Gravity!",
			"1. [ ] Watch apple tree",
			"2. [ ] Read Kepler's paper",
			" a. [ ] Read Napier's logarithms letter! looks important: why .9999999 ???",
			"3. Write Philosophiæ Naturalis Principia Mathematica: No later than 1687!",
			"",
			"# Balls-deep big ideas 1: Transmutation",
			"1. [ ] Build laboratory furnaces",
			"2. [ ] pursue obsessively - three decades may not be enough!",
			"",
			"# Task 1: Run the mint!",
			"1. [ ]",
			"",
			"# God and existence - most important but hardest problem!",
			"1. [ ] Adopt devout but heretical Christianity",
			"2. [ ] Produce roughly 1.3 million words on religion"
		), collapse = "\n")
		
		# Ensure parent directory exists
		parent_dir = dirname(todo_path)
		if (!dir.exists(parent_dir)) {
			dir.create(parent_dir, recursive = TRUE)
		}
		writeLines(initial_content, todo_path)
	}

	# 4. Handle "do = open" (opens in default system editor)
	if (do == "open") {
		message("Opening to-do Markdown file: ", todo_path)
		if (Sys.info()['sysname'] == "Darwin") {
			system2("open", shQuote(todo_path))
		} else if (.Platform$OS.type == "windows") {
			shell.exec(todo_path)
		} else {
			system2("xdg-open", shQuote(todo_path))
		}
		return(invisible(todo_path))
	}

	# 5. Read the Markdown file content
	md_content = paste(readLines(todo_path, warn = FALSE), collapse = "\n")
	md_content_safe = gsub("</script>", "<\\/script>", md_content, ignore.case = TRUE)

	# 6. Read the HTML template (embedded here using raw string literal)
	html_template = R"---(<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>umx Task Board</title>
  <link rel="preconnect" href="https://fonts.googleapis.com">
  <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
  <link href="https://fonts.googleapis.com/css2?family=Outfit:wght@300;400;500;600;700&display=swap" rel="stylesheet">
  <style>
    :root {
      --bg-dark: #0f111a;
      --card-bg: rgba(255, 255, 255, 0.04);
      --card-border: rgba(255, 255, 255, 0.08);
      --text-main: #f8fafc;
      --text-muted: #94a3b8;
      --accent-neon: #ff6b4a; /* Vibrant accent color */
      --accent-secondary: #00f2fe; /* Alternate neon */
      --accent-green: #10b981;
    }
    
    * {
      box-sizing: border-box;
      margin: 0;
      padding: 0;
    }

    body {
      font-family: 'Outfit', sans-serif;
      background-color: var(--bg-dark);
      color: var(--text-main);
      min-height: 100vh;
      display: flex;
      flex-direction: column;
      padding: 40px;
      overflow-x: hidden;
      position: relative;
    }

    /* Glowing decorative backgrounds */
    body::before {
      content: '';
      position: absolute;
      top: -10%;
      right: -10%;
      width: 600px;
      height: 600px;
      background: radial-gradient(circle, rgba(255, 107, 74, 0.08) 0%, rgba(0,0,0,0) 70%);
      z-index: 0;
      pointer-events: none;
    }

    body::after {
      content: '';
      position: absolute;
      bottom: -10%;
      left: -10%;
      width: 600px;
      height: 600px;
      background: radial-gradient(circle, rgba(0, 242, 254, 0.06) 0%, rgba(0,0,0,0) 70%);
      z-index: 0;
      pointer-events: none;
    }

    header {
      display: flex;
      justify-content: space-between;
      align-items: center;
      margin-bottom: 30px;
      z-index: 1;
    }

    .header-left h1 {
      font-size: 2.2rem;
      font-weight: 700;
      background: linear-gradient(135deg, #fff 0%, #cbd5e1 100%);
      -webkit-background-clip: text;
      -webkit-text-fill-color: transparent;
      letter-spacing: -0.5px;
    }

    .header-left p {
      color: var(--text-muted);
      margin-top: 5px;
      font-size: 0.95rem;
    }

    .header-right {
      text-align: right;
    }

    .header-right .clock {
      font-size: 1.2rem;
      font-weight: 500;
      color: var(--text-main);
    }

    .header-right .file-path {
      font-size: 0.8rem;
      color: var(--text-muted);
      margin-top: 5px;
      font-family: monospace;
      background: rgba(255, 255, 255, 0.03);
      padding: 4px 8px;
      border-radius: 6px;
      border: 1px solid var(--card-border);
    }

    .dashboard-grid {
      display: grid;
      grid-template-columns: 2fr 1fr;
      gap: 30px;
      z-index: 1;
    }

    .main-column {
      display: flex;
      flex-direction: column;
      gap: 25px;
    }

    .sidebar-column {
      display: flex;
      flex-direction: column;
      gap: 25px;
    }

    /* Glass Card Base */
    .card {
      background: var(--card-bg);
      border: 1px solid var(--card-border);
      border-radius: 16px;
      padding: 24px;
      backdrop-filter: blur(12px);
      -webkit-backdrop-filter: blur(12px);
    }

    /* Stats Widget */
    .stats-card {
      display: flex;
      justify-content: space-around;
      align-items: center;
      padding: 20px;
    }

    .stat-item {
      text-align: center;
    }

    .stat-val {
      font-size: 1.8rem;
      font-weight: 700;
      color: var(--accent-neon);
    }

    .stat-lbl {
      font-size: 0.8rem;
      color: var(--text-muted);
      text-transform: uppercase;
      margin-top: 4px;
      letter-spacing: 0.5px;
    }

    /* Project Cards */
    .project-card {
      position: relative;
      overflow: hidden;
      transition: transform 0.2s, box-shadow 0.2s;
    }

    .project-card:hover {
      transform: translateY(-2px);
      box-shadow: 0 8px 30px rgba(0,0,0,0.3);
    }

    .project-card::before {
      content: '';
      position: absolute;
      top: 0;
      left: 0;
      width: 4px;
      height: 100%;
      background: var(--accent-neon);
    }

    .project-card.completed::before {
      background: var(--accent-green);
    }

    .project-title {
      font-size: 1.25rem;
      font-weight: 600;
      margin-bottom: 12px;
      display: flex;
      justify-content: space-between;
      align-items: center;
    }

    .project-notes {
      font-size: 0.9rem;
      color: var(--text-muted);
      margin-bottom: 16px;
      line-height: 1.4;
    }

    .tasks-list {
      display: flex;
      flex-direction: column;
      gap: 10px;
    }

    .task-item {
      display: flex;
      align-items: flex-start;
      gap: 10px;
      padding: 8px 12px;
      background: rgba(255, 255, 255, 0.02);
      border-radius: 8px;
      border: 1px solid transparent;
      transition: background 0.15s, border-color 0.15s;
    }

    .task-item:hover {
      background: rgba(255, 255, 255, 0.04);
      border-color: rgba(255, 255, 255, 0.05);
    }

    .task-item.completed {
      opacity: 0.5;
    }

    .task-item.completed .task-title {
      text-decoration: line-through;
      color: var(--text-muted);
    }

    .chk-wrapper {
      position: relative;
      display: flex;
      align-items: center;
      justify-content: center;
      margin-top: 2px;
    }

    /* Stylized checkmark display */
    .chk-indicator {
      width: 18px;
      height: 18px;
      border-radius: 4px;
      border: 1px solid var(--text-muted);
      display: inline-block;
      position: relative;
      background: transparent;
    }

    .task-item.completed .chk-indicator {
      background: var(--accent-green);
      border-color: var(--accent-green);
    }

    .task-item.completed .chk-indicator::after {
      content: '';
      position: absolute;
      left: 5px;
      top: 2px;
      width: 4px;
      height: 8px;
      border: solid white;
      border-width: 0 2px 2px 0;
      transform: rotate(45deg);
    }

    .task-title {
      font-size: 0.95rem;
      line-height: 1.4;
    }

    /* Calendar Grid */
    .calendar-header {
      display: flex;
      justify-content: space-between;
      align-items: center;
      margin-bottom: 15px;
    }

    .month-name {
      font-weight: 600;
      font-size: 1.1rem;
    }

    .today-tag {
      font-size: 0.8rem;
      background: rgba(255, 107, 74, 0.15);
      color: var(--accent-neon);
      padding: 2px 8px;
      border-radius: 20px;
      font-weight: 500;
    }

    .calendar-grid {
      display: grid;
      grid-template-columns: repeat(7, 1fr);
      gap: 6px;
      text-align: center;
    }

    .calendar-day-header {
      font-size: 0.75rem;
      color: var(--text-muted);
      font-weight: 500;
      padding-bottom: 6px;
    }

    .calendar-day-empty {
      height: 30px;
    }

    .calendar-day {
      height: 30px;
      display: flex;
      align-items: center;
      justify-content: center;
      font-size: 0.85rem;
      border-radius: 6px;
      background: rgba(255, 255, 255, 0.02);
      border: 1px solid transparent;
    }

    .calendar-day.today {
      background: rgba(255, 107, 74, 0.15);
      border: 1px solid var(--accent-neon);
      color: #fff;
      font-weight: 600;
      box-shadow: 0 0 10px rgba(255, 107, 74, 0.2);
    }

    /* Quick Guide Card */
    .guide-title {
      font-weight: 600;
      margin-bottom: 10px;
      font-size: 1rem;
    }

    .guide-text {
      font-size: 0.85rem;
      color: var(--text-muted);
      line-height: 1.5;
    }

    .guide-text code {
      background: rgba(255, 255, 255, 0.06);
      padding: 2px 4px;
      border-radius: 4px;
      font-family: monospace;
      color: var(--accent-secondary);
    }
  </style>
</head>
<body>

  <header>
    <div class="header-left">
      <h1>umx Task Board</h1>
      <p>Visual status wrapper for your local Markdown checklist</p>
    </div>
    <div class="header-right">
      <div class="clock" id="live-clock">00:00:00 AM</div>
      <div class="file-path">FILE_PATH_PLACEHOLDER</div>
    </div>
  </header>

  <div class="dashboard-grid">
    <div class="main-column">
      
      <!-- Stats Summary Card -->
      <div class="card stats-card">
        <div class="stat-item">
          <div class="stat-val" id="stats-total-proj">0</div>
          <div class="stat-lbl">Projects</div>
        </div>
        <div class="stat-item">
          <div class="stat-val" id="stats-total-tasks">0</div>
          <div class="stat-lbl">Total Tasks</div>
        </div>
        <div class="stat-item">
          <div class="stat-val" id="stats-completed-pct">0%</div>
          <div class="stat-lbl">Completion</div>
        </div>
      </div>

      <!-- Projects and Tasks Container -->
      <div id="projects-container" style="display: flex; flex-direction: column; gap: 20px;">
        <!-- Dynamically rendered project cards go here -->
      </div>

    </div>

    <div class="sidebar-column">
      
      <!-- Calendar Widget Card -->
      <div class="card" id="calendar-widget">
        <!-- Rendered calendar goes here -->
      </div>

      <!-- Quick Guide Card -->
      <div class="card">
        <div class="guide-title">💡 How to Update Tasks</div>
        <div class="guide-text">
          Edit the underlying Markdown file directly:
          <br><br>
          1. Use headings <code># Project Name</code> for new project categories.
          2. Use checkbox lines <code>- [ ] Task</code> or <code>- [x] Done Task</code> for checklist items.
          3. Save the file and re-run <code>todo()</code> in your R console to refresh this dashboard!
        </div>
      </div>

    </div>
  </div>

  <script type="text/markdown" id="markdown-raw">MD_CONTENT_PLACEHOLDER</script>

  <script>
    // Extract raw markdown content
    const rawMarkdown = document.getElementById('markdown-raw').textContent;

    // Simple markdown parser supporting various bullet formats
    function parseMarkdown(md) {
      const lines = md.split('\n');
      const projects = [];
      let currentProject = null;

      for (let line of lines) {
        const trimmed = line.trim();
        if (trimmed.startsWith('# ')) {
          currentProject = {
            title: trimmed.replace('# ', '').trim(),
            tasks: [],
            notes: []
          };
          projects.push(currentProject);
        } else if (currentProject) {
          // Check for checked box
          // Matches: - [x], * [x], 1. [x], 1 [x], a. [x], a [x], [x]
          const completedMatch = trimmed.match(/^(?:[-*]|\w+\.|\w+)?\s*\[[xX]\]\s*(.*)/);
          if (completedMatch) {
            currentProject.tasks.push({
              title: completedMatch[1].trim(),
              completed: true
            });
          } 
          // Check for unchecked box
          // Matches: - [ ], * [ ], 1. [ ], 1 [ ], a. [ ], a [ ], [ ], - [], 1 [], a [], []
          else {
            const incompleteMatch = trimmed.match(/^(?:[-*]|\w+\.|\w+)?\s*\[\s*\]\s*(.*)/);
            if (incompleteMatch) {
              currentProject.tasks.push({
                title: incompleteMatch[1].trim(),
                completed: false
              });
            } else if (trimmed.match(/^(?:[-*]|\w+\.|\w+)\s*(.*)/)) {
              // Note bullet item, e.g. - note or 1. note
              const noteMatch = trimmed.match(/^(?:[-*]|\w+\.|\w+)\s*(.*)/);
              currentProject.notes.push(noteMatch[1].trim());
            } else if (trimmed.length > 0) {
              // Regular text
              currentProject.notes.push(trimmed);
            }
          }
        }
      }
      return projects;
    }

    // Render Dashboard
    function renderDashboard() {
      const projects = parseMarkdown(rawMarkdown);
      const container = document.getElementById('projects-container');
      container.innerHTML = '';

      let totalTasks = 0;
      let completedTasks = 0;

      projects.forEach(project => {
        // Sort tasks: Incomplete first, Completed last
        project.tasks.sort((a, b) => a.completed - b.completed);

        const projectCompleted = project.tasks.length > 0 && project.tasks.every(t => t.completed);
        const card = document.createElement('div');
        card.className = `card project-card ${projectCompleted ? 'completed' : ''}`;

        let notesHtml = '';
        if (project.notes.length > 0) {
          notesHtml = `<div class="project-notes">${project.notes.join('<br>')}</div>`;
        }

        let tasksHtml = '';
        if (project.tasks.length > 0) {
          tasksHtml = `
            <div class="tasks-list">
              ${project.tasks.map(task => {
                totalTasks++;
                if (task.completed) completedTasks++;
                return `
                  <div class="task-item ${task.completed ? 'completed' : ''}">
                    <div class="chk-wrapper">
                      <span class="chk-indicator"></span>
                    </div>
                    <span class="task-title">${escapeHtml(task.title)}</span>
                  </div>
                `;
              }).join('')}
            </div>
          `;
        } else {
          tasksHtml = `<div style="color: var(--text-muted); font-size: 0.9rem; font-style: italic;">No tasks defined.</div>`;
        }

        card.innerHTML = `
          <div class="project-title">
            <span>${escapeHtml(project.title)}</span>
            ${project.tasks.length > 0 ? `
              <span style="font-size: 0.8rem; color: var(--text-muted);">
                ${project.tasks.filter(t => t.completed).length}/${project.tasks.length} Done
              </span>
            ` : ''}
          </div>
          ${notesHtml}
          ${tasksHtml}
        `;

        container.appendChild(card);
      });

      // Update Stats
      document.getElementById('stats-total-proj').textContent = projects.length;
      document.getElementById('stats-total-tasks').textContent = totalTasks;
      const pct = totalTasks > 0 ? Math.round((completedTasks / totalTasks) * 100) : 0;
      document.getElementById('stats-completed-pct').textContent = `${pct}%`;
    }

    function escapeHtml(text) {
      const map = {
        '&': '&amp;',
        '<': '&lt;',
        '>': '&gt;',
        '"': '&quot;',
        "'": '&#039;'
      };
      return text.replace(/[&<>"']/g, function(m) { return map[m]; });
    }

    // Render Calendar
    function renderCalendar() {
      const now = new Date();
      const year = now.getFullYear();
      const month = now.getMonth();
      const today = now.getDate();

      const monthNames = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
      const daysOfWeek = ["Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"];

      const firstDay = new Date(year, month, 1).getDay();
      const daysInMonth = new Date(year, month + 1, 0).getDate();

      let html = `
        <div class="calendar-header">
          <span class="month-name">${monthNames[month]} ${year}</span>
          <span class="today-tag">Today: ${today}</span>
        </div>
        <div class="calendar-grid">
      `;

      daysOfWeek.forEach(d => {
        html += `<div class="calendar-day-header">${d}</div>`;
      });

      for (let i = 0; i < firstDay; i++) {
        html += `<div class="calendar-day-empty"></div>`;
      }

      for (let d = 1; d <= daysInMonth; d++) {
        const isToday = d === today;
        html += `<div class="calendar-day ${isToday ? 'today' : ''}">${d}</div>`;
      }

      html += `</div>`;
      document.getElementById('calendar-widget').innerHTML = html;
    }

    // Clock
    function updateClock() {
      const now = new Date();
      let hours = now.getHours();
      let minutes = now.getMinutes();
      let seconds = now.getSeconds();
      const ampm = hours >= 12 ? 'PM' : 'AM';
      hours = hours % 12;
      hours = hours ? hours : 12; // the hour '0' should be '12'
      minutes = minutes < 10 ? '0'+minutes : minutes;
      seconds = seconds < 10 ? '0'+seconds : seconds;
      document.getElementById('live-clock').textContent = `${hours}:${minutes}:${seconds} ${ampm}`;
    }

    // Initialize
    renderCalendar();
    renderDashboard();
    updateClock();
    setInterval(updateClock, 1000);
  </script>
</body>
</html>
)---"

	# 7. Inject values
	html_content = gsub("FILE_PATH_PLACEHOLDER", todo_path, html_template)
	html_content = gsub("MD_CONTENT_PLACEHOLDER", md_content_safe, html_content)

	# 8. Write to destination HTML file
	if (!is.null(html)) {
		html_path = path.expand(html)
		if (!grepl("^(/|[A-Za-z]:)", html_path)) {
			html_path = file.path(dirname(todo_path), html_path)
		}
		dest_html = normalizePath(html_path, winslash = "/", mustWork = FALSE)
	} else {
		dest_html = tempfile(fileext = ".html")
	}
	writeLines(html_content, dest_html)

	# 9. Open in browser
	if (browser) {
		utils::browseURL(dest_html)
	}

	return(invisible(dest_html))
}
