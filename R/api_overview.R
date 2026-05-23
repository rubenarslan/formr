#' Render a participant-flow Sankey for an overview script
#'
#' Pulls the run's per-unit interaction history via
#' [formr_api_unit_sessions()] and renders a plotly Sankey diagram of
#' how participants are moving through the ordered units.
#'
#' Sankey diagrams can only draw acyclic graphs, but diary / longitudinal
#' studies revisit the same units across iterations. To stay readable
#' without losing information, the helper collapses each position to a
#' single node (counting only the first time a participant visits it)
#' and surfaces the average per-participant visit count as an "avg N
#' visits" suffix on the node label. Single-pass runs stay clutter-free
#' (no suffix when the average is ~1); diary studies show e.g. "p20:
#' Daily mood (avg 14.2 visits)" so the loop density is visible without
#' drawing it.
#'
#' Designed to be called from an OverviewScriptPage on formr.org, where
#' the server injects the per-token `.formr$access_token` / `.formr$host`
#' / `.formr$run_name` environment and [formr_api_authenticate()] picks
#' them up automatically. Outside an Overview render, set
#' `run_name` explicitly and call [formr_api_authenticate()] first.
#'
#' @param run_name Name of the run. Defaults to `.formr$run_name`,
#'   which formr.org sets when an OverviewScriptPage renders.
#' @param testing If `FALSE` (default), only real participants are
#'   included. `TRUE` to include only test sessions, `NULL` for both.
#' @param orientation Sankey orientation; `"v"` (default) renders
#'   top-to-bottom (readable on narrow admin pages), `"h"` renders
#'   left-to-right.
#' @param min_avg_visits_to_annotate Threshold above which a node's
#'   label gets the "avg N visits" suffix. Default 1.1 -- slightly above
#'   exactly-once so single-pass runs stay clean.
#' @return A plotly Sankey object, or `NULL` with a message when there
#'   are no rows to plot. Returning `NULL` lets the caller's knitr chunk
#'   gracefully display the message instead of erroring.
#' @export
formr_overview_sankey <- function(run_name = .formr$run_name,
                                  testing = FALSE,
                                  orientation = "v",
                                  min_avg_visits_to_annotate = 1.1) {

	us <- formr_api_unit_sessions(run_name, testing = testing)

	if (nrow(us) == 0) {
		message("No participants yet -- nothing to plot.")
		return(invisible(NULL))
	}

	# Special units (OverviewScriptPage / ServiceMessagePage / ReminderEmail)
	# sit outside the ordered flow and arrive with position = NA -- drop
	# them so the diagram focuses on the participant path.
	steps <- dplyr::filter(us, !is.na(.data$position))
	steps <- dplyr::arrange(steps, .data$session, .data$created, .data$unit_session_id)

	if (nrow(steps) == 0) {
		message("No participants in an ordered unit yet -- nothing to plot.")
		return(invisible(NULL))
	}

	# Per (position) average visit count across participants who reached
	# the position. Surfaces re-iteration density (diary loops) without
	# drawing the loop edge in the Sankey itself.
	visit_stats <- dplyr::count(steps, .data$session, .data$position, name = "n_visits")
	visit_stats <- dplyr::summarise(
		dplyr::group_by(visit_stats, .data$position),
		avg_visits = mean(.data$n_visits),
		.groups = "drop"
	)

	node_labels <- dplyr::distinct(steps, .data$position, .data$unit_type, .data$unit_description)
	node_labels <- dplyr::slice_head(dplyr::group_by(node_labels, .data$position), n = 1)
	node_labels <- dplyr::ungroup(node_labels)
	node_labels <- dplyr::left_join(node_labels, visit_stats, by = "position")
	node_labels <- dplyr::mutate(
		node_labels,
		label = dplyr::if_else(
			.data$avg_visits > min_avg_visits_to_annotate,
			sprintf("p%d: %s (avg %.1f visits)", .data$position,
			        dplyr::coalesce(.data$unit_description, .data$unit_type),
			        .data$avg_visits),
			sprintf("p%d: %s", .data$position,
			        dplyr::coalesce(.data$unit_description, .data$unit_type))
		)
	)
	node_labels <- dplyr::select(node_labels, "position", "label")

	# First-visit-only trajectory per session: turns the per-iteration
	# "loop back to p20" into a single node so the Sankey stays acyclic.
	# The collapsed re-visits are surfaced via the visit-count annotation
	# in node_labels above.
	trajectory <- dplyr::slice_head(dplyr::group_by(steps, .data$session, .data$position), n = 1)
	trajectory <- dplyr::ungroup(trajectory)
	trajectory <- dplyr::arrange(trajectory, .data$session, .data$created, .data$unit_session_id)
	trajectory <- dplyr::left_join(trajectory, node_labels, by = "position")
	trajectory <- dplyr::group_by(trajectory, .data$session)
	trajectory <- dplyr::mutate(
		trajectory,
		next_label = dplyr::lead(.data$label),
		terminal = dplyr::case_when(
			dplyr::row_number() != dplyr::n() ~ NA_character_,
			!is.na(.data$ended)   ~ "Completed",
			!is.na(.data$expired) ~ "Expired",
			TRUE                  ~ sprintf("Active @ %s", .data$label)
		)
	)
	trajectory <- dplyr::ungroup(trajectory)

	step_edges <- dplyr::filter(trajectory, !is.na(.data$next_label),
	                                       .data$label != .data$next_label)
	step_edges <- dplyr::count(step_edges, src = .data$label, tgt = .data$next_label, name = "value")

	terminal_edges <- dplyr::filter(trajectory, !is.na(.data$terminal))
	terminal_edges <- dplyr::count(terminal_edges, src = .data$label, tgt = .data$terminal, name = "value")

	edges <- dplyr::bind_rows(step_edges, terminal_edges)

	nodes <- unique(c(edges$src, edges$tgt))
	idx   <- setNames(seq_along(nodes) - 1L, nodes)
	n_real <- length(unique(steps$session))

	if (!requireNamespace("plotly", quietly = TRUE)) {
		stop("formr_overview_sankey() needs the 'plotly' package. ",
		     "Install it with install.packages(\"plotly\").")
	}

	plot <- plotly::plot_ly(
		type = "sankey",
		orientation = orientation,
		node = list(label = nodes, pad = 15, thickness = 15),
		link = list(source = idx[edges$src],
		            target = idx[edges$tgt],
		            value  = edges$value)
	)
	plotly::layout(
		plot,
		title = sprintf("Participant flow (n = %d, each position counted once per participant)", n_real)
	)
}
