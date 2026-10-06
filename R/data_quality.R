#' Read the records of formr `data_quality` items
#'
#' A survey page that holds a `data_quality` item stores one record for that
#' page: a JSON object of counts, durations and yes/no values about the browser
#' and about how the answers were made. `dq_parse()` turns such records into a
#' data frame with one row per page.
#'
#' Records never hold what was typed, mouse paths, clipboard content, an IP
#' address or a device fingerprint. Their fields are described in formr's
#' documentation for administrators.
#'
#' @param x a character vector of JSON records, typically the `data_quality`
#'   columns of one participant. Missing and empty values, and values that are
#'   not JSON, are dropped.
#' @return A data frame with one row per record and one column per field; an
#'   empty data frame if `x` holds no record.
#' @seealso [dq_flags()] to turn the records of one participant into
#'   indicators, [dq_flags_by_session()] to do so for a whole results table.
#' @export
#' @examples
#' dq_parse(c('{"v":2,"ld":1,"keys":12,"pd":3}', NA, '{"v":2,"ld":1,"keys":40,"pd":5}'))
dq_parse <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  rows <- lapply(x, function(s) {
    j <- tryCatch(jsonlite::fromJSON(s), error = function(e) NULL)
    if (!is.list(j) || !length(j)) return(NULL)
    as.data.frame(lapply(j, function(v) if (length(v) == 0) NA else v), stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  cols <- unique(unlist(lapply(rows, names)))
  do.call(rbind, lapply(rows, function(r) { r[setdiff(cols, names(r))] <- NA; r[cols] }))
}

# Adler-32 checksum (hex) of a string, the same as the `uah` field of a record,
# which holds the checksum of the User-Agent the page read.
dq_adler32 <- function(x) {
  x <- utils::tail(x, 1)
  if (!length(x) || is.na(x)) return(NA_character_)
  a <- 1; b <- 0
  for (ch in utf8ToInt(x)) { a <- (a + ch) %% 65521; b <- (b + a) %% 65521 }
  # as JavaScript prints (b * 65536 + a).toString(16)
  if (b == 0) sprintf("%x", as.integer(a)) else sprintf("%x%04x", as.integer(b), as.integer(a))
}

# The operating system a User-Agent names, and the one the Sec-CH-UA-Platform
# client hint names.
dq_ua_os <- function(ua) {
  if (grepl("Windows", ua)) "win"
  else if (grepl("Android", ua)) "android"
  else if (grepl("iPhone|iPad|iPod", ua)) "ios"
  else if (grepl("CrOS", ua)) "cros"
  else if (grepl("Macintosh|Mac OS X", ua)) "mac"
  else if (grepl("Linux", ua)) "linux"
  else "other"
}
dq_hint_os <- function(hint) {
  h <- gsub('"', "", trimws(hint))
  known <- c(macOS = "mac", Windows = "win", Linux = "linux", Android = "android", iOS = "ios",
             "Chrome OS" = "cros", "Chromium OS" = "cros")
  if (h %in% names(known)) known[[h]] else ""
}

#' Estimate the share of AI-assisted responses from a randomised-response question
#'
#' For a question of the form "answer Yes if at least one of these is true: you
#' have an identical twin; you are an AI agent; you used a bot, a script or an
#' AI for some of your answers". Nobody can tell from a single Yes which
#' statement applies, which makes it safe to answer honestly; across a sample,
#' the known rate of the harmless statement gives the share of the others.
#'
#' Everyone the sensitive statements apply to (share `pi`) answers Yes whether
#' or not they are a twin, everyone else only if they are one (rate `p`), so
#' `P(Yes) = pi + (1 - pi) * p` and `pi = (P(Yes) - p) / (1 - p)`. The only
#' assumption is that the harmless statement applies to the others at rate `p`.
#' With a small sample the estimate can be below zero.
#'
#' @param yes a logical or 0/1 vector, `TRUE` for "Yes". Missing values are
#'   dropped.
#' @param p the probability of the harmless statement. The default is the
#'   share of people who are identical twins (about four monozygotic twin
#'   pairs per 1,000 births).
#' @return A named numeric vector: `n`, `yes_rate`, the estimated share
#'   `ai_share`, its standard error `se`, and the bounds `lower` and `upper`
#'   of a 95% confidence interval.
#' @export
#' @examples
#' answers <- c(rep(TRUE, 12), rep(FALSE, 288))
#' dq_rrt(answers)
dq_rrt <- function(yes, p = 0.008) {
  yes <- as.logical(yes[!is.na(yes)])
  n <- length(yes)
  lambda <- mean(yes)
  est <- (lambda - p) / (1 - p)
  se <- sqrt(lambda * (1 - lambda) / n) / (1 - p)
  c(n = n, yes_rate = lambda, ai_share = est, se = se, lower = est - 1.96 * se, upper = est + 1.96 * se)
}

#' Indicators of automated or careless responding for one participant
#'
#' Sums the page records of one participant (see [dq_parse()]) and reports,
#' per indicator, what was observed and whether it crosses the threshold at
#' which it is worth a look.
#'
#' No single indicator proves anything. Several input indicators together, or
#' any text in an `agent_probe` field, are strong signs that a program made the
#' input; the attention indicators (leaving the page, copying and pasting) are
#' context about how a person answered. Review flagged participants by hand
#' before excluding anyone.
#'
#' @param d a data frame of page records, as returned by [dq_parse()].
#' @param probe what was stored in the participant's `agent_probe` items, as
#'   one string (people never see these fields, so any text is flagged).
#' @param srv_ua the User-Agent header stored by a `browser` item. It is
#'   compared with the User-Agent the page read.
#' @param sig_agent the `Signature-Agent` header stored by a
#'   `server HTTP_SIGNATURE_AGENT` item. AI agents that identify themselves
#'   send it.
#' @param ch_platform,ch_mobile the client hints stored by
#'   `server HTTP_SEC_CH_UA_PLATFORM` and `server HTTP_SEC_CH_UA_MOBILE` items.
#'   They are compared with `srv_ua`: a User-Agent rewritten on its way to the
#'   server leaves them as they were.
#' @return A data frame with one row per indicator and the columns `group`
#'   (`"environment"`: the browser and the request, `"input"`: how clicks and
#'   text were made, `"attention"`: time, leaving the page, copy and paste),
#'   `indicator`, `value` (what was observed, as text) and `flag` (`TRUE` when
#'   the indicator crosses its threshold; purely informative indicators are
#'   never flagged). If `d` has no record, a single unflagged row says so.
#' @seealso [dq_flags_by_session()] for a whole results table.
#' @export
#' @examples
#' records <- c(
#'   '{"v":2,"ld":1,"t":41000,"keys":0,"txt_ch":80,"fld_nokey":1,"pd":4,"pd_ctr":4}',
#'   '{"v":2,"ld":1,"t":9000,"keys":0,"txt_ch":0,"pd":2,"pd_ctr":2}'
#' )
#' flags <- dq_flags(dq_parse(records), probe = "yes, a language model")
#' flags[flags$flag, ]
dq_flags <- function(d, probe = NA, srv_ua = NA, sig_agent = NA, ch_platform = NA, ch_mobile = NA) {
  if (!is.data.frame(d) || !nrow(d)) {
    return(data.frame(group = "environment", indicator = "no data quality record", value = NA_character_,
                      flag = FALSE, stringsAsFactors = FALSE))
  }
  s <- function(k) sum(as.numeric(d[[k]]), na.rm = TRUE)
  # the largest value over the pages; 0 for a field the records do not have
  mx <- function(k) {
    v <- suppressWarnings(as.numeric(d[[k]]))
    v <- v[!is.na(v)]
    if (length(v)) max(v) else 0
  }
  txt <- function(k) paste(unique(unlist(strsplit(stats::na.omit(as.character(d[[k]])), ","))), collapse = ",")
  share <- function(a, b) if (b > 0) round(a / b, 2) else NA
  # the first page's value of a field, "?" when the records do not have it
  one <- function(k) {
    v <- d[[k]]
    if (is.null(v) || !length(v) || is.na(v[1])) "?" else as.character(v[1])
  }
  pd <- s("pd")
  ins <- s("ins_nokey_ch"); tch <- s("txt_ch")
  kcv <- suppressWarnings(min(as.numeric(d$kcv[as.numeric(d$kcv) >= 0]), na.rm = TRUE))
  kwb <- suppressWarnings(min(as.numeric(d$kwb[as.numeric(d$kwb) >= 0]), na.rm = TRUE))
  last <- function(x) { x <- utils::tail(x, 1); if (!length(x) || is.na(x)) "" else as.character(x) }
  js_uah <- unique(stats::na.omit(as.character(d$uah)))
  ua_diff <- nzchar(last(srv_ua)) && length(js_uah) > 0 && !(dq_adler32(last(srv_ua)) %in% js_uah)
  probe_txt <- if (length(probe) && !is.na(probe[1]) && nzchar(probe[1])) probe[1] else ""
  ua <- last(srv_ua); sig <- last(sig_agent); hint_os <- dq_hint_os(last(ch_platform)); hint_mob <- trimws(last(ch_mobile))
  ua_os <- if (nzchar(ua)) dq_ua_os(ua) else ""
  # Android reports itself as Linux in places, as the page's own check allows too
  os_diff <- nzchar(ua_os) && nzchar(hint_os) && ua_os != "other" && ua_os != hint_os &&
    !(ua_os %in% c("android", "linux") && hint_os %in% c("android", "linux"))
  mob_diff <- nzchar(ua) && hint_mob %in% c("?0", "?1") && (hint_mob == "?1") != grepl("Mobi", ua)
  environment <- list(
    c("navigator.webdriver true", mx("wd"), mx("wd") > 0),
    c("webdriver flag tampered", mx("wdt"), mx("wdt") > 0),
    c("headless UA / no window.chrome / no plugins / no languages / permission mismatch",
      paste(mx("hl"), mx("nochr"), mx("plg0"), mx("lang0"), mx("perm"), sep = "/"),
      mx("hl") + mx("nochr") + mx("plg0") + mx("lang0") + mx("perm") > 0),
    c("no browser window when first interacting", mx("ow0"), mx("ow0") > 0),
    c("window larger than screen (spoofed screen)", mx("winscr"), mx("winscr") > 0),
    c("screen size = viewport (emulated device)", mx("scrvp"), NA),
    c("software WebGL renderer", mx("swgl"), mx("swgl") > 0),
    c("automation globals", txt("glob"), nzchar(txt("glob"))),
    c("agent/AI-assistant names in injected DOM", txt("ext"), nzchar(txt("ext"))),
    c("extension resources injected into page", s("ext_res"), NA),
    c("accessibility setting active (forced colours, high contrast, reduced motion)", mx("a11y"), NA),
    c("AI browser brand", txt("aib"), nzchar(txt("aib"))),
    c("agent_probe field filled", probe_txt, nzchar(probe_txt)),
    c("user-agent incoherent (UA vs platform vs client hints)", txt("uac"), nzchar(txt("uac"))),
    c("User-Agent header differs from navigator.userAgent", if (nzchar(last(srv_ua))) as.integer(ua_diff) else NA, ua_diff),
    c("request signed by an agent (Signature-Agent header)", sig, nzchar(sig)),
    c("client hints name another system than the User-Agent header (header / hint)",
      if (nzchar(ua_os) && nzchar(hint_os)) paste(ua_os, hint_os, sep = " / ") else NA, os_diff),
    c("client hints and User-Agent header disagree about mobile (hint)", if (nzchar(ua) && nzchar(hint_mob)) hint_mob else NA, mob_diff),
    c("real JS engine differs from the engine the UA claims (real vs claimed)", paste(txt("eng"), txt("eng_ua"), sep = " vs "), mx("eng_mm") > 0),
    c("UA Chrome version outside the range its JS features allow", mx("ver_mm"), mx("ver_mm") > 0),
    c("claims Google Chrome but has no Widevine (automation launch or non-Google build)", mx("chr_nowv"), mx("chr_nowv") > 0),
    c("DRM key systems available (wv Widevine, pr PlayReady, fp FairPlay; na not checked; blocked by a Permissions-Policy)", txt("drm"), NA)
  )
  input <- list(
    c("script-made events that are not the page's own", s("untr"), s("untr") > 0),
    c("answers changed without click/tap/key", s("prog"), s("prog") > 0),
    c("text answers mostly not typed or pasted (fields)", s("fld_nokey"), s("fld_nokey") > 0),
    c("text inserted without key presses (events / chars / other inserts)", paste(s("ins_nokey"), ins, s("ins_other"), sep = " / "), ins >= 20 && ins > 0.5 * tch),
    c("share of text typed via keys (keys / chars in fields)", share(s("keys"), tch), tch >= 20 && s("keys") < 0.5 * tch),
    c("key intervals < 12 ms (of intervals)", paste(s("kfast"), s("kn"), sep = " / "), s("kn") >= 10 && s("kfast") > 0.3 * s("kn")),
    c("key dwell < 8 ms", s("kdwf"), s("kn") >= 10 && s("kdwf") > 0.5 * s("keys")),
    c("typing rhythm CV within bursts (min over pages; ~0 = metronome)", if (is.finite(kcv)) kcv else NA, is.finite(kcv) && kcv < 0.15),
    c("pause before new word / within word (median ratio within bursts; info)", if (is.finite(kwb)) kwb else NA, NA),
    c("long pauses (0.5-5 s) at word starts: share / expected share / long pauses",
      paste(share(s("klp_ws"), s("klp")), share(s("kws"), s("kwc")), s("klp"), sep = " / "),
      s("klp") >= 8 && s("kwc") > 0 && s("klp_ws") / s("klp") < 1.3 * s("kws") / s("kwc")),
    c("corrections (Backspace/Delete) / typed keys", paste(s("bksp"), s("keys"), sep = " / "), s("keys") >= 300 && s("bksp") == 0),
    c("capitals: Shift not held / no Shift key press / capitals", paste(s("kcap_ns"), s("kcap_nosk"), s("kcap"), sep = " / "), s("kcap_ns") + s("kcap_nosk") > 0),
    c("non-ASCII letters inserted without their key", s("ins_na"), s("ins_na") > 0),
    c("key presses without key code", s("kcode0"), s("kcode0") >= 3),
    c("mouse presses / at exact centre", paste(pd, s("pd_ctr"), sep = " / "), pd >= 3 && s("pd_ctr") > 0.5 * pd),
    c("mouse presses with <= 2 moves in prior second", s("pd_fewmv"), pd >= 3 && s("pd_fewmv") > 0.8 * pd),
    c("mouse presses with screen == client coordinates", s("pd_scr"), pd >= 1 && s("pd_scr") == pd),
    c("clicks released within 25 ms / clicks (info: trackpad tap-to-click does this too)", paste(s("pd_hold0"), s("pu"), sep = " / "), NA),
    c("clicks within 200 ms of target scrolling into view", s("pd_newview"), pd >= 2 && s("pd_newview") > 0.3 * pd),
    c("pointer moves with coalesced samples (share; device-dependent)", if (mx("mv_coal") < 0) "unsupported" else share(s("mv_coal"), s("mv")), NA),
    c("pointer moves without movementX/Y", s("mv_mz"), s("mv") >= 10 && s("mv_mz") > 0.5 * s("mv")),
    c("touches with radius <= 1 px / touches", paste(s("tap_r1"), s("tap"), sep = " / "), s("tap") >= 3 && s("tap_r1") == s("tap")),
    c("pointer moves / jumps > 250 px", paste(s("mv"), s("mv_jump"), sep = " / "), s("mv") > 0 && s("mv_jump") > 0.3 * s("mv")),
    c("scrolls without wheel/touch/key", paste(s("scr_nogest"), s("scr"), sep = " / "), s("scr") >= 3 && s("scr_nogest") > 0.8 * s("scr")),
    c("focus moved without click/key", s("foc_nogest"), s("foc_nogest") > 1),
    c("focus() called by code that is not the page's (run in the page by a tool)", s("foc_prog"), s("foc_prog") > 0),
    c("clicks without a pointer (not Enter/Space or label)", s("clk_nopt"), s("clk_nopt") > 0),
    c("actions while page hidden / unfocused", paste(s("act_hid"), s("act_unf"), sep = " / "), s("act_hid") + s("act_unf") > 0),
    c("info bar / side panel opened", paste(s("rsz_bar"), s("rsz_side"), sep = " / "), s("rsz_bar") + s("rsz_side") > 0)
  )
  attention <- list(
    c("seconds on pages", round(s("t") / 1000), NA),
    c("tab hidden: times / seconds", paste(s("hid_n"), round(s("hid_ms") / 1000), sep = " / "), s("hid_n") > 0),
    c("window unfocused: times / seconds", paste(s("blur_n"), round(s("blur_ms") / 1000), sep = " / "), s("blur_n") > 0),
    c("copy (of question text) / chars", paste0(s("copy"), " (", s("copy_q"), ") / ", s("copy_ch")), s("copy_q") > 0),
    c("paste / chars (share of text in fields)", paste0(s("paste"), " / ", s("paste_ch"), " (", share(s("paste_ch"), tch), ")"), s("paste") > 0),
    c("browser: family / mobile / pointer", paste(one("fam"), one("mob"), one("ptr"), sep = " / "), NA)
  )
  bind <- function(rows, group) {
    stopifnot(all(lengths(rows) == 3))
    m <- do.call(rbind, rows)
    data.frame(group = group, indicator = m[, 1], value = m[, 2], flag = m[, 3], stringsAsFactors = FALSE)
  }
  out <- rbind(bind(environment, "environment"), bind(input, "input"), bind(attention, "attention"))
  out$flag <- !is.na(out$flag) & as.logical(out$flag)
  out
}

#' Indicators of automated or careless responding for every session of a results table
#'
#' Applies [dq_flags()] to each row of a results table: the `data_quality`
#' columns of the row are read as its page records, the `agent_probe` columns
#' as what was typed into the hidden fields, and the optional columns with
#' request headers are passed on when they exist.
#'
#' The defaults match the column names used in formr's example survey
#' (`dq_p1`, `dq_p2`, ... for the records, `dq_agent_p1`, ... for the hidden
#' fields). Use your own names or patterns if you named the items differently.
#'
#' @param results a data frame of survey results with one row per session, for
#'   example from [formr_results()].
#' @param pages a regular expression that matches the names of the
#'   `data_quality` columns.
#' @param probes a regular expression that matches the names of the
#'   `agent_probe` columns.
#' @param srv_ua,sig_agent,ch_platform,ch_mobile the names of the columns that
#'   hold the User-Agent header (a `browser` item), the `Signature-Agent`
#'   header and the two client hints (`server` items). A column that does not
#'   exist is ignored; `NULL` leaves it out.
#' @param session the name of the column that identifies a session. Row
#'   numbers are used when there is no such column.
#' @param flagged_only return only the indicators that are flagged (the
#'   default), or all of them.
#' @return A data frame with the columns `session`, `group`, `indicator`,
#'   `value` and `flag`, one row per session and indicator.
#' @seealso [dq_rrt()] for the randomised-response question.
#' @export
#' @examples
#' results <- data.frame(
#'   session = c("a", "b"),
#'   dq_p1 = c(
#'     '{"v":2,"ld":1,"keys":95,"txt_ch":90,"pd":5,"mv":160}',
#'     '{"v":2,"ld":1,"keys":0,"txt_ch":90,"fld_nokey":1,"pd":5,"pd_ctr":5}'
#'   ),
#'   dq_agent_p1 = c(NA, "yes, a language model")
#' )
#' dq_flags_by_session(results)
#' # how many indicators are flagged per session
#' table(dq_flags_by_session(results)$session)
dq_flags_by_session <- function(results, pages = "^dq_p[0-9]+$", probes = "^dq_agent_p[0-9]+$",
                                srv_ua = "dq_srv_ua", sig_agent = "dq_sig_agent",
                                ch_platform = "dq_ch_platform", ch_mobile = "dq_ch_mobile",
                                session = "session", flagged_only = TRUE) {
  results <- as.data.frame(results, stringsAsFactors = FALSE)
  page_cols <- grep(pages, names(results), value = TRUE)
  if (!length(page_cols)) {
    stop("No data_quality columns found: no column name matches `pages` (", pages, ").", call. = FALSE)
  }
  probe_cols <- grep(probes, names(results), value = TRUE)
  cell <- function(name, i) {
    if (is.null(name) || !name %in% names(results)) return(NA)
    as.character(results[[name]][i])
  }
  ids <- if (!is.null(session) && session %in% names(results)) {
    as.character(results[[session]])
  } else {
    as.character(seq_len(nrow(results)))
  }
  per_session <- lapply(seq_len(nrow(results)), function(i) {
    records <- vapply(page_cols, cell, character(1), i = i)
    typed <- vapply(probe_cols, cell, character(1), i = i)
    typed <- paste(typed[!is.na(typed) & nzchar(typed)], collapse = " | ")
    flags <- dq_flags(dq_parse(records), probe = typed, srv_ua = cell(srv_ua, i), sig_agent = cell(sig_agent, i),
                      ch_platform = cell(ch_platform, i), ch_mobile = cell(ch_mobile, i))
    if (flagged_only) flags <- flags[flags$flag, , drop = FALSE]
    if (!nrow(flags)) return(NULL)
    cbind(session = ids[i], flags, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, per_session)
  if (is.null(out)) {
    out <- data.frame(session = character(0), group = character(0), indicator = character(0),
                      value = character(0), flag = logical(0), stringsAsFactors = FALSE)
  }
  rownames(out) <- NULL
  out
}
