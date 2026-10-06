library(testthat)

# A page record as formr's data_quality item stores it: every field, nothing
# counted, an ordinary desktop Chrome. `...` overrides fields.
dq_record <- function(...) {
  base <- list(
    v = 2, ld = 1,
    wd = 0, wdt = 0, hl = 0, nochr = 0, plg0 = 0, lang0 = 0, ow0 = 0, scrvp = 0, swgl = 0, perm = 0,
    glob = "", ext = "", ext_res = 0, aib = "", fam = "chrome", mob = 0, ptr = "fine", fx0 = 1, vis0 = "visible",
    uac = "", uah = "", a11y = 0, winscr = 0, eng = "blink", eng_ua = "blink", eng_mm = 0, ver_mm = 0,
    drm = "wv", chr_nowv = 0, t = 30000, t1 = 900,
    hid_n = 0, hid_ms = 0, blur_n = 0, blur_ms = 0,
    copy = 0, copy_q = 0, copy_ch = 0, cut = 0, paste = 0, paste_ch = 0, ctx = 0,
    keys = 0, kn = 0, kfast = 0, kdwf = 0, kcv = -1, kwb = -1, bksp = 0, kcap = 0, kcap_ns = 0, kcap_nosk = 0,
    kcode0 = 0, txt_ch = 0, fld_nokey = 0, ins_na = 0, kwc = 0, kws = 0, klp = 0, klp_ws = 0,
    ins_nokey = 0, ins_nokey_ch = 0, ins_repl = 0, ins_other = 0,
    mv = 0, mv_jump = 0, mv_coal = 0, mv_mz = 0, pd = 0, pd_fewmv = 0, pd_ctr = 0, pd_scr = 0, pu = 0,
    pd_hold0 = 0, pd_newview = 0, tap = 0, tap_r1 = 0,
    scr = 0, scr_nogest = 0, foc_nogest = 0, foc_prog = 0, clk_nopt = 0, rsz = 0, rsz_side = 0, rsz_bar = 0,
    untr = 0, prog = 0, act_hid = 0, act_unf = 0, dom_top = 0
  )
  as.character(jsonlite::toJSON(utils::modifyList(base, list(...)), auto_unbox = TRUE))
}
# A page a person answered: an approach before every click, a key for every character.
dq_person <- function(...) {
  typical <- list(keys = 96, kn = 80, kcv = 0.42, bksp = 4, kcap = 3, txt_ch = 90, kwc = 78, kws = 15, klp = 3,
                  klp_ws = 3, mv = 180, pd = 5, pu = 5, scr = 6)
  do.call(dq_record, utils::modifyList(typical, list(...)))
}
# The same page answered by a script: clicks at the exact centre, text without keys.
dq_script <- function(...) {
  typical <- list(txt_ch = 90, fld_nokey = 1, ins_nokey = 1, ins_nokey_ch = 90, mv = 5, pd = 5, pu = 5, pd_ctr = 5,
                  pd_fewmv = 5, pd_hold0 = 5)
  do.call(dq_record, utils::modifyList(typical, list(...)))
}
flagged <- function(f) f$indicator[f$flag]
UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/141.0.0.0 Safari/537.36"

test_that("dq_parse reads one row per record and drops what is no record", {
  d <- dq_parse(c(dq_record(keys = 12), NA, "", "not json", dq_record(keys = 40)))
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 2)
  expect_equal(d$keys, c(12, 40))
  expect_equal(d$v, c(2, 2))
  expect_equal(d$fam, c("chrome", "chrome"))

  expect_equal(nrow(dq_parse(c(NA, ""))), 0)
  expect_equal(nrow(dq_parse(character(0))), 0)
  expect_equal(nrow(dq_parse("[1,2,3]")), 0)
})

test_that("dq_parse fills the fields a record does not have", {
  d <- dq_parse(c('{"v":1,"keys":3}', '{"v":2,"keys":5,"pd":2}'))
  expect_equal(nrow(d), 2)
  expect_true(is.na(d$pd[1]))
  expect_equal(d$pd[2], 2)
})

test_that("a person's pages raise no input flag", {
  f <- dq_flags(dq_parse(c(dq_person(), dq_person(t = 12000))))
  expect_named(f, c("group", "indicator", "value", "flag"))
  expect_type(f$flag, "logical")
  expect_false(anyNA(f$flag))
  expect_setequal(unique(f$group), c("environment", "input", "attention"))
  expect_length(flagged(f), 0)
  # the counts are summed over the pages
  expect_equal(f$value[f$indicator == "seconds on pages"], "42")
})

test_that("a script's pages are flagged by how the input was made", {
  f <- dq_flags(dq_parse(c(dq_script(), dq_script())))
  expect_true(all(f$group[f$flag] == "input"))
  expect_true("mouse presses / at exact centre" %in% flagged(f))
  expect_true("mouse presses with <= 2 moves in prior second" %in% flagged(f))
  expect_true("text answers mostly not typed or pasted (fields)" %in% flagged(f))
  expect_true("text inserted without key presses (events / chars / other inserts)" %in% flagged(f))
  # a trackpad tap is as short as a script's click: shown, never flagged
  expect_false(any(grepl("released within 25 ms", flagged(f))))
})

test_that("thresholds need enough to go on", {
  # two centre clicks are not a pattern, three are
  expect_false("mouse presses / at exact centre" %in% flagged(dq_flags(dq_parse(dq_record(pd = 2, pd_ctr = 2)))))
  expect_true("mouse presses / at exact centre" %in% flagged(dq_flags(dq_parse(dq_record(pd = 3, pd_ctr = 3)))))
  # one focus move without a click is tolerated
  expect_false("focus moved without click/key" %in% flagged(dq_flags(dq_parse(dq_record(foc_nogest = 1)))))
  expect_true("focus moved without click/key" %in% flagged(dq_flags(dq_parse(dq_record(foc_nogest = 2)))))
  # typing like a metronome
  expect_true(any(grepl("typing rhythm", flagged(dq_flags(dq_parse(dq_person(kcv = 0.02)))))))
  # capitals without Shift
  expect_true(any(grepl("capitals", flagged(dq_flags(dq_parse(dq_person(kcap_ns = 2)))))))
})

test_that("leaving the page and pasting are attention indicators", {
  f <- dq_flags(dq_parse(dq_person(hid_n = 3, hid_ms = 40000, paste = 1, paste_ch = 60)))
  expect_true(all(f$group[f$flag] == "attention"))
  expect_equal(length(flagged(f)), 2)
})

test_that("the browser's own report is flagged in the environment group", {
  f <- dq_flags(dq_parse(dq_person(wd = 1, eng = "blink", eng_ua = "gecko", eng_mm = 1, chr_nowv = 1, glob = "__playwright__binding__")))
  expect_true(all(f$group[f$flag] == "environment"))
  expect_true("navigator.webdriver true" %in% flagged(f))
  expect_true("automation globals" %in% flagged(f))
  expect_true(any(grepl("real JS engine differs", flagged(f))))
  expect_true(any(grepl("no Widevine", flagged(f))))
})

test_that("any text in the hidden field is flagged", {
  expect_true("agent_probe field filled" %in% flagged(dq_flags(dq_parse(dq_person()), probe = "no")))
  expect_false("agent_probe field filled" %in% flagged(dq_flags(dq_parse(dq_person()), probe = "")))
  expect_false("agent_probe field filled" %in% flagged(dq_flags(dq_parse(dq_person()), probe = NA)))
})

test_that("the User-Agent header is compared with the one the page read", {
  same <- dq_parse(dq_person(uah = dq_adler32(UA)))
  header <- "User-Agent header differs from navigator.userAgent"
  expect_false(header %in% flagged(dq_flags(same, srv_ua = UA)))
  expect_true(header %in% flagged(dq_flags(same, srv_ua = sub("Windows NT 10.0", "Windows NT 6.1", UA))))
  # without the header there is nothing to compare
  expect_false(header %in% flagged(dq_flags(same)))
  expect_true(is.na(dq_flags(same)$value[dq_flags(same)$indicator == header]))
})

test_that("the checksum is the one the page computes", {
  expect_equal(dq_adler32("Wikipedia"), "11e60398")
  expect_equal(dq_adler32(""), "1")
  expect_true(is.na(dq_adler32(NA)))
})

test_that("a signed agent and contradicting client hints are flagged", {
  d <- dq_parse(dq_person(uah = dq_adler32(UA)))
  expect_true(any(grepl("signed by an agent", flagged(dq_flags(d, sig_agent = '"https://chatgpt.com"')))))
  expect_false(any(grepl("signed by an agent", flagged(dq_flags(d, sig_agent = "")))))

  system <- "client hints name another system than the User-Agent header (header / hint)"
  expect_true(system %in% flagged(dq_flags(d, srv_ua = UA, ch_platform = '"Linux"')))
  expect_false(system %in% flagged(dq_flags(d, srv_ua = UA, ch_platform = '"Windows"')))
  # Android calls itself Linux in places
  android <- "Mozilla/5.0 (Linux; Android 14; Pixel 8) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/141.0.0.0 Mobile Safari/537.36"
  expect_false(system %in% flagged(dq_flags(d, srv_ua = android, ch_platform = '"Linux"')))
  # Firefox and Safari send no hints
  expect_false(system %in% flagged(dq_flags(d, srv_ua = UA, ch_platform = "")))

  mobile <- "client hints and User-Agent header disagree about mobile (hint)"
  expect_true(mobile %in% flagged(dq_flags(d, srv_ua = UA, ch_mobile = "?1")))
  expect_false(mobile %in% flagged(dq_flags(d, srv_ua = UA, ch_mobile = "?0")))
  expect_false(mobile %in% flagged(dq_flags(d, srv_ua = android, ch_mobile = "?1")))
})

test_that("no record gives one unflagged row", {
  f <- dq_flags(dq_parse(NA))
  expect_equal(nrow(f), 1)
  expect_equal(f$indicator, "no data quality record")
  expect_false(f$flag)
})

test_that("records without some fields are read without warnings", {
  expect_silent(f <- dq_flags(dq_parse('{"v":1,"keys":3,"pd":4,"pd_ctr":4}')))
  expect_false(anyNA(f$flag))
  expect_true("mouse presses / at exact centre" %in% flagged(f))
})

test_that("dq_flags_by_session reads a results table", {
  results <- data.frame(
    session = c("person", "script", "nothing"),
    dq_p1 = c(dq_person(uah = dq_adler32(UA)), dq_script(), NA),
    dq_p2 = c(dq_person(), dq_script(), NA),
    dq_agent_p1 = c(NA, "yes, a language model", NA),
    dq_agent_p2 = c(NA, "", NA),
    dq_srv_ua = c(UA, NA, NA),
    dq_sig_agent = c("", '"https://chatgpt.com"', NA),
    other_item = 1:3,
    stringsAsFactors = FALSE
  )
  f <- dq_flags_by_session(results)
  expect_named(f, c("session", "group", "indicator", "value", "flag"))
  expect_true(all(f$flag))
  expect_equal(unique(f$session), "script")
  expect_true("agent_probe field filled" %in% f$indicator)
  expect_equal(f$value[f$indicator == "agent_probe field filled"], "yes, a language model")
  expect_true(any(grepl("signed by an agent", f$indicator)))

  all_rows <- dq_flags_by_session(results, flagged_only = FALSE)
  expect_setequal(unique(all_rows$session), c("person", "script", "nothing"))
  expect_equal(all_rows$indicator[all_rows$session == "nothing"], "no data quality record")
  expect_equal(sum(all_rows$session == "person"), sum(all_rows$session == "script"))
})

test_that("dq_flags_by_session takes other column names and does without the optional ones", {
  results <- data.frame(code = c("x", "y"), quality_1 = c(dq_script(), dq_person()), hidden_1 = c("I am an AI", NA),
                        stringsAsFactors = FALSE)
  expect_error(dq_flags_by_session(results), "No data_quality columns")
  f <- dq_flags_by_session(results, pages = "^quality_", probes = "^hidden_", session = "code")
  expect_equal(unique(f$session), "x")
  # no session column: row numbers
  expect_equal(unique(dq_flags_by_session(results, pages = "^quality_", probes = "^hidden_")$session), "1")
  # nobody flagged: an empty table with the same columns
  none <- dq_flags_by_session(results[2, ], pages = "^quality_", probes = "^hidden_", session = "code")
  expect_equal(nrow(none), 0)
  expect_named(none, c("session", "group", "indicator", "value", "flag"))
})

test_that("dq_rrt estimates the share behind the harmless statement", {
  est <- dq_rrt(c(rep(TRUE, 12), rep(FALSE, 288)))
  expect_equal(est[["n"]], 300)
  expect_equal(est[["yes_rate"]], 0.04)
  expect_equal(est[["ai_share"]], (0.04 - 0.008) / (1 - 0.008))
  expect_lt(est[["lower"]], est[["ai_share"]])
  expect_gt(est[["upper"]], est[["ai_share"]])
  # 0/1 input, missing answers dropped, another rate
  est2 <- dq_rrt(c(1, 0, 0, NA, 1, 0, 0, 0, 0, 0, 0), p = 0.1)
  expect_equal(est2[["n"]], 10)
  expect_equal(est2[["ai_share"]], (0.2 - 0.1) / 0.9)
  # only twins: no share left
  expect_equal(dq_rrt(c(rep(TRUE, 8), rep(FALSE, 992)))[["ai_share"]], 0)
})
