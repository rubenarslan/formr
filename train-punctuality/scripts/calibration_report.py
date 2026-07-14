#!/usr/bin/env python3
"""Standalone calibration report: prints the summary and writes a small HTML page.

Usage: python scripts/calibration_report.py [--days 90] [--out data/report.html]
"""

from __future__ import annotations

import argparse
import json

from trainpunct import calibration
from trainpunct.config import load_settings
from trainpunct.store import Store


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--days", type=int, default=90)
    parser.add_argument("--out", default=None)
    args = parser.parse_args()

    settings = load_settings()
    store = Store(settings.db_path)
    summary = calibration.summarize(store.history(days=args.days))
    print(json.dumps(summary, indent=2))

    if args.out:
        rows = []
        for bucket, entry in summary.get("buckets", {}).items():
            for q, cov in entry.get("coverage", {}).items():
                rows.append(
                    f"<tr><td>{bucket} min</td><td>{q}</td><td>{cov:.0%}</td>"
                    f"<td>{entry['pinball'].get(q, '')}</td><td>{entry['n']}</td></tr>"
                )
        db_bias = "".join(
            f"<p><b>{src}</b>: realized − forecast = {v['mean_underestimate_min']} min "
            f"on average; {v['p_worse_than_forecast']:.0%} of events worse than forecast "
            f"(n={v['n']})</p>"
            for src, v in summary.get("db_bias", {}).items()
        )
        html = f"""<!doctype html><meta charset="utf-8"><title>trainpunct calibration</title>
<h1>Calibration ({args.days} days, n={summary['n_resolved']})</h1>
{db_bias}
<table border="1" cellpadding="4">
<tr><th>horizon</th><th>quantile</th><th>coverage</th><th>pinball</th><th>n</th></tr>
{''.join(rows)}</table>
<p>Cancellations: {json.dumps(summary.get('cancellation', {}))}</p>"""
        with open(args.out, "w") as f:
            f.write(html)
        print(f"wrote {args.out}")


if __name__ == "__main__":
    main()
