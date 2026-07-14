"""Command line interface."""

from __future__ import annotations

import asyncio
import json
import logging

import typer

from .config import load_settings

app = typer.Typer(help="Personal train punctuality forecasts (trainpunct).")

logging.basicConfig(level=logging.INFO, format="%(asctime)s %(name)s %(levelname)s %(message)s")


@app.command()
def serve(host: str = "0.0.0.0", port: int = 8300) -> None:
    """Run the API + PWA server."""
    import uvicorn

    from .api import create_app

    uvicorn.run(create_app(load_settings()), host=host, port=port)


@app.command()
def resolve(loop: bool = typer.Option(False, "--loop", help="Keep polling forever.")) -> None:
    """Resolve realized outcomes for logged predictions."""
    from .resolver import resolve_loop, resolve_once
    from .store import Store

    settings = load_settings()
    store = Store(settings.db_path)
    if loop:
        asyncio.run(resolve_loop(settings, store))
    else:
        n = asyncio.run(resolve_once(settings, store))
        typer.echo(f"resolved {n} outcomes")


@app.command()
def report(days: int = 90) -> None:
    """Print the calibration summary as JSON."""
    from . import calibration
    from .store import Store

    settings = load_settings()
    store = Store(settings.db_path)
    typer.echo(json.dumps(calibration.summarize(store.history(days=days)), indent=2))


@app.command()
def locations(query: str) -> None:
    """Look up stations/EVA numbers on transport.rest (verify config EVAs with this)."""
    from .transportrest import TransportRest

    settings = load_settings()

    async def run() -> list[dict]:
        client = TransportRest(settings.transportrest_url)
        try:
            return await client.locations(query)
        finally:
            await client.aclose()

    for loc in asyncio.run(run()):
        typer.echo(f"{loc.get('id')}  {loc.get('name')}")


PARQUET_ARG = typer.Argument(..., help="piebro monthly parquet file(s)")
OUT_OPT = typer.Option(None, help="Output path (default: cancel_table_path from config)")


@app.command("build-cancel-table")
def build_cancel_table(parquet: list[str] = PARQUET_ARG, out: str = OUT_OPT) -> None:
    """Build the empirical cancellation-rate lookup from piebro/deutsche-bahn-data parquet."""
    from .cancellations import build_table_from_piebro

    settings = load_settings()
    result = build_table_from_piebro(
        parquet_paths=parquet, out_path=out or settings.cancel_table_path
    )
    typer.echo(
        f"overall cancellation rate {result['overall_rate']:.4f} over {result['n_stops']} stops; "
        f"{len(result['rates'])} lookup keys -> {out or settings.cancel_table_path}"
    )


@app.command()
def healthcheck(url: str = "http://localhost:8300/api/health") -> None:
    """Exit non-zero when the API reports unhealthy (for docker healthchecks)."""
    import httpx

    r = httpx.get(url, timeout=10)
    typer.echo(r.text)
    raise typer.Exit(0 if r.status_code == 200 else 1)


if __name__ == "__main__":
    app()
