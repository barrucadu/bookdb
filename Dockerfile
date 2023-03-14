FROM python:3.11.2 AS base
RUN useradd -m app
WORKDIR /app

FROM base AS poetry
ENV POETRY_HOME=/opt/poetry
ENV POETRY_VIRTUALENVS_IN_PROJECT=true
ENV PATH="$POETRY_HOME/bin:$PATH"
ENV POETRY_NO_INTERACTION=1
RUN curl -sSL https://install.python-poetry.org | python -
COPY poetry.lock pyproject.toml ./
RUN poetry install

FROM base AS app
ENV PATH="/app/.venv/bin:$PATH"
ENV ELASTIC_CLIENT_APIVERSIONING=1
COPY --chown=app bookdb /app/bookdb
COPY --chown=app config /app/config
COPY --from=poetry /app/.venv /app/.venv
RUN mkdir /covers && chown -R app /covers
USER app
CMD ["gunicorn", "-w", "4", "-b", "0.0.0.0:8888", "bookdb.serve:app"]
