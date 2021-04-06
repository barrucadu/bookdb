FROM python:3.9.4

COPY requirements-freeze.txt .
RUN pip install -r requirements-freeze.txt

RUN useradd -m app
COPY --chown=app src /app
COPY --chown=app config /app/config
WORKDIR /app
USER app

CMD gunicorn -w 4 -b 0.0.0.0:8888 serve:app
