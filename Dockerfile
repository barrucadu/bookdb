FROM haskell:8.6.5

RUN apt-get update && apt-get install -y libpq-dev && rm -rf /var/lib/apt/lists/*

RUN useradd -m bookdb
COPY --chown=bookdb . /bookdb
WORKDIR /bookdb
USER bookdb

RUN stack build --copy-bins && rm -rf .stack-work && rm -rf ~/.stack

CMD ["/home/bookdb/.local/bin/bookdb", "run"]
