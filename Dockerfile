FROM debian:testing-slim

RUN apt-get update \
 && apt-get install -y \
    libcurl4-gnutls-dev \
    libz-dev \
    haskell-stack \
 && rm -rf /var/lib/apt/lists/*

# Build app
COPY . /app
WORKDIR /app
RUN stack build

ENV LC_ALL=C.UTF-8
EXPOSE 3000
CMD ["stack", "run", "stund-gui"]
