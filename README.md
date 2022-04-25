# PinkSands

I use Debian (unstable).

## Running

Run the backend:

```shell
env SCOTTY_ENV=Test cabal run
```

And serve the built static files directory (here's a way to test):

```
cd built
python3 -m http.server
```

## Setup/Dependencies

```
sudo apt install libjwt zlib1g-dev postgresql postgresql-contrib libpq-dev libjwt-dev
```

### Setting up PostgreSQL

```
sudo -u postgres psql
CREATE USER testpguser with PASSWORD 'testpguser';
CREATE DATABASE testpgdatabase WITH OWNER=testpguser;
```
