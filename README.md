# IntraMaze

I use Debian (unstable).

## Running

Run the backend:

```shell
env SCOTTY_ENV=Test SCOTTY_SITE_TITLE=MazeQuest cabal run
```

The above will build the static files and run the REST API, which both manages the database and handles updating the static files.

Serve the built static files directory (here's a way to test):

```
cd built
python3 -m http.server
```

Now you can visit http://localhost:8000/.

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


## `static/`

The `static/mustache-build` are literal pages to build (like copying + running through parser), whereas `static/mustache` is simply for templates that get used to build pages.