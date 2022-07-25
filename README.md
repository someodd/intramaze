# Interwebz

Random chat encounters in a collaboratively-built maze.

A maze made out of rooms users have made and arbitrarily linked together. Explore the maze, if you bump into the other users in the same room you can have an ephemeral conversation.

I use Debian (unstable).

## How does it work

The website is a static website managed by daemon.

The daemon is a REST API for managing the static website as well as managing the websocket connections for the chatting. REST API authentication is managed with JWT.

### `static/`

The `static/mustache-build` are literal pages to build (like copying + running through parser), whereas `static/mustache` is simply for templates that get used to build pages.

## Running

You will need to generate the private key for JSON Web Tokens (regardless how you run):

```
openssl ecparam -name secp256k1 -genkey -noout -out jwt-priv-sig-key.pem
openssl ec -in jwt-priv-sig-key.pem -pubout > jwt-pub-sig-key.pem

```

### Running with Docker

Be sure to start by editing an env file like `.env.dev`:

```
POSTGRES_USER=testpguser
POSTGRES_PASSWORD=testpguser

SCOTTY_ENV=Test
SCOTTY_SITE_TITLE=MazeQuest
```

This command will build all the dependencies as an image, so dependencies don't have to be built/installed/downloaded every time:

```
docker build -f docker/Dockerfile-depends -t interwebz_depends .
```

This command will bring in new changes to the codebase and compile, then put up the PostgreSQL service, as well as the
API+static service (using nginx as reverse proxy):

```
docker compose -f docker/docker-compose.yml --env-file .env.dev up
```

Now you can visit the website using either:

  * http://localhost:8080/new-room.html
  * type in the docker ip and can use port 80 (check IP with `docker inspect idofwebcontainer`)

You'll want to keep an eye on the PostgreSQL database volume, as well as the built volume (which contains room images and the static site). Try `docker volume ls`. Read more about volumes, including backing up and restoring, on [the official Docker volumes documentation](https://docs.docker.com/storage/volumes/#back-up-a-volume).

### Running on host

Install the depends:

```
sudo apt install libjwt zlib1g-dev postgresql postgresql-contrib libpq-dev libjwt-dev
```

Setup PostgreSQL:

```
sudo -u postgres psql
CREATE USER testpguser with PASSWORD 'testpguser';
CREATE DATABASE testpgdatabase WITH OWNER=testpguser;
```

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
